### stats-stud: outlier handling analysis
### Purpose: Demonstrate three outlier-handling strategies and export results to CSV.
###          This is a companion to notes/outlier_guide.md.
###
### Outputs (all written to outputs/tables/):
###   public_outlier_detail.csv         — row-level flags for IQR and modified Z-score
###   public_outlier_winsorized.csv     — winsorized scores vs original (Option 2)
###   public_outlier_winsorize_compare.csv — descriptive stats: original vs winsorized
###   public_outlier_sensitivity.csv    — ANOVA + KW results: full vs outliers-excluded (Option 3)

stats_stud_context <- "public"
source("scripts/00_setup.R")

df <- utils::read.csv(paths$public_clean, stringsAsFactors = FALSE)
df$race       <- trimws(tolower(df$race_ethnicity))
df$math_score <- as.numeric(df$math_score)
df$reading_score <- as.numeric(df$reading_score)
df$writing_score <- as.numeric(df$writing_score)

score_cols <- c("math_score", "reading_score", "writing_score")

message("\n", paste(rep("=", 60), collapse = ""))
message("OUTLIER ANALYSIS — notes/outlier_guide.md companion")
message(paste(rep("=", 60), collapse = ""))

# ============================================================================
# OPTION 1 — Detailed row-level outlier flagging (IQR + modified Z-score)
# ============================================================================

message("\n[Option 1] Building row-level outlier flag table...")

flag_outliers_detail <- function(x, var_name) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower_15  <- q1 - 1.5 * iqr   # Standard IQR fence
  upper_15  <- q3 + 1.5 * iqr
  lower_3   <- q1 - 3   * iqr   # Extreme outlier fence
  upper_3   <- q3 + 3   * iqr

  med <- median(x, na.rm = TRUE)
  mad <- median(abs(x - med), na.rm = TRUE)
  # Avoid division-by-zero if MAD = 0
  mod_z <- if (mad > 0) 0.6745 * (x - med) / mad else rep(0, length(x))

  data.frame(
    row_id          = seq_along(x),
    variable        = var_name,
    value           = x,
    iqr_lower_fence = round(lower_15, 2),
    iqr_upper_fence = round(upper_15, 2),
    iqr_flag        = x < lower_15 | x > upper_15,
    extreme_flag    = x < lower_3  | x > upper_3,
    modified_z      = round(mod_z, 4),
    modz_flag       = abs(mod_z) > 3.5,
    stringsAsFactors = FALSE
  )
}

detail_list <- lapply(score_cols, function(sc) {
  flag_outliers_detail(df[[sc]], sc)
})
detail_df <- dplyr::bind_rows(detail_list)

# Keep only flagged rows for the CSV (much more readable than 3,000 rows)
flagged_df <- detail_df[detail_df$iqr_flag | detail_df$modz_flag, ]
flagged_df <- flagged_df[order(flagged_df$variable, flagged_df$value), ]

save_table_csv(flagged_df, file.path(paths$tables, "public_outlier_detail.csv"))
message(sprintf("  -> Flagged %d observations across all score variables.", nrow(flagged_df)))
message("  -> Saved: outputs/tables/public_outlier_detail.csv")
print(flagged_df[, c("variable", "row_id", "value", "iqr_flag", "extreme_flag", "modz_flag")])

# ============================================================================
# OPTION 2 — Winsorize (replace values at fence, do not delete)
# ============================================================================

message("\n[Option 2] Winsorizing scores at 1.5×IQR fences...")

winsorize <- function(x, lower, upper) {
  pmax(pmin(x, upper), lower)
}

# Get fences from detect_outliers (already defined in 00_setup.R)
get_iqr_fences <- function(x) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  list(lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

fences <- lapply(score_cols, function(sc) get_iqr_fences(df[[sc]]))
names(fences) <- score_cols

df$math_score_wins    <- winsorize(df$math_score,    fences$math_score$lower,    fences$math_score$upper)
df$reading_score_wins <- winsorize(df$reading_score, fences$reading_score$lower, fences$reading_score$upper)
df$writing_score_wins <- winsorize(df$writing_score, fences$writing_score$lower, fences$writing_score$upper)

# Row-level comparison: what changed?
wins_compare_rows <- data.frame(
  row_id               = seq_len(nrow(df)),
  math_original        = df$math_score,
  math_winsorized      = df$math_score_wins,
  math_changed         = df$math_score != df$math_score_wins,
  reading_original     = df$reading_score,
  reading_winsorized   = df$reading_score_wins,
  reading_changed      = df$reading_score != df$reading_score_wins,
  writing_original     = df$writing_score,
  writing_winsorized   = df$writing_score_wins,
  writing_changed      = df$writing_score != df$writing_score_wins,
  stringsAsFactors = FALSE
)
# Only save rows where at least one score was winsorized
wins_changed <- wins_compare_rows[
  wins_compare_rows$math_changed |
    wins_compare_rows$reading_changed |
    wins_compare_rows$writing_changed, ]

save_table_csv(wins_changed, file.path(paths$tables, "public_outlier_winsorized.csv"))
message(sprintf("  -> %d rows had at least one score winsorized.", nrow(wins_changed)))
message("  -> Saved: outputs/tables/public_outlier_winsorized.csv")

# Descriptive comparison table: original vs winsorized
desc_stat <- function(x, label) {
  data.frame(
    series   = label,
    n        = sum(!is.na(x)),
    mean     = round(mean(x, na.rm = TRUE), 4),
    sd       = round(sd(x, na.rm = TRUE), 4),
    median   = round(median(x, na.rm = TRUE), 4),
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

wins_desc <- dplyr::bind_rows(
  desc_stat(df$math_score,        "math_original"),
  desc_stat(df$math_score_wins,   "math_winsorized"),
  desc_stat(df$reading_score,     "reading_original"),
  desc_stat(df$reading_score_wins,"reading_winsorized"),
  desc_stat(df$writing_score,     "writing_original"),
  desc_stat(df$writing_score_wins,"writing_winsorized")
)

save_table_csv(wins_desc, file.path(paths$tables, "public_outlier_winsorize_compare.csv"))
message("  -> Saved: outputs/tables/public_outlier_winsorize_compare.csv")
message("  Descriptive comparison (original vs winsorized):")
print(wins_desc)

# ============================================================================
# OPTION 3 — Sensitivity analysis (run ANOVA + KW twice, compare results)
# ============================================================================

message("\n[Option 3] Sensitivity analysis: full dataset vs outliers excluded...")

# Exclude observations below IQR lower fence for math score
math_lower_fence <- fences$math_score$lower
df_no_out <- df[df$math_score >= math_lower_fence, ]

message(sprintf("  Full dataset:      n = %d", nrow(df)))
message(sprintf("  Outliers excluded: n = %d (removed %d rows where math_score < %.2f)",
                nrow(df_no_out), nrow(df) - nrow(df_no_out), math_lower_fence))

# Helper: run ANOVA and extract key stats
run_anova <- function(data, label) {
  model  <- aov(math_score ~ race, data = data)
  smry   <- summary(model)[[1]]
  k      <- length(unique(data$race))
  n      <- nrow(data)

  # Effect size: eta-squared
  ss_between <- smry$"Sum Sq"[1]
  ss_total   <- sum((data$math_score - mean(data$math_score, na.rm = TRUE))^2, na.rm = TRUE)
  eta_sq     <- ss_between / ss_total

  data.frame(
    analysis    = label,
    test        = "One-Way ANOVA",
    n           = n,
    df_between  = smry$Df[1],
    df_within   = smry$Df[2],
    F_statistic = round(smry$"F value"[1], 4),
    p_value     = round(smry$"Pr(>F)"[1], 6),
    significant = smry$"Pr(>F)"[1] < 0.05,
    eta_squared = round(eta_sq, 4),
    stringsAsFactors = FALSE
  )
}

# Helper: run Kruskal-Wallis and extract key stats
run_kw <- function(data, label) {
  kw   <- kruskal.test(math_score ~ race, data = data)
  k    <- length(unique(data$race))
  n    <- nrow(data)
  H    <- as.numeric(kw$statistic)
  # Epsilon-squared: (H - k + 1) / (n - k)
  eps_sq <- (H - k + 1) / (n - k)

  data.frame(
    analysis        = label,
    test            = "Kruskal-Wallis",
    n               = n,
    df              = as.integer(kw$parameter),
    H_statistic     = round(H, 4),
    p_value         = round(kw$p.value, 6),
    significant     = kw$p.value < 0.05,
    epsilon_squared = round(eps_sq, 4),
    stringsAsFactors = FALSE
  )
}

# Run both tests on both datasets
anova_full   <- run_anova(df,        "Full dataset (n=1000)")
anova_no_out <- run_anova(df_no_out, paste0("Outliers excluded (n=", nrow(df_no_out), ")"))

kw_full      <- run_kw(df,        "Full dataset (n=1000)")
kw_no_out    <- run_kw(df_no_out, paste0("Outliers excluded (n=", nrow(df_no_out), ")"))

# Combine: common columns for ANOVA rows (F-based) vs KW rows (H-based)
sensitivity_results <- dplyr::bind_rows(
  # ANOVA rows — fill NA for KW-only columns
  dplyr::mutate(anova_full,
    H_statistic     = NA_real_,
    epsilon_squared = NA_real_),
  dplyr::mutate(anova_no_out,
    H_statistic     = NA_real_,
    epsilon_squared = NA_real_),
  # KW rows — fill NA for ANOVA-only columns
  dplyr::mutate(kw_full,
    df_between  = NA_integer_,
    df_within   = NA_integer_,
    F_statistic = NA_real_,
    eta_squared = NA_real_),
  dplyr::mutate(kw_no_out,
    df_between  = NA_integer_,
    df_within   = NA_integer_,
    F_statistic = NA_real_,
    eta_squared = NA_real_)
)

# Reorder columns for readability
sensitivity_results <- sensitivity_results[, c(
  "analysis", "test", "n",
  "df_between", "df_within", "df",
  "F_statistic", "H_statistic",
  "p_value", "significant",
  "eta_squared", "epsilon_squared"
)]

save_table_csv(sensitivity_results, file.path(paths$tables, "public_outlier_sensitivity.csv"))
message("  -> Saved: outputs/tables/public_outlier_sensitivity.csv")
message("  Sensitivity results:")
print(sensitivity_results[, c("analysis", "test", "n", "F_statistic", "H_statistic",
                               "p_value", "significant", "eta_squared", "epsilon_squared")])

# ============================================================================
# VERDICT SUMMARY
# ============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("VERDICT: KEEP OUTLIERS (Option 1 — see notes/outlier_guide.md)")
message(paste(rep("=", 60), collapse = ""))

# Pull key numbers for the verdict printout
n_iqr_math  <- sum(flagged_df$variable == "math_score" & flagged_df$iqr_flag, na.rm = TRUE)
n_ext_math  <- sum(flagged_df$variable == "math_score" & flagged_df$extreme_flag, na.rm = TRUE)
n_modz_math <- sum(flagged_df$variable == "math_score" & flagged_df$modz_flag, na.rm = TRUE)

anova_p_full   <- sensitivity_results$p_value[sensitivity_results$test == "One-Way ANOVA" &
                                                 grepl("Full", sensitivity_results$analysis)]
anova_p_no_out <- sensitivity_results$p_value[sensitivity_results$test == "One-Way ANOVA" &
                                                 grepl("excluded", sensitivity_results$analysis)]
kw_p_full      <- sensitivity_results$p_value[sensitivity_results$test == "Kruskal-Wallis" &
                                                 grepl("Full", sensitivity_results$analysis)]
kw_p_no_out    <- sensitivity_results$p_value[sensitivity_results$test == "Kruskal-Wallis" &
                                                 grepl("excluded", sensitivity_results$analysis)]

message(sprintf("  math_score: %d IQR outliers, %d extreme (3xIQR), %d modified-Z > 3.5",
                n_iqr_math, n_ext_math, n_modz_math))
message(sprintf("  ANOVA p-value:         full = %.6f | no-outliers = %.6f", anova_p_full, anova_p_no_out))
message(sprintf("  Kruskal-Wallis p-value: full = %.6f | no-outliers = %.6f", kw_p_full, kw_p_no_out))

p_change_anova <- abs(anova_p_full - anova_p_no_out)
p_change_kw    <- abs(kw_p_full - kw_p_no_out)

if (p_change_anova < 0.01 && p_change_kw < 0.01) {
  message("  FINDING: Results are ROBUST — p-values shift by < 0.01 with/without outliers.")
  message("  CONCLUSION: Outliers do NOT materially affect findings. Safe to retain.")
} else {
  message("  FINDING: p-values shift noticeably. Inspect sensitivity output carefully.")
}

message("\nAll outputs saved to outputs/tables/. Refer to notes/outlier_guide.md for interpretation.")
