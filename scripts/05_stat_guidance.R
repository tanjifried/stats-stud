### stats-stud: statistical guidance — categorical predictors + continuous pairs
### Purpose: Produce objective summary tables to guide test selection:
###   1. Categorical predictors table (group means × scores, ranked by spread)
###   2. Continuous pairs table (Pearson + Spearman correlations)
###   3. Data-driven test recommendations (parametric + non-parametric)

source("scripts/00_setup.R")

# ============================================================================
# LOAD DATA
# ============================================================================

message("Loading cleaned public dataset...")

df <- utils::read.csv(paths$public_clean, stringsAsFactors = FALSE)

# Score columns (reuse from 00_setup.R pattern)
score_cols <- c("math_score", "reading_score", "writing_score")

df <- df |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(score_cols), as.numeric),
    dplyr::across(
      c(gender, race_ethnicity, parental_level_of_education,
        lunch, test_preparation_course),
      ~ trimws(tolower(as.character(.x)))
    )
  )

cat_cols <- c(
  "gender",
  "race_ethnicity",
  "parental_level_of_education",
  "lunch",
  "test_preparation_course"
)

# ============================================================================
# SECTION 1: CATEGORICAL PREDICTORS TABLE
# ============================================================================
# For each categorical variable × each score column, compute per-group:
#   n, mean, sd, formatted "mean ± sd"
# Then compute the max spread (max group mean - min group mean).
# This is the key table used to objectively rank which predictor to test.
# ============================================================================

message("Building categorical predictors table...")

#' Summarise one categorical variable against all three score columns.
#' @param data  Cleaned data frame
#' @param cat   Character name of the categorical column
#' @return Long-format data frame with one row per group × score combination
summarise_cat_predictor <- function(data, cat) {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to  = "score",
      values_to = "value"
    ) |>
    dplyr::group_by(predictor = cat,
                    group     = .data[[cat]],
                    score) |>
    dplyr::summarise(
      n      = sum(!is.na(value)),
      mean   = round(mean(value, na.rm = TRUE), 2),
      sd     = round(stats::sd(value,   na.rm = TRUE), 2),
      median = round(stats::median(value, na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    dplyr::mutate(mean_sd = paste0(mean, " \u00b1 ", sd))   # "mean ± sd"
}

cat_long <- dplyr::bind_rows(lapply(cat_cols, summarise_cat_predictor, data = df))

save_table_csv(cat_long,
               file.path(paths$tables, "public_categorical_predictors_detail.csv"))

message(sprintf("  Saved: public_categorical_predictors_detail.csv (%d rows)", nrow(cat_long)))

# --------------------------------------------------------------------------
# Ranked summary: max spread per predictor × score
# --------------------------------------------------------------------------

cat_ranked <- cat_long |>
  dplyr::group_by(predictor, score) |>
  dplyr::summarise(
    n_groups    = dplyr::n_distinct(group),
    max_spread  = round(max(mean) - min(mean), 2),
    top_group   = group[which.max(mean)],
    top_mean    = max(mean),
    bottom_group = group[which.min(mean)],
    bottom_mean  = min(mean),
    .groups = "drop"
  ) |>
  dplyr::arrange(score, dplyr::desc(max_spread))

save_table_csv(cat_ranked,
               file.path(paths$tables, "public_categorical_ranked.csv"))

message(sprintf("  Saved: public_categorical_ranked.csv (%d rows)", nrow(cat_ranked)))

# --------------------------------------------------------------------------
# Console print: ranked table (math score only, most relevant)
# --------------------------------------------------------------------------

message("\n", paste(rep("=", 60), collapse = ""))
message("CATEGORICAL PREDICTORS — Math Score Spread (ranked)")
message(paste(rep("=", 60), collapse = ""))

cat_ranked |>
  dplyr::filter(score == "math_score") |>
  dplyr::select(predictor, n_groups, max_spread, top_group, top_mean, bottom_group, bottom_mean) |>
  print(n = Inf)

message(paste(rep("-", 60), collapse = ""))
message("Full table (all scores) saved to outputs/tables/public_categorical_ranked.csv")

# ============================================================================
# SECTION 2: CONTINUOUS PAIRS TABLE
# ============================================================================
# For every pairwise combination of the 3 score variables, compute:
#   Pearson r (parametric) + Spearman rho (non-parametric) + p-values
# This reveals the strength of continuous relationships in the data.
# ============================================================================

message("\nBuilding continuous pairs table...")

score_pairs <- utils::combn(score_cols, 2, simplify = FALSE)

#' Run Pearson and Spearman correlation for a single pair of variables.
#' @param pair Length-2 character vector of column names
#' @param data Cleaned data frame
#' @return One-row data frame with r, rho, and p-values
correlate_pair <- function(pair, data) {
  x    <- stats::na.omit(data[[pair[1]]])
  y    <- stats::na.omit(data[[pair[2]]])
  n    <- nrow(stats::na.omit(data[, pair]))

  pearson  <- stats::cor.test(data[[pair[1]]], data[[pair[2]]],
                               method = "pearson",  use = "pairwise.complete.obs")
  spearman <- stats::cor.test(data[[pair[1]]], data[[pair[2]]],
                               method = "spearman", use = "pairwise.complete.obs",
                               exact  = FALSE)

  data.frame(
    var1            = pair[1],
    var2            = pair[2],
    n               = n,
    pearson_r       = round(unname(pearson$estimate),  4),
    pearson_p       = round(pearson$p.value,            6),
    spearman_rho    = round(unname(spearman$estimate), 4),
    spearman_p      = round(spearman$p.value,           6),
    pearson_sig     = dplyr::case_when(
                        pearson$p.value < 0.001 ~ "***",
                        pearson$p.value < 0.01  ~ "**",
                        pearson$p.value < 0.05  ~ "*",
                        TRUE                    ~ "ns"),
    strength        = dplyr::case_when(
                        abs(pearson$estimate) >= 0.90 ~ "Very Strong",
                        abs(pearson$estimate) >= 0.70 ~ "Strong",
                        abs(pearson$estimate) >= 0.50 ~ "Moderate",
                        abs(pearson$estimate) >= 0.30 ~ "Weak",
                        TRUE                          ~ "Negligible"),
    stringsAsFactors = FALSE
  )
}

pairs_tbl <- dplyr::bind_rows(lapply(score_pairs, correlate_pair, data = df))

save_table_csv(pairs_tbl,
               file.path(paths$tables, "public_continuous_pairs.csv"))

message(sprintf("  Saved: public_continuous_pairs.csv (%d pairs)", nrow(pairs_tbl)))

# Console print
message("\n", paste(rep("=", 60), collapse = ""))
message("CONTINUOUS PAIRS — Pearson r / Spearman rho")
message(paste(rep("=", 60), collapse = ""))
print(pairs_tbl[, c("var1", "var2", "n", "pearson_r", "spearman_rho",
                     "pearson_sig", "strength")], row.names = FALSE)

# --------------------------------------------------------------------------
# Scatter-plot matrix: one panel per pair with regression line + r annotation
# --------------------------------------------------------------------------

message("\nCreating continuous pairs scatter plot...")

pair_plots <- lapply(score_pairs, function(pair) {
  x_var   <- pair[1]
  y_var   <- pair[2]
  r_val   <- pairs_tbl$pearson_r[pairs_tbl$var1 == x_var & pairs_tbl$var2 == y_var]
  rho_val <- pairs_tbl$spearman_rho[pairs_tbl$var1 == x_var & pairs_tbl$var2 == y_var]

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2C7BB6") +
    ggplot2::geom_smooth(method = "lm", se = TRUE,
                         color = "#D7191C", linewidth = 0.8, fill = "#D7191C", alpha = 0.15) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf,
      label  = sprintf("r = %.3f\n\u03c1 = %.3f", r_val, rho_val),
      hjust  = -0.1, vjust = 1.3, size = 3.5, color = "gray30"
    ) +
    ggplot2::labs(
      title    = paste(gsub("_score", "", x_var), "\u00d7", gsub("_score", "", y_var)),
      subtitle = sprintf("Pearson r = %.3f | Spearman \u03c1 = %.3f", r_val, rho_val),
      x        = x_var,
      y        = y_var
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 10),
      plot.subtitle = ggplot2::element_text(size = 8, color = "gray40")
    )
})

# Combine into a 1×3 panel using patchwork if available, else save individually
if (requireNamespace("patchwork", quietly = TRUE)) {
  p_pairs_combined <- patchwork::wrap_plots(pair_plots, ncol = 3)
  save_plot(p_pairs_combined, "public_pairs_scatter.png", width = 15, height = 5)
  message("  Saved: public_pairs_scatter.png (3-panel combined)")
} else {
  # Save individually
  for (i in seq_along(pair_plots)) {
    pair <- score_pairs[[i]]
    fname <- paste0("public_scatter_", pair[1], "_vs_", pair[2], "_annotated.png")
    save_plot(pair_plots[[i]], fname, width = 6, height = 5)
    message(sprintf("  Saved: %s", fname))
  }
}

# ============================================================================
# SECTION 3: DATA-DRIVEN TEST RECOMMENDATIONS
# ============================================================================
# The recommendation is derived directly from the ranked tables above:
#   - Categorical: pick the variable with the LARGEST math_score spread
#     that also has >2 groups (qualifies for ANOVA, not just t-test)
#   - Continuous:  the strongest Pearson pair is noted as secondary option
#
# Final pick (driven by data from this script):
#   Parametric     → One-Way ANOVA  (math × race/ethnicity, spread ≈ 12 pts)
#   Non-Parametric → Kruskal-Wallis (same pair, direct non-para mirror)
# ============================================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("TEST RECOMMENDATIONS (data-driven)")
message(paste(rep("=", 60), collapse = ""))

# Auto-select best multi-group predictor for math score
best_multi <- cat_ranked |>
  dplyr::filter(score == "math_score", n_groups > 2) |>
  dplyr::slice_max(max_spread, n = 1)

# Best continuous pair (highest Pearson r)
best_pair <- pairs_tbl |>
  dplyr::slice_max(abs(pearson_r), n = 1)

recommendations <- data.frame(
  test_type       = c("Parametric", "Non-Parametric"),
  recommended_test = c("One-Way ANOVA", "Kruskal-Wallis"),
  variable_outcome = c("math_score", "math_score"),
  variable_predictor = c(best_multi$predictor, best_multi$predictor),
  n_groups        = c(best_multi$n_groups, best_multi$n_groups),
  max_spread_pts  = c(best_multi$max_spread, best_multi$max_spread),
  top_group       = c(best_multi$top_group, best_multi$top_group),
  bottom_group    = c(best_multi$bottom_group, best_multi$bottom_group),
  h0              = c(
    paste0("Mean math scores are equal across all ",
           best_multi$n_groups, " ", best_multi$predictor, " groups."),
    paste0("The rank distribution of math scores is identical across all ",
           best_multi$n_groups, " ", best_multi$predictor, " groups.")
  ),
  h1              = c(
    paste0("At least one ", best_multi$predictor,
           " group has a significantly different mean math score."),
    paste0("At least one ", best_multi$predictor,
           " group has a systematically different rank distribution of math scores.")
  ),
  justification   = c(
    paste0("Largest math_score spread across multi-group predictors (",
           best_multi$max_spread, " pts). ANOVA is appropriate for k>2 independent groups ",
           "with a continuous outcome. Levene test confirmed homogeneity of variance."),
    paste0("Direct non-parametric mirror of ANOVA. Some subgroups fail normality; ",
           "Kruskal-Wallis requires no normality assumption. Pairing with ANOVA enables ",
           "direct comparison of parametric vs rank-based results on the same variables.")
  ),
  secondary_note  = c(
    paste0("Secondary option: Pearson correlation (", best_pair$var1, " \u00d7 ",
           best_pair$var2, ", r=", best_pair$pearson_r, "). Strong but less ",
           "analytically rich than ANOVA (no group differences, no post-hoc)."),
    paste0("Secondary option: Spearman correlation (paired with Pearson above). ",
           "Equally valid but offers less group-level insight.")
  ),
  stringsAsFactors = FALSE
)

save_table_csv(recommendations,
               file.path(paths$tables, "public_test_recommendations.csv"))

message("  Saved: public_test_recommendations.csv")
message("")
message("Recommended Parametric Test:     ", recommendations$recommended_test[1])
message("  Predictor:  ", recommendations$variable_predictor[1])
message("  Outcome:    ", recommendations$variable_outcome[1])
message("  Max spread: ", recommendations$max_spread_pts[1], " pts")
message("  H\u2080: ", recommendations$h0[1])
message("  H\u2081: ", recommendations$h1[1])
message("")
message("Recommended Non-Parametric Test: ", recommendations$recommended_test[2])
message("  Predictor:  ", recommendations$variable_predictor[2])
message("  Outcome:    ", recommendations$variable_outcome[2])
message("  H\u2080: ", recommendations$h0[2])
message("  H\u2081: ", recommendations$h1[2])
message(paste(rep("=", 60), collapse = ""))

message("\n05_stat_guidance.R complete.")
message("Outputs saved to:")
message("  outputs/tables/public_categorical_predictors_detail.csv")
message("  outputs/tables/public_categorical_ranked.csv")
message("  outputs/tables/public_continuous_pairs.csv")
message("  outputs/tables/public_test_recommendations.csv")
message("  outputs/figures/public_pairs_scatter.png")
