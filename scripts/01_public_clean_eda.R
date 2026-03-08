### stats-stud: public dataset cleaning + EDA (enhanced)
### Purpose: Clean public dataset and perform comprehensive exploratory analysis
### 
### Changes from original:
### - Added outlier detection using IQR and Modified Z-score methods
### - Implemented comprehensive normality assessment (Anderson-Darling instead of Shapiro-Wilk)
### - Added correlation matrix and visualization
### - Added skewness and kurtosis calculations
### - Implemented QQ plots for normality visualization
### - Added duplicate detection
### - Improved histogram bin selection using Freedman-Diaconis rule
### - Added interaction visualizations (e.g., Math by Gender × Test Prep)
### - Added missing data pattern analysis
### - Implemented comprehensive documentation

source("scripts/00_setup.R")

#' Clean variable names: convert to lowercase, replace special chars with underscore
#' @param x Character vector of names
#' @return Cleaned names
clean_names_simple <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

# ============================================================================
# DATA LOADING
# ============================================================================

message("Loading public dataset...")

raw <- utils::read.csv(
  paths$public_raw,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(raw) <- clean_names_simple(names(raw))

# Define expected columns
expected <- c(
  "gender",
  "race_ethnicity",
  "parental_level_of_education",
  "lunch",
  "test_preparation_course",
  "math_score",
  "reading_score",
  "writing_score"
)

# Validate columns
missing_cols <- setdiff(expected, names(raw))
if (length(missing_cols) > 0) {
  stop("Unexpected columns in public dataset. Missing: ", paste(missing_cols, collapse = ", "))
}

# ============================================================================
# DATA CLEANING
# ============================================================================

message("Cleaning data...")

df <- raw |>
  dplyr::mutate(
    # Clean categorical variables
    dplyr::across(
      dplyr::all_of(c(
        "gender",
        "race_ethnicity",
        "parental_level_of_education",
        "lunch",
        "test_preparation_course"
      )),
      ~ trimws(gsub("\\s+", " ", as.character(.x)))
    ),
    # Convert scores to numeric (suppress warnings for non-numeric entries)
    dplyr::across(
      dplyr::all_of(c("math_score", "reading_score", "writing_score")),
      ~ suppressWarnings(as.integer(.x))
    )
  )

# Define score columns for reuse
score_cols <- c("math_score", "reading_score", "writing_score")

# Consistent colors for each score
score_colors <- c(
  math_score = "#1F77B4",    # blue
  reading_score = "#2CA02C", # green
  writing_score = "#D62728"  # red
)

# ============================================================================
# QUALITY CHECKS (Enhanced)
# ============================================================================

message("Performing quality checks...")

# Check for duplicates
duplicates <- df[duplicated(df), ]
n_duplicates <- nrow(duplicates)

# Check for impossible scores (beyond 0-100 range)
impossible_scores <- df |>
  dplyr::filter(
    math_score < 0 | math_score > 100 |
      reading_score < 0 | reading_score > 100 |
      writing_score < 0 | writing_score > 100
  )

# Outlier detection for each score
outlier_results <- lapply(score_cols, function(sc) {
  out <- detect_outliers(df[[sc]])
  data.frame(
    variable = sc,
    n_total = out$n_total,
    n_missing = out$n_missing,
    iqr_outliers = out$iqr_outliers,
    iqr_bounds = paste0("[", round(out$iqr_lower, 2), ", ", round(out$iqr_upper, 2), "]"),
    modz_outliers = out$modz_outliers,
    extreme_outliers = out$extreme_outliers,
    stringsAsFactors = FALSE
  )
})
outlier_summary <- dplyr::bind_rows(outlier_results)

# Compile quality checks
quality <- dplyr::tibble(
  check = c(
    "n_rows",
    "n_cols",
    "n_duplicates",
    "any_missing_scores",
    "scores_outside_0_100",
    "n_impossible_scores"
  ),
  value = c(
    nrow(df),
    ncol(df),
    n_duplicates,
    any(is.na(df[, score_cols])),
    sum(
      df$math_score < 0 | df$math_score > 100 |
        df$reading_score < 0 | df$reading_score > 100 |
        df$writing_score < 0 | df$writing_score > 100,
      na.rm = TRUE
    ),
    nrow(impossible_scores)
  )
)

save_table_csv(quality, file.path(paths$tables, "public_quality_checks.csv"))
save_table_csv(outlier_summary, file.path(paths$tables, "public_outlier_detection.csv"))

if (n_duplicates > 0) {
  message(sprintf("WARNING: Found %d duplicate rows", n_duplicates))
  save_table_csv(duplicates, file.path(paths$tables, "public_duplicate_rows.csv"))
}

# Save cleaned dataset
save_table_csv(df, paths$public_clean)

# ============================================================================
# DATA DICTIONARY
# ============================================================================

message("Creating data dictionary...")

data_dict <- dplyr::tibble(
  variable = names(df),
  type = vapply(df, function(x) class(x)[1], character(1)),
  n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
  n_unique = vapply(df, function(x) dplyr::n_distinct(x), integer(1))
)
save_table_csv(data_dict, file.path(paths$tables, "public_data_dictionary.csv"))

# ============================================================================
# MISSING DATA PATTERN ANALYSIS
# ============================================================================

message("Analyzing missing data patterns...")

missing_pattern <- df |>
  dplyr::mutate(
    missing_math = is.na(math_score),
    missing_reading = is.na(reading_score),
    missing_writing = is.na(writing_score)
  ) |>
  dplyr::group_by(missing_math, missing_reading, missing_writing) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(desc(n))

save_table_csv(missing_pattern, file.path(paths$tables, "public_missing_patterns.csv"))

# Visualize missing data pattern (if any missing)
if (sum(is.na(df[score_cols])) > 0) {
  p_missing <- df |>
    dplyr::select(dplyr::all_of(score_cols)) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") |>
    dplyr::mutate(is_missing = is.na(value)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(n_missing = sum(is_missing), pct_missing = mean(is_missing) * 100) |>
    ggplot2::ggplot(ggplot2::aes(x = variable, y = pct_missing)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%d (%.1f%%)", n_missing, pct_missing)), 
                       vjust = -0.5) +
    ggplot2::labs(
      title = "Missing Data by Variable",
      x = "Variable",
      y = "Percentage Missing"
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_missing, "public_missing_data_pattern.png")
}

# ============================================================================
# DESCRIPTIVE STATISTICS (Enhanced with skewness and kurtosis)
# ============================================================================

message("Computing descriptive statistics...")

#' Calculate comprehensive descriptive statistics including skewness and kurtosis
#' @param data Data frame
#' @return Summary statistics data frame
score_summary <- function(data) {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to = "score",
      values_to = "value"
    ) |>
    dplyr::group_by(score) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      missing = sum(is.na(value)),
      mean = mean(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      iqr = stats::IQR(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      skewness = moments::skewness(value, na.rm = TRUE),
      kurtosis = moments::kurtosis(value, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate descriptive statistics by group
#' @param data Data frame
#' @param group_col Grouping variable
#' @return Grouped summary statistics
score_summary_by <- function(data, group_col) {
  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to = "score",
      values_to = "value"
    ) |>
    dplyr::group_by(.data[[group_col]], score) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      iqr = stats::IQR(value, na.rm = TRUE),
      skewness = moments::skewness(value, na.rm = TRUE),
      kurtosis = moments::kurtosis(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(group = dplyr::all_of(group_col))
}

overall_stats <- score_summary(df)
save_table_csv(overall_stats, file.path(paths$tables, "public_eda_scores_overall.csv"))

group_vars <- c(
  "gender",
  "lunch",
  "test_preparation_course",
  "parental_level_of_education",
  "race_ethnicity"
)

for (g in group_vars) {
  tbl <- score_summary_by(df, g)
  save_table_csv(tbl, file.path(paths$tables, paste0("public_eda_scores_by_", g, ".csv")))
}

# ============================================================================
# CORRELATION ANALYSIS
# ============================================================================

message("Computing correlation matrix...")

# Pearson correlations (for parametric) and Spearman (for non-parametric robustness)
cor_matrix_pearson <- df |>
  dplyr::select(dplyr::all_of(score_cols)) |>
  stats::cor(method = "pearson", use = "pairwise.complete.obs")

cor_matrix_spearman <- df |>
  dplyr::select(dplyr::all_of(score_cols)) |>
  stats::cor(method = "spearman", use = "pairwise.complete.obs")

# Save correlation matrices
cor_pearson_df <- as.data.frame(cor_matrix_pearson)
cor_pearson_df$variable <- rownames(cor_matrix_pearson)
save_table_csv(cor_pearson_df, file.path(paths$tables, "public_correlation_pearson.csv"))

cor_spearman_df <- as.data.frame(cor_matrix_spearman)
cor_spearman_df$variable <- rownames(cor_matrix_spearman)
save_table_csv(cor_spearman_df, file.path(paths$tables, "public_correlation_spearman.csv"))

# Visualize correlation matrix (essential-only dependency: ggplot2)
# Note: We avoid GGally::ggpairs to keep dependencies minimal.
cor_long <- as.data.frame(cor_matrix_pearson, stringsAsFactors = FALSE)
cor_long$var1 <- rownames(cor_matrix_pearson)

cor_long <- cor_long |>
  tidyr::pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "pearson_r"
  )

p_cor <- ggplot2::ggplot(cor_long, ggplot2::aes(x = var1, y = var2, fill = pearson_r)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", pearson_r)), size = 4) +
  ggplot2::scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  ggplot2::labs(
    title = "Correlation Heatmap (Pearson)",
    subtitle = "Score variables",
    x = NULL,
    y = NULL,
    fill = "r"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

save_plot(p_cor, "public_correlation_matrix.png", width = 7, height = 6)

# ============================================================================
# NORMALITY ASSESSMENT (Enhanced - Anderson-Darling, QQ plots)
# ============================================================================

message("Assessing normality...")

# Overall normality assessment using Anderson-Darling (better for large samples)
normality_overall <- lapply(score_cols, function(sc) {
  result <- assess_normality(df[[sc]], sc)
  as.data.frame(result, stringsAsFactors = FALSE)
})
normality_df <- dplyr::bind_rows(normality_overall)
save_table_csv(normality_df, file.path(paths$tables, "public_normality_assessment.csv"))

# Group-wise normality (for test prep groups)
normality_by_prep <- lapply(score_cols, function(sc) {
  lapply(sort(unique(df$test_preparation_course)), function(g) {
    x <- df[[sc]][df$test_preparation_course == g]
    x <- stats::na.omit(x)
    if (length(x) > 7) {  # Need at least 8 observations for AD test
      result <- assess_normality(x, paste0(sc, "_", g))
      as.data.frame(result, stringsAsFactors = FALSE)
    } else {
      data.frame(
        variable = paste0(sc, "_", g),
        n = length(x),
        interpretation = "Insufficient sample size for normality test",
        stringsAsFactors = FALSE
      )
    }
  }) |>
    dplyr::bind_rows()
}) |>
  dplyr::bind_rows()

save_table_csv(normality_by_prep, file.path(paths$tables, "public_normality_by_prep.csv"))

# ============================================================================
# VISUALIZATIONS (Enhanced)
# ============================================================================

message("Creating visualizations...")

# Histograms with optimal bin selection (Freedman-Diaconis rule)
for (sc in score_cols) {
  x <- stats::na.omit(df[[sc]])
  # Calculate optimal bins using Freedman-Diaconis rule
  bin_width <- 2 * stats::IQR(x) / length(x)^(1/3)
  n_bins <- max(10, min(50, ceiling(diff(range(x)) / bin_width)))
  
  p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[sc]])) +
    ggplot2::geom_histogram(bins = n_bins, fill = score_colors[[sc]], color = "white") +
    ggplot2::geom_vline(xintercept = mean(x), color = "black", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = median(x), color = "gray40", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = paste("Distribution of", sc),
      subtitle = sprintf("Mean = %.1f (red), Median = %.1f (green), Skewness = %.2f",
                        mean(x), median(x), moments::skewness(x)),
      x = sc,
      y = "Count"
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_hist, paste0("public_hist_", sc, ".png"))
}

# Boxplots
for (sc in score_cols) {
  p_box <- ggplot2::ggplot(df, ggplot2::aes(y = .data[[sc]])) +
    ggplot2::geom_boxplot(fill = score_colors[[sc]], alpha = 0.7) +
    ggplot2::labs(
      title = paste("Boxplot:", sc),
      subtitle = paste("IQR method outliers shown as points"),
      x = NULL,
      y = sc
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_box, paste0("public_box_", sc, ".png"), width = 5, height = 5)
}

# QQ plots for normality visualization
for (sc in score_cols) {
  p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = .data[[sc]])) +
    ggplot2::stat_qq(color = score_colors[[sc]]) +
    ggplot2::stat_qq_line(color = "black") +
    ggplot2::labs(
      title = paste("QQ Plot:", sc),
      subtitle = "Deviations from red line indicate non-normality"
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_qq, paste0("public_qqplot_", sc, ".png"))
}

# Bar charts for categorical variables
for (cat_col in c("gender", "race_ethnicity", "parental_level_of_education", "lunch", "test_preparation_course")) {
  p_bar <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[cat_col]])) +
    ggplot2::geom_bar(fill = "steelblue") +
    ggplot2::labs(
      title = paste("Distribution of", cat_col),
      x = cat_col,
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  
  save_plot(p_bar, paste0("public_bar_", cat_col, ".png"), width = 8, height = 5)
}

# Scatterplots with regression line and correlation
scatter_pairs <- list(
  c("math_score", "reading_score"),
  c("reading_score", "writing_score"),
  c("math_score", "writing_score")
)

for (pair in scatter_pairs) {
  x <- pair[1]
  y <- pair[2]
  
  # Calculate correlation
  cor_val <- cor(df[[x]], df[[y]], use = "complete.obs")
  
  p_scatter <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(alpha = 0.5, color = score_colors[[x]]) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = score_colors[[y]]) +
    ggplot2::labs(
      title = paste("Relationship:", x, "vs", y),
      subtitle = sprintf("Pearson r = %.3f", cor_val),
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_scatter, paste0("public_scatter_", x, "_vs_", y, ".png"), width = 7, height = 5)
}

# ============================================================================
# INTERACTION VISUALIZATIONS (New)
# ============================================================================

message("Creating interaction visualizations...")

# Math scores by Gender × Test Preparation interaction
p_interaction <- df |>
  tidyr::pivot_longer(cols = dplyr::all_of(score_cols), names_to = "subject", values_to = "score") |>
  ggplot2::ggplot(ggplot2::aes(x = gender, y = score, fill = test_preparation_course)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~ subject, ncol = 3) +
  ggplot2::labs(
    title = "Score Distributions by Gender and Test Preparation",
    x = "Gender",
    y = "Score",
    fill = "Test Prep"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))

save_plot(p_interaction, "public_interaction_gender_prep.png", width = 12, height = 5)

# Density plots comparing groups
density_vars <- list(
  list(var = "gender", label = "Gender"),
  list(var = "test_preparation_course", label = "Test Prep"),
  list(var = "parental_level_of_education", label = "Parent Education")
)

for (dvar in density_vars) {
  p_density <- df |>
    tidyr::pivot_longer(cols = dplyr::all_of(score_cols), names_to = "subject", values_to = "score") |>
    ggplot2::ggplot(ggplot2::aes(x = score, color = .data[[dvar$var]], fill = .data[[dvar$var]])) +
    ggplot2::geom_density(alpha = 0.3) +
    ggplot2::facet_wrap(~ subject, ncol = 3) +
    ggplot2::labs(
      title = paste("Score Densities by", dvar$label),
      x = "Score",
      y = "Density",
      color = dvar$label,
      fill = dvar$label
    ) +
    ggplot2::theme_minimal()
  
  save_plot(p_density, paste0("public_density_by_", dvar$var, ".png"), width = 12, height = 5)
}

# ============================================================================
# SUMMARY REPORT
# ============================================================================

message(paste0("\n", paste(rep("=", 60), collapse = "")))
message("EDA SUMMARY REPORT")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("Dataset: %d rows × %d columns", nrow(df), ncol(df)))
message(sprintf("Duplicates found: %d", n_duplicates))
message(sprintf("Missing values in scores: %d", sum(is.na(df[score_cols]))))
message("\nCorrelations (Pearson):")
print(round(cor_matrix_pearson, 3))
message("\nNormality Assessment:")
print(normality_df |>
  dplyr::select(variable, n, skewness, kurtosis, ad_pvalue, is_normal))
message("\nOutlier Detection:")
print(outlier_summary)
message(paste(rep("=", 60), collapse = ""))

message("\nPublic cleaning + EDA complete.")
message("Outputs saved to: outputs/tables/ and outputs/figures/")
