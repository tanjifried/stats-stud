### stats-stud: CCSICT preparedness study EDA
### Purpose: Generate descriptive outputs from the clean CCSICT dataset

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Loading CCSICT clean dataset...")

if (!file.exists(paths$ccsict_clean)) {
  stop("Missing clean dataset: ", paths$ccsict_clean, ". Run scripts/ccsict/02_build_ccsict_dataset_clean.R first.")
}

dataset <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)
clean_n_cols <- ncol(dataset)

dataset$preparedness_score <- as.numeric(dataset$preparedness_score)
dataset$lecture_score <- as.numeric(dataset$lecture_score)
dataset$lab_completed_binary <- dplyr::case_when(
  dataset$lab_completed == "Completed" ~ 1L,
  dataset$lab_completed == "Not completed" ~ 0L,
  TRUE ~ NA_integer_
)

expected_level <- dplyr::case_when(
  dataset$preparedness_score <= 1.80 ~ "Very Low",
  dataset$preparedness_score <= 2.60 ~ "Low",
  dataset$preparedness_score <= 3.40 ~ "Moderate",
  dataset$preparedness_score <= 4.20 ~ "High",
  dataset$preparedness_score <= 5.00 ~ "Very High",
  TRUE ~ NA_character_
)

describe_numeric <- function(x, variable, label = variable) {
  x <- stats::na.omit(x)

  data.frame(
    variable = variable,
    label = label,
    n = length(x),
    mean = mean(x),
    median = stats::median(x),
    sd = if (length(x) > 1) stats::sd(x) else NA_real_,
    min = min(x),
    max = max(x),
    stringsAsFactors = FALSE
  )
}

summarize_outliers <- function(x, variable, label = variable) {
  out <- detect_outliers(x)

  data.frame(
    variable = variable,
    label = label,
    iqr = stats::IQR(x, na.rm = TRUE),
    lower_fence = out$iqr_lower,
    upper_fence = out$iqr_upper,
    outliers = out$iqr_outliers,
    stringsAsFactors = FALSE
  )
}

make_profile <- function(data, column, variable_name) {
  data.frame(level = data[[column]], stringsAsFactors = FALSE) |>
    dplyr::count(level, name = "n") |>
    dplyr::mutate(
      variable = variable_name,
      percent = round(100 * n / sum(n), 2),
      .before = 1
    )
}

quality_checks <- data.frame(
  check = c(
    "n_rows",
    "n_cols",
    "duplicate_respondent_id",
    "missing_preparedness_score",
    "missing_lecture_score",
    "missing_lab_completed",
    "preparedness_score_outside_1_5",
    "invalid_preparedness_level_rows"
  ),
  value = c(
    nrow(dataset),
    clean_n_cols,
    sum(duplicated(dataset$respondent_id)),
    sum(is.na(dataset$preparedness_score)),
    sum(is.na(dataset$lecture_score)),
    sum(is.na(dataset$lab_completed_binary)),
    sum(dataset$preparedness_score < 1 | dataset$preparedness_score > 5, na.rm = TRUE),
    sum(dplyr::coalesce(expected_level, "") != dplyr::coalesce(dataset$preparedness_level, ""))
  ),
  stringsAsFactors = FALSE
)

respondent_profile <- dplyr::bind_rows(
  make_profile(dataset, "sex", "sex"),
  make_profile(dataset, "program", "program"),
  make_profile(dataset, "preparation_mode", "preparation_mode"),
  make_profile(dataset, "preparedness_level", "preparedness_level"),
  make_profile(dataset, "lab_completed", "lab_completed")
)

preparedness_descriptives <- describe_numeric(
  dataset$preparedness_score,
  "preparedness_score",
  "Preparedness Score"
)

numeric_descriptives <- dplyr::bind_rows(
  preparedness_descriptives,
  describe_numeric(dataset$lecture_score, "lecture_score", "Lecture Score")
)

score_descriptives <- dplyr::bind_rows(
  describe_numeric(dataset$lecture_score, "lecture_score", "Lecture Score"),
  describe_numeric(dataset$lab_completed_binary, "lab_completed", "Laboratory Completed (0/1)")
)

outlier_summary <- dplyr::bind_rows(
  summarize_outliers(dataset$preparedness_score, "preparedness_score", "Preparedness Score"),
  summarize_outliers(dataset$lecture_score, "lecture_score", "Lecture Score")
)

preparedness_by_program <- dataset |>
  dplyr::group_by(program) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_preparedness_score = mean(preparedness_score, na.rm = TRUE),
    median_preparedness_score = stats::median(preparedness_score, na.rm = TRUE),
    mean_lecture_score = mean(lecture_score, na.rm = TRUE),
    lab_completion_rate = mean(lab_completed_binary, na.rm = TRUE) * 100,
    .groups = "drop"
  )

save_table_csv(quality_checks, file.path(paths$tables, "ccsict_quality_checks.csv"))
save_table_csv(respondent_profile, file.path(paths$tables, "ccsict_respondent_profile.csv"))
save_table_csv(numeric_descriptives, file.path(paths$tables, "ccsict_numeric_descriptives.csv"))
save_table_csv(preparedness_descriptives, file.path(paths$tables, "ccsict_preparedness_descriptives.csv"))
save_table_csv(score_descriptives, file.path(paths$tables, "ccsict_score_descriptives.csv"))
save_table_csv(outlier_summary, file.path(paths$tables, "ccsict_outlier_summary.csv"))
save_table_csv(preparedness_by_program, file.path(paths$tables, "ccsict_preparedness_by_program.csv"))

# ============================================================================
# VISUALIZATIONS
# ============================================================================

message("Creating visualizations...")

score_cols <- c("lecture_score", "preparedness_score")
score_colors <- c(
  lecture_score = "#2563eb",
  preparedness_score = "#059669"
)

pretty_labels <- c(
  sex = "Sex",
  program = "Program",
  preparation_mode = "Preparation Mode",
  preparedness_level = "Preparedness Level",
  lab_completed = "Laboratory Completion Status",
  lecture_score = "Lecture Score",
  preparedness_score = "Preparedness Score"
)

hist_specs <- data.frame(
  variable = c("lecture_score", "preparedness_score"),
  binwidth = c(14, 0.5),
  boundary = c(35, 2.5),
  title = c("Frequency Distribution of Lecture Scores", "Frequency Distribution of Preparedness Scores"),
  x_label = c("Lecture Score", "Preparedness Score"),
  stringsAsFactors = FALSE
)

p_box_overall <- dataset |>
  dplyr::select(preparedness_score, lecture_score) |>
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "value") |>
  ggplot2::ggplot(ggplot2::aes(x = variable, y = value, fill = variable)) +
  ggplot2::geom_boxplot(alpha = 0.85, show.legend = FALSE) +
  ggplot2::labs(
    title = "Boxplot of Preparedness and Lecture Score",
    x = "Variable",
    y = "Value"
  ) +
  ggplot2::theme_minimal()

save_plot(p_box_overall, "ccsict_box_overall_scores.png", width = 7, height = 5)

for (i in seq_len(nrow(hist_specs))) {
  spec <- hist_specs[i, ]

  p_hist <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[spec$variable]])) +
    ggplot2::geom_histogram(
      binwidth = spec$binwidth,
      boundary = spec$boundary,
      fill = score_colors[[spec$variable]],
      color = "white"
    ) +
    ggplot2::labs(
      title = spec$title,
      x = spec$x_label,
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_hist, paste0("ccsict_hist_", spec$variable, ".png"), width = 7, height = 5)
}

for (sc in score_cols) {
  p_qq <- ggplot2::ggplot(dataset, ggplot2::aes(sample = .data[[sc]])) +
    ggplot2::stat_qq(color = score_colors[[sc]]) +
    ggplot2::stat_qq_line(color = "black") +
    ggplot2::labs(
      title = paste("Normal Q-Q Plot of", pretty_labels[[sc]]),
      subtitle = "Deviations from line indicate non-normality"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_qq, paste0("ccsict_qqplot_", sc, ".png"), width = 7, height = 5)
}

bar_specs <- data.frame(
  variable = c("sex", "program", "preparation_mode", "preparedness_level", "lab_completed"),
  width = c(7, 7, 8, 7, 7),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(bar_specs))) {
  spec <- bar_specs[i, ]

  p_bar <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[spec$variable]])) +
    ggplot2::geom_bar(fill = "steelblue") +
    ggplot2::labs(
      title = paste("Distribution of", pretty_labels[[spec$variable]]),
      x = pretty_labels[[spec$variable]],
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  if (spec$variable %in% c("preparation_mode", "preparedness_level")) {
    p_bar <- p_bar +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }

  save_plot(p_bar, paste0("ccsict_bar_", spec$variable, ".png"), width = spec$width, height = 5)
}

pearson_r <- stats::cor(dataset$preparedness_score, dataset$lecture_score, use = "complete.obs")

scatter_pairs <- list(
  c("preparedness_score", "lecture_score")
)

for (pair in scatter_pairs) {
  x <- pair[1]
  y <- pair[2]

  p_scatter <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(alpha = 0.5, color = score_colors[[x]]) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = score_colors[[y]]) +
    ggplot2::labs(
      title = paste(pretty_labels[[x]], "versus", pretty_labels[[y]]),
      subtitle = sprintf("Pearson r = %.3f", pearson_r),
      x = pretty_labels[[x]],
      y = pretty_labels[[y]]
    ) +
    ggplot2::theme_minimal()

  save_plot(p_scatter, paste0("ccsict_scatter_", x, "_vs_", y, ".png"), width = 7, height = 5)
}

# ============================================================================
# GROUPED BOXPLOTS
# ============================================================================

message("Creating grouped boxplots...")

box_specs <- list(
  list(x = "program", y = "preparedness_score", width = 7),
  list(x = "sex", y = "preparedness_score", width = 7),
  list(x = "preparation_mode", y = "preparedness_score", width = 8),
  list(x = "program", y = "lecture_score", width = 7),
  list(x = "sex", y = "lecture_score", width = 7),
  list(x = "preparation_mode", y = "lecture_score", width = 8)
)

for (spec in box_specs) {
  p_box <- ggplot2::ggplot(dataset, ggplot2::aes(x = .data[[spec$x]], y = .data[[spec$y]], fill = .data[[spec$x]])) +
    ggplot2::geom_boxplot(alpha = 0.85, show.legend = FALSE) +
    ggplot2::labs(
      title = paste(pretty_labels[[spec$y]], "by", pretty_labels[[spec$x]]),
      x = pretty_labels[[spec$x]],
      y = pretty_labels[[spec$y]]
    ) +
    ggplot2::theme_minimal()

  if (spec$x == "preparation_mode") {
    p_box <- p_box +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  }

  save_plot(p_box, paste0("ccsict_box_", spec$y, "_by_", spec$x, ".png"), width = spec$width, height = 5)
}

message("")
message("============================================================")
message("CCSICT EDA COMPLETE")
message("============================================================")
message("Clean dataset: ", paths$ccsict_clean)
message("Tables saved to: ", paths$tables)
message("Figures saved to: ", paths$figures)
message("============================================================")
