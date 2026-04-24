### stats-stud: CCSICT preparedness study tests
### Purpose: Run valid hypothesis tests from the clean CCSICT dataset

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Loading CCSICT clean dataset...")

if (!file.exists(paths$ccsict_clean)) {
  stop("Missing clean dataset: ", paths$ccsict_clean, ". Run scripts/ccsict/02_build_ccsict_dataset_clean.R first.")
}

df <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)

df$preparedness_score <- as.numeric(df$preparedness_score)
df$lecture_score <- as.numeric(df$lecture_score)
df$lab_completed_binary <- dplyr::case_when(
  df$lab_completed == "Completed" ~ 1L,
  df$lab_completed == "Not completed" ~ 0L,
  TRUE ~ NA_integer_
)

normality_to_df <- function(x, variable_name) {
  res <- assess_normality(x, variable_name)

  data.frame(
    variable = res$variable,
    n = res$n,
    mean = res$mean,
    sd = res$sd,
    median = res$median,
    skewness = res$skewness,
    kurtosis = res$kurtosis,
    ad_statistic = res$ad_statistic,
    ad_pvalue = res$ad_pvalue,
    is_normal = res$is_normal,
    interpretation = res$interpretation,
    recommendation = res$recommendation,
    stringsAsFactors = FALSE
  )
}

cor_strength <- function(r) {
  abs_r <- abs(r)

  dplyr::case_when(
    abs_r < 0.20 ~ "Negligible",
    abs_r < 0.40 ~ "Weak",
    abs_r < 0.60 ~ "Moderate",
    abs_r < 0.80 ~ "Strong",
    TRUE ~ "Very strong"
  )
}

message("Running normality checks...")

normality_results <- dplyr::bind_rows(
  normality_to_df(df$lecture_score, "lecture_score"),
  normality_to_df(df$preparedness_score, "preparedness_score")
)

save_table_csv(normality_results, file.path(paths$tables, "ccsict_normality_assessment.csv"))

message("Testing preparedness vs lecture score...")

pearson_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "pearson")
spearman_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "spearman", exact = FALSE)

lecture_ready_for_pearson <- all(normality_results$is_normal[normality_results$variable %in% c("lecture_score", "preparedness_score")])
recommended_test <- if (lecture_ready_for_pearson) "Pearson" else "Spearman"

lecture_corr <- data.frame(
  test = c("Pearson", "Spearman"),
  variable_x = "preparedness_score",
  variable_y = "lecture_score",
  estimate = c(unname(pearson_res$estimate), unname(spearman_res$estimate)),
  statistic = c(unname(pearson_res$statistic), unname(spearman_res$statistic)),
  p_value = c(pearson_res$p.value, spearman_res$p.value),
  significant = c(pearson_res$p.value < 0.05, spearman_res$p.value < 0.05),
  interpretation = c(cor_strength(unname(pearson_res$estimate)), cor_strength(unname(spearman_res$estimate))),
  recommended = c(recommended_test == "Pearson", recommended_test == "Spearman"),
  decision = c(
    ifelse(pearson_res$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    ifelse(spearman_res$p.value < 0.05, "Reject H0", "Fail to reject H0")
  ),
  stringsAsFactors = FALSE
)

save_table_csv(lecture_corr, file.path(paths$tables, "ccsict_preparedness_vs_lecture.csv"))

lecture_lab_note <- data.frame(
  analysis = "lecture_vs_lab_difference",
  status = "not_run",
  reason = "lecture_score is a continuous exam score, while lab_completed is a binary completion outcome. A paired t-test or Wilcoxon signed-rank test would not be valid because the measures are not on the same scale.",
  recommendation = "Report lecture score and lab completion separately in the paper.",
  stringsAsFactors = FALSE
)

save_table_csv(lecture_lab_note, file.path(paths$tables, "ccsict_lecture_vs_lab_note.csv"))

message("Checking whether preparedness vs laboratory completion can be tested...")

completed_n <- sum(df$lab_completed_binary == 1, na.rm = TRUE)
not_completed_n <- sum(df$lab_completed_binary == 0, na.rm = TRUE)

if (completed_n >= 2 && not_completed_n >= 2) {
  lab_res <- stats::cor.test(df$preparedness_score, df$lab_completed_binary, method = "pearson")

  lab_tbl <- data.frame(
    analysis = "preparedness_vs_lab_completed",
    status = "completed",
    test = "Point-biserial correlation (Pearson with binary outcome)",
    estimate = unname(lab_res$estimate),
    statistic = unname(lab_res$statistic),
    p_value = lab_res$p.value,
    significant = lab_res$p.value < 0.05,
    completed_n = completed_n,
    not_completed_n = not_completed_n,
    interpretation = cor_strength(unname(lab_res$estimate)),
    decision = ifelse(lab_res$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    reason = "Binary laboratory completion had enough cases in both groups for a point-biserial correlation.",
    stringsAsFactors = FALSE
  )
} else {
  lab_tbl <- data.frame(
    analysis = "preparedness_vs_lab_completed",
    status = "skipped",
    test = NA_character_,
    estimate = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    significant = NA,
    completed_n = completed_n,
    not_completed_n = not_completed_n,
    interpretation = NA_character_,
    decision = "Not tested",
    reason = "Laboratory completion is too imbalanced for a stable inferential test. At least two students are needed in each group.",
    stringsAsFactors = FALSE
  )
}

save_table_csv(lab_tbl, file.path(paths$tables, "ccsict_preparedness_vs_lab.csv"))

selected_lecture_test <- lecture_corr[lecture_corr$recommended, , drop = FALSE]

hypothesis_summary <- data.frame(
  hypothesis = c(
    "H1: Lecture vs laboratory performance",
    "H2: Preparedness vs lecture performance",
    "H3: Preparedness vs laboratory performance"
  ),
  status = c(
    lecture_lab_note$status[1],
    "completed",
    lab_tbl$status[1]
  ),
  test_used = c(
    "Not run",
    selected_lecture_test$test[1],
    ifelse(is.na(lab_tbl$test[1]), "Not run", lab_tbl$test[1])
  ),
  estimate = c(
    NA_real_,
    selected_lecture_test$estimate[1],
    lab_tbl$estimate[1]
  ),
  p_value = c(
    NA_real_,
    selected_lecture_test$p_value[1],
    lab_tbl$p_value[1]
  ),
  decision = c(
    "Not tested",
    selected_lecture_test$decision[1],
    lab_tbl$decision[1]
  ),
  note = c(
    lecture_lab_note$reason[1],
    paste0("Recommended test based on normality: ", recommended_test),
    lab_tbl$reason[1]
  ),
  stringsAsFactors = FALSE
)

save_table_csv(hypothesis_summary, file.path(paths$tables, "ccsict_hypothesis_summary.csv"))

message("")
message("============================================================")
message("CCSICT HYPOTHESIS TESTS COMPLETE")
message("============================================================")
message("Preparedness vs lecture recommended test: ", recommended_test)
message("Preparedness vs lecture p-value: ", format_pvalue(selected_lecture_test$p_value[1]))
message("Preparedness vs lab status: ", lab_tbl$status[1])
message("Lecture vs lab status: ", lecture_lab_note$status[1])
message("Tables saved to: ", paths$tables)
message("============================================================")
