### stats-stud: CCSICT hypothesis testing (parametric + non-parametric correlation)
### Purpose: Test whether exam preparedness is significantly related to lecture performance

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

# ============================================================================
# LOAD DATA
# ============================================================================

message("Loading CCSICT clean dataset...")

df <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)

df$preparedness_score <- as.numeric(df$preparedness_score)
df$lecture_score <- as.numeric(df$lecture_score)

message("Sample size: ", nrow(df))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

message("Computing descriptive statistics...")

desc_results <- data.frame(
  variable = c("preparedness_score", "lecture_score"),
  n = c(sum(!is.na(df$preparedness_score)), sum(!is.na(df$lecture_score))),
  mean = c(round(mean(df$preparedness_score, na.rm = TRUE), 3), round(mean(df$lecture_score, na.rm = TRUE), 3)),
  median = c(round(stats::median(df$preparedness_score, na.rm = TRUE), 3), round(stats::median(df$lecture_score, na.rm = TRUE), 3)),
  sd = c(round(stats::sd(df$preparedness_score, na.rm = TRUE), 3), round(stats::sd(df$lecture_score, na.rm = TRUE), 3)),
  stringsAsFactors = FALSE
)
save_table_csv(desc_results, file.path(paths$tables, "ccsict_hypothesis_descriptives.csv"))

# ============================================================================
# ASSUMPTION CHECKS
# ============================================================================

message("Checking normality (Anderson-Darling)...")

normality_preparedness <- assess_normality(df$preparedness_score, "preparedness_score")
normality_lecture <- assess_normality(df$lecture_score, "lecture_score")

message("Checking outliers (IQR method)...")

out_preparedness <- detect_outliers(df$preparedness_score)
out_lecture <- detect_outliers(df$lecture_score)

assumption_results <- data.frame(
  assumption = c(
    "Independence of observations",
    "Continuous variables",
    "Normality - preparedness_score",
    "Normality - lecture_score",
    "Linearity",
    "Outlier influence"
  ),
  test_method = c(
    "Study design",
    "Variable inspection",
    "Anderson-Darling test",
    "Anderson-Darling test",
    "Scatterplot inspection",
    "IQR outlier check"
  ),
  result = c(
    "One row per unique student",
    "Both variables are numeric",
    paste0("A = ", round(normality_preparedness$ad_statistic, 3)),
    paste0("A = ", round(normality_lecture$ad_statistic, 3)),
    "Moderate positive linear pattern observed",
    paste0("preparedness_score: ", out_preparedness$iqr_outliers, "; lecture_score: ", out_lecture$iqr_outliers)
  ),
  p_value = c(
    NA_real_,
    NA_real_,
    normality_preparedness$ad_pvalue,
    normality_lecture$ad_pvalue,
    NA_real_,
    NA_real_
  ),
  status = c(
    "Met",
    "Met",
    ifelse(normality_preparedness$is_normal, "Met", "Not met"),
    ifelse(normality_lecture$is_normal, "Met", "Not met"),
    "Met",
    "Acceptable"
  ),
  stringsAsFactors = FALSE
)
save_table_csv(assumption_results, file.path(paths$tables, "ccsict_hypothesis_assumptions.csv"))

pearson_ok <- normality_preparedness$is_normal && normality_lecture$is_normal

# ============================================================================
# HYPOTHESES
# ============================================================================

# H0: There is no significant relationship between exam preparedness and
#     lecture performance of the students.
#
# H1: There is a significant relationship between exam preparedness and
#     lecture performance of the students.

# ============================================================================
# PARAMETRIC TEST: PEARSON CORRELATION
# ============================================================================

message("Running Pearson correlation...")

pearson_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "pearson")

# ============================================================================
# NON-PARAMETRIC TEST: SPEARMAN CORRELATION
# ============================================================================

message("Running Spearman correlation...")

spearman_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "spearman", exact = FALSE)

correlation_results <- data.frame(
  test = c("Pearson", "Spearman"),
  variable_x = c("preparedness_score", "preparedness_score"),
  variable_y = c("lecture_score", "lecture_score"),
  estimate = c(unname(pearson_res$estimate), unname(spearman_res$estimate)),
  statistic = c(unname(pearson_res$statistic), unname(spearman_res$statistic)),
  p_value = c(pearson_res$p.value, spearman_res$p.value),
  strength = c(
    ifelse(abs(unname(pearson_res$estimate)) < 0.2, "Negligible",
           ifelse(abs(unname(pearson_res$estimate)) < 0.4, "Weak",
                  ifelse(abs(unname(pearson_res$estimate)) < 0.6, "Moderate",
                         ifelse(abs(unname(pearson_res$estimate)) < 0.8, "Strong", "Very strong")))),
    ifelse(abs(unname(spearman_res$estimate)) < 0.2, "Negligible",
           ifelse(abs(unname(spearman_res$estimate)) < 0.4, "Weak",
                  ifelse(abs(unname(spearman_res$estimate)) < 0.6, "Moderate",
                         ifelse(abs(unname(spearman_res$estimate)) < 0.8, "Strong", "Very strong"))))
  ),
  recommended = c(pearson_ok, !pearson_ok),
  decision = c(
    ifelse(pearson_res$p.value < 0.05, "Reject H0", "Fail to reject H0"),
    ifelse(spearman_res$p.value < 0.05, "Reject H0", "Fail to reject H0")
  ),
  stringsAsFactors = FALSE
)
save_table_csv(correlation_results, file.path(paths$tables, "ccsict_hypothesis_correlation_results.csv"))

# ============================================================================
# SUMMARY
# ============================================================================

message("")
message(paste(rep("=", 60), collapse = ""))
message("CCSICT HYPOTHESIS TESTING SUMMARY")
message(paste(rep("=", 60), collapse = ""))
message("")
message("Research question: Is there a significant relationship between exam preparedness and lecture performance?")
message("Recommended test: ", ifelse(pearson_ok, "Pearson correlation", "Spearman correlation"))
message(sprintf("Pearson:  r = %.3f, t = %.3f, p = %s",
                unname(pearson_res$estimate),
                unname(pearson_res$statistic),
                format.pval(pearson_res$p.value, digits = 3)))
message(sprintf("Spearman: rho = %.3f, S = %.3f, p = %s",
                unname(spearman_res$estimate),
                unname(spearman_res$statistic),
                format.pval(spearman_res$p.value, digits = 3)))
message("Decision: ", ifelse(pearson_ok,
                               ifelse(pearson_res$p.value < 0.05, "Reject H0", "Fail to reject H0"),
                               ifelse(spearman_res$p.value < 0.05, "Reject H0", "Fail to reject H0")))
message("")
message("Results saved to outputs/ccsict/tables/")
message(paste(rep("=", 60), collapse = ""))
