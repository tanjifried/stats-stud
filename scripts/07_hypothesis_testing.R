### stats-stud: hypothesis testing (parametric — pooled independent t-test)
### Purpose: Test whether test preparation course completion significantly affects
###          composite (overall) academic performance
### Justification: See temp/Hypothesis_Test_Justification.docx

source("scripts/00_setup.R")

# ============================================================================
# LOAD DATA
# ============================================================================

message("Loading data...")

df <- utils::read.csv(paths$public_clean, stringsAsFactors = FALSE)

# Create composite overall score (average of three subjects)
df$overall_score <- rowMeans(
  df[, c("math_score", "reading_score", "writing_score")],
  na.rm = TRUE
)

# Clean test preparation group label
df$test_prep <- trimws(tolower(df$test_preparation_course))

# Split into two groups and remove NAs
group_completed <- df$overall_score[df$test_prep == "completed"]
group_completed <- group_completed[!is.na(group_completed)]

group_none <- df$overall_score[df$test_prep == "none"]
group_none <- group_none[!is.na(group_none)]

message("Group sizes — completed: ", length(group_completed), " | none: ", length(group_none))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

message("Computing descriptive statistics by group...")

desc_results <- data.frame(
  group  = c("completed", "none"),
  n      = c(length(group_completed), length(group_none)),
  mean   = c(round(mean(group_completed), 2), round(mean(group_none), 2)),
  median = c(round(median(group_completed), 2), round(median(group_none), 2)),
  sd     = c(round(sd(group_completed), 2), round(sd(group_none), 2)),
  stringsAsFactors = FALSE
)
save_table_csv(desc_results, file.path(paths$tables, "hypothesis_descriptives.csv"))

mean_diff <- round(mean(group_completed) - mean(group_none), 2)
message("Mean difference (completed - none): ", mean_diff, " pts")

# ============================================================================
# ASSUMPTION 1: NORMALITY (Anderson-Darling per group)
# ============================================================================

message("Checking normality (Anderson-Darling)...")

ad_completed <- nortest::ad.test(group_completed)
ad_none      <- nortest::ad.test(group_none)

normality_results <- data.frame(
  group        = c("completed", "none"),
  n            = c(length(group_completed), length(group_none)),
  ad_statistic = c(round(ad_completed$statistic, 3), round(ad_none$statistic, 3)),
  p_value      = c(ad_completed$p.value, ad_none$p.value),
  stringsAsFactors = FALSE
)
save_table_csv(normality_results, file.path(paths$tables, "hypothesis_normality.csv"))

message("AD completed: stat = ", round(ad_completed$statistic, 3),
        " | p = ", format.pval(ad_completed$p.value, digits = 3))
message("AD none:      stat = ", round(ad_none$statistic, 3),
        " | p = ", format.pval(ad_none$p.value, digits = 3))
message("-> Both groups pass normality (CLT also applies: n >> 30 per group)")

# ============================================================================
# ASSUMPTION 2: HOMOGENEITY OF VARIANCE (Levene's test)
# ============================================================================

message("Checking homogeneity of variance (Levene's test)...")

levene_result <- car::leveneTest(overall_score ~ test_prep, data = df)

levene_f <- round(levene_result$`F value`[1], 3)
levene_p <- levene_result$`Pr(>F)`[1]

message("Levene's F = ", levene_f, " | p = ", format.pval(levene_p, digits = 3))
message("-> p > 0.05: Equal variances confirmed — pooled t-test justified (var.equal = TRUE)")

# ============================================================================
# HYPOTHESES
# ============================================================================

# H0: mu_completed = mu_none
#     No significant difference in mean overall score between groups
#
# H1: mu_completed > mu_none   [one-tailed, directional]
#     Students who completed test preparation score significantly higher
#
# WHY t-test and NOT ANOVA?
#   Exactly 2 groups -> t-test is the correct, direct method.
#   ANOVA with 2 groups is mathematically equivalent but unnecessary.
#
# WHY pooled and NOT Welch?
#   Levene's p = 0.090 > 0.05 -> equal variances -> pooled version is justified.

# ============================================================================
# PARAMETRIC TEST: INDEPENDENT SAMPLES t-TEST (POOLED)
# ============================================================================

message("Running independent samples t-test (pooled)...")

t_result <- t.test(
  group_completed,
  group_none,
  alternative = "greater",  # directional: completed > none
  var.equal   = TRUE,        # pooled: justified by Levene's p = 0.090
  conf.level  = 0.95
)

# Effect size: Cohen's d (using cohens_d_ci from 00_setup.R)
d_result <- cohens_d_ci(group_completed, group_none)

# Save results
ttest_results <- data.frame(
  test         = "Independent t-test (pooled)",
  variable     = "overall_score",
  group1       = "completed",
  group2       = "none",
  n1           = length(group_completed),
  n2           = length(group_none),
  mean1        = round(mean(group_completed), 2),
  mean2        = round(mean(group_none), 2),
  mean_diff    = mean_diff,
  t_stat       = round(t_result$statistic, 3),
  df           = t_result$parameter,
  p_value      = t_result$p.value,
  cohens_d     = round(d_result$cohens_d, 3),
  effect_size  = d_result$interpretation,
  ci_low       = round(t_result$conf.int[1], 2),
  ci_high      = round(t_result$conf.int[2], 2),
  decision     = ifelse(t_result$p.value < 0.05, "Reject H0", "Fail to reject H0"),
  stringsAsFactors = FALSE
)
save_table_csv(ttest_results, file.path(paths$tables, "hypothesis_ttest_result.csv"))

message("T-test complete. p = ", format.pval(t_result$p.value, digits = 3))
message("Cohen's d = ", round(d_result$cohens_d, 3), " (", d_result$interpretation, " effect)")

# ============================================================================
# SUMMARY
# ============================================================================

message("")
message(paste(rep("=", 60), collapse = ""))
message("HYPOTHESIS TESTING SUMMARY")
message(paste(rep("=", 60), collapse = ""))
message("")
message("Test:          Independent samples t-test (pooled)")
message("Variable:      overall_score (composite of math, reading, writing)")
message("Groups:        completed (n = ", length(group_completed),
        ") vs. none (n = ", length(group_none), ")")
message("Mean diff:     ", mean_diff, " pts  (completed higher)")
message(sprintf("Result:        t(%.0f) = %.3f, p = %s",
                t_result$parameter,
                t_result$statistic,
                format.pval(t_result$p.value, digits = 3)))
message(sprintf("95%% CI:        [%.2f, %.2f]", t_result$conf.int[1], t_result$conf.int[2]))
message(sprintf("Cohen's d:     %.3f (%s effect)", d_result$cohens_d, d_result$interpretation))
message("Decision:      ", ifelse(t_result$p.value < 0.05, "Reject H0", "Fail to reject H0"))
message("")
message("All results saved to outputs/tables/")
message(paste(rep("=", 60), collapse = ""))

# ============================================================================
# non parametric testing code:
# ============================================================================
