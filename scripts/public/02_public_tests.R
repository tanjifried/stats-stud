### stats-stud: hypothesis tests (simplified student version)
### Purpose: Conduct hypothesis testing with both parametric and non-parametric tests

stats_stud_context <- "public"
source("scripts/00_setup.R")

# ============================================================================
# LOAD DATA
# ============================================================================

message("Loading data...")

df <- utils::read.csv(paths$public_clean, stringsAsFactors = FALSE)

# Clean variables
df$test_prep <- trimws(tolower(df$test_preparation_course))
df$parent_edu <- trimws(tolower(df$parental_level_of_education))
df$gender <- trimws(tolower(df$gender))
df$math_score <- as.numeric(df$math_score)
df$reading_score <- as.numeric(df$reading_score)
df$writing_score <- as.numeric(df$writing_score)

# ============================================================================
# PARAMETRIC TEST 1: Welch t-test (Test Prep vs Math Score)
# ============================================================================

message("Running Welch t-test...")

# Get two groups
group_completed <- df$math_score[df$test_prep == "completed"]
group_completed <- group_completed[!is.na(group_completed)]

group_none <- df$math_score[df$test_prep == "none"]
group_none <- group_none[!is.na(group_none)]

# Run t-test
t_result <- t.test(group_completed, group_none, var.equal = FALSE)

# Calculate effect size (Cohen's d)
mean_diff <- mean(group_completed) - mean(group_none)
pooled_sd <- sqrt(((length(group_completed)-1)*sd(group_completed)^2 + (length(group_none)-1)*sd(group_none)^2) / (length(group_completed)+length(group_none)-2))
cohens_d <- mean_diff / pooled_sd

# Save results
ttest_results <- data.frame(
  test = "Welch t-test",
  variable = "math_score",
  group1 = "completed",
  group2 = "none",
  n1 = length(group_completed),
  n2 = length(group_none),
  mean1 = mean(group_completed),
  mean2 = mean(group_none),
  t_stat = t_result$statistic,
  df = t_result$parameter,
  p_value = t_result$p.value,
  cohens_d = cohens_d,
  stringsAsFactors = FALSE
)
save_table_csv(ttest_results, file.path(paths$tables, "public_ttest_result.csv"))

message("T-test complete. p = ", format.pval(t_result$p.value, digits = 3))

# ============================================================================
# PARAMETRIC TEST 2: One-way ANOVA (Parent Education vs Math Score)
# ============================================================================

message("Running ANOVA...")

# Run ANOVA
anova_model <- aov(math_score ~ parent_edu, data = df)
anova_summary <- summary(anova_model)

# Get results
f_stat <- anova_summary[[1]]$"F value"[1]
p_value_anova <- anova_summary[[1]]$"Pr(>F)"[1]

# Save
anova_results <- data.frame(
  test = "One-way ANOVA",
  variable = "math_score",
  predictor = "parental_level_of_education",
  F_statistic = f_stat,
  df1 = anova_summary[[1]]$Df[1],
  df2 = anova_summary[[1]]$Df[2],
  p_value = p_value_anova,
  stringsAsFactors = FALSE
)
save_table_csv(anova_results, file.path(paths$tables, "public_anova_result.csv"))

message("ANOVA complete. p = ", format.pval(p_value_anova, digits = 3))

# ============================================================================
# NON-PARAMETRIC TEST 1: Kruskal-Wallis (Parent Education vs Math Score)
# ============================================================================

message("Running Kruskal-Wallis...")

kw_result <- kruskal.test(math_score ~ parent_edu, data = df)

kw_results <- data.frame(
  test = "Kruskal-Wallis",
  variable = "math_score",
  predictor = "parental_level_of_education",
  H_statistic = kw_result$statistic,
  df = kw_result$parameter,
  p_value = kw_result$p.value,
  stringsAsFactors = FALSE
)
save_table_csv(kw_results, file.path(paths$tables, "public_kruskal_result.csv"))

message("Kruskal-Wallis complete. p = ", format.pval(kw_result$p.value, digits = 3))

# ============================================================================
# NON-PARAMETRIC TEST 2: Mann-Whitney U (Test Prep vs Math Score)
# ============================================================================

message("Running Mann-Whitney U...")

mw_result <- wilcox.test(group_completed, group_none, exact = FALSE)

mw_results <- data.frame(
  test = "Mann-Whitney U",
  variable = "math_score",
  group1 = "completed",
  group2 = "none",
  W_statistic = mw_result$statistic,
  p_value = mw_result$p.value,
  stringsAsFactors = FALSE
)
save_table_csv(mw_results, file.path(paths$tables, "public_mannwhitney_result.csv"))

message("Mann-Whitney complete. p = ", format.pval(mw_result$p.value, digits = 3))

# ============================================================================
# NON-PARAMETRIC TEST 3: Spearman Correlation (Reading vs Writing)
# ============================================================================

message("Running Spearman correlation...")

spearman_result <- cor.test(df$reading_score, df$writing_score, method = "spearman", exact = FALSE)

spearman_results <- data.frame(
  test = "Spearman Correlation",
  variable1 = "reading_score",
  variable2 = "writing_score",
  rho = spearman_result$estimate,
  p_value = spearman_result$p.value,
  stringsAsFactors = FALSE
)
save_table_csv(spearman_results, file.path(paths$tables, "public_spearman_result.csv"))

message("Spearman correlation complete. rho = ", round(spearman_result$estimate, 3))

# ============================================================================
# SUMMARY
# ============================================================================

message("")
message("======================================")
message("HYPOTHESIS TESTING SUMMARY")
message("======================================")
message("")
message("Parametric Tests:")
message("  1. Welch t-test: p = ", format.pval(t_result$p.value, digits = 3))
message("  2. One-way ANOVA: p = ", format.pval(p_value_anova, digits = 3))
message("")
message("Non-parametric Tests:")
message("  3. Kruskal-Wallis: p = ", format.pval(kw_result$p.value, digits = 3))
message("  4. Mann-Whitney U: p = ", format.pval(mw_result$p.value, digits = 3))
message("  5. Spearman: rho = ", round(spearman_result$estimate, 3), ", p = ", format.pval(spearman_result$p.value, digits = 3))
message("")
message("All results saved to outputs/tables/")
message("======================================")
