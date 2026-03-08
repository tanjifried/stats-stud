### stats-stud: comparative analysis (simplified student version)
### Purpose: Compare results between public dataset and CCSICT dataset

source("scripts/00_setup.R")

# ============================================================================
# LOAD BOTH DATASETS
# ============================================================================

message("Loading datasets...")

# Load public dataset
df_public <- utils::read.csv(paths$public_clean, stringsAsFactors = FALSE)
message("Public dataset: ", nrow(df_public), " rows")

# Check if CCSICT data exists
if (!file.exists(paths$ccsict_clean)) {
  message("")
  message("======================================")
  message("CCSICT DATA NOT FOUND")
  message("======================================")
  message("Please run script 03 first to create CCSICT data.")
  message("======================================")
  quit(save = "no", status = 0)
}

# Load CCSICT dataset
df_ccsict <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)
message("CCSICT dataset: ", nrow(df_ccsict), " rows")

# ============================================================================
# COMPARE DESCRIPTIVE STATS
# ============================================================================

message("Comparing descriptive statistics...")

# Compare means
public_math_mean <- mean(df_public$math_score, na.rm = TRUE)
ccsict_math_mean <- mean(df_ccsict$math_score, na.rm = TRUE)

# Save comparison
comparison <- data.frame(
  metric = c("Sample Size (Public)", "Sample Size (CCSICT)", 
             "Math Mean (Public)", "Math Mean (CCSICT)",
             "Mean Difference"),
  value = c(nrow(df_public), nrow(df_ccsict),
            public_math_mean, ccsict_math_mean,
            public_math_mean - ccsict_math_mean),
  stringsAsFactors = FALSE
)
save_table_csv(comparison, file.path(paths$tables, "comparison_summary.csv"))

# ============================================================================
# COMPARE T-TEST RESULTS
# ============================================================================

message("Comparing t-test results...")

# Clean public data
df_public$test_prep <- trimws(tolower(df_public$test_preparation_course))

# Public t-test
pub_completed <- df_public$math_score[df_public$test_prep == "completed"]
pub_completed <- pub_completed[!is.na(pub_completed)]
pub_none <- df_public$math_score[df_public$test_prep == "none"]
pub_none <- pub_none[!is.na(pub_none)]

pub_t <- t.test(pub_completed, pub_none, var.equal = FALSE)

# CCSICT t-test (if data exists)
if ("test_preparation_course" %in% names(df_ccsict)) {
  df_ccsict$test_prep <- trimws(tolower(df_ccsict$test_preparation_course))
  
  ccs_completed <- df_ccsict$math_score[df_ccsict$test_prep == "completed"]
  ccs_completed <- ccs_completed[!is.na(ccs_completed)]
  ccs_none <- df_ccsict$math_score[df_ccsict$test_prep == "none"]
  ccs_none <- ccs_none[!is.na(ccs_none)]
  
  if (length(ccs_completed) > 1 && length(ccs_none) > 1) {
    ccs_t <- t.test(ccs_completed, ccs_none, var.equal = FALSE)
    
    ttest_compare <- data.frame(
      dataset = c("Public", "CCSICT"),
      t_statistic = c(pub_t$statistic, ccs_t$statistic),
      p_value = c(pub_t$p.value, ccs_t$p.value),
      stringsAsFactors = FALSE
    )
    save_table_csv(ttest_compare, file.path(paths$tables, "comparison_ttest.csv"))
  }
}

# ============================================================================
# SUMMARY
# ============================================================================

message("")
message("======================================")
message("COMPARISON SUMMARY")
message("======================================")
message("")
message("Public Dataset:")
message("  n = ", nrow(df_public))
message("  Math Mean = ", round(public_math_mean, 2))
message("  t-test p = ", format.pval(pub_t$p.value, digits = 3))
message("")
message("CCSICT Dataset:")
message("  n = ", nrow(df_ccsict))
message("  Math Mean = ", round(ccsict_math_mean, 2))
message("")
message("Results saved to outputs/tables/")
message("======================================")
