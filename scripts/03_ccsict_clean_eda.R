### stats-stud: CCSICT data cleaning and EDA (simplified student version)
### Purpose: Clean CCSICT survey data and perform EDA

source("scripts/00_setup.R")

# ============================================================================
# CHECK IF CCSICT DATA EXISTS
# ============================================================================

message("Checking for CCSICT data...")

if (!file.exists(paths$ccsict_raw)) {
  message("")
  message("======================================")
  message("CCSICT DATA NOT FOUND")
  message("======================================")
  message("Please collect survey responses and save to:")
  message("  data/ccsict/ccsict_responses_raw.csv")
  message("")
  message("Template not created - please manually add your data.")
  message("======================================")
  quit(save = "no", status = 0)
}

# ============================================================================
# LOAD AND CLEAN DATA
# ============================================================================

message("Loading CCSICT data...")

raw <- utils::read.csv(paths$ccsict_raw, stringsAsFactors = FALSE)

message("Loaded ", nrow(raw), " rows")

# Rename columns to match public dataset
# CCSICT: gender, parental_education, financial_support, test_preparation, 
#         data_mining_score, reading_score, writing_score
# Public: gender, parental_level_of_education, lunch, test_preparation_course,
#         math_score, reading_score, writing_score

# Try to rename if columns exist
if ("gender" %in% names(raw)) {
  raw$gender <- trimws(tolower(raw$gender))
}

if ("parental_education" %in% names(raw)) {
  raw$parental_level_of_education <- trimws(tolower(raw$parental_education))
}

if ("financial_support" %in% names(raw)) {
  raw$lunch <- trimws(tolower(raw$financial_support))
}

if ("test_preparation" %in% names(raw)) {
  raw$test_preparation_course <- trimws(tolower(raw$test_preparation))
}

if ("data_mining_score" %in% names(raw)) {
  raw$math_score <- as.numeric(raw$data_mining_score)
}

if ("reading_score" %in% names(raw)) {
  raw$reading_score <- as.numeric(raw$reading_score)
}

if ("writing_score" %in% names(raw)) {
  raw$writing_score <- as.numeric(raw$writing_score)
}

# ============================================================================
# QUALITY CHECKS
# ============================================================================

message("Running quality checks...")

# Check for missing values
math_missing <- sum(is.na(raw$math_score))
reading_missing <- sum(is.na(raw$reading_score))
writing_missing <- sum(is.na(raw$writing_score))

# Check for duplicates
n_duplicates <- sum(duplicated(raw))

# Save quality report
quality_check <- data.frame(
  check = c("total_rows", "math_missing", "reading_missing", "writing_missing", "duplicates"),
  value = c(nrow(raw), math_missing, reading_missing, writing_missing, n_duplicates),
  stringsAsFactors = FALSE
)
save_table_csv(quality_check, file.path(paths$tables, "ccsict_quality_check.csv"))

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

message("Computing descriptive statistics...")

# Calculate stats for each score
score_cols <- c("math_score", "reading_score", "writing_score")

# Only calculate if data exists
if (nrow(raw) > 0) {
  
  desc_stats <- data.frame()
  
  for (sc in score_cols) {
    if (sc %in% names(raw)) {
      x <- raw[[sc]]
      x <- x[!is.na(x)]
      
      if (length(x) > 0) {
        desc_stats <- rbind(desc_stats, data.frame(
          variable = sc,
          n = length(x),
          mean = mean(x),
          sd = sd(x),
          min = min(x),
          max = max(x),
          median = median(x),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  save_table_csv(desc_stats, file.path(paths$tables, "ccsict_descriptive_stats.csv"))
}

# ============================================================================
# CORRELATION ANALYSIS
# ============================================================================

message("Computing correlations...")

if ("reading_score" %in% names(raw) && "writing_score" %in% names(raw)) {
  r <- cor.test(raw$reading_score, raw$writing_score, method = "pearson", use = "complete.obs")
  
  cor_result <- data.frame(
    test = "Pearson Correlation",
    var1 = "reading_score",
    var2 = "writing_score",
    r = r$estimate,
    p_value = r$p.value,
    stringsAsFactors = FALSE
  )
  save_table_csv(cor_result, file.path(paths$tables, "ccsict_correlation.csv"))
}

# ============================================================================
# SUMMARY
# ============================================================================

message("")
message("======================================")
message("CCSICT DATA SUMMARY")
message("======================================")
message("Total responses: ", nrow(raw))
message("Missing math scores: ", math_missing)
message("Missing reading scores: ", reading_missing)
message("Missing writing scores: ", writing_missing)
message("")
message("Results saved to outputs/tables/")
message("======================================")
