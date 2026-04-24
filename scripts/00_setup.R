### stats-stud: project setup (enhanced)
### Purpose: Initialize project environment with required packages and utilities
### Author: Enhanced version with improved statistical methods
### Date: 2024
###
### Changes from original:
### - Added comprehensive statistical packages for robust assumption testing
### - Included here::here() for reproducible paths
### - Added utility functions for normality assessment and effect sizes
### - Implemented proper package version management recommendations

# Clear environment safely
clean_env <- function() {
  # Preserve run context values needed before setup is reloaded.
  keep <- character(0)

  if ("renv" %in% loadedNamespaces()) {
    keep <- c(keep, "renv")
  }

  if (exists("stats_stud_context", envir = .GlobalEnv, inherits = FALSE)) {
    keep <- c(keep, "stats_stud_context")
  }

  rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
}
clean_env()

# Set reproducible options
options(
  stringsAsFactors = FALSE,
  scipen = 999,  # Suppress scientific notation
  digits = 4,    # Consistent numeric display
  warn = 1       # Immediate warnings
)

set.seed(42)

# Required packages with descriptions:
# - dplyr/tidyr/ggplot2: Core tidyverse for data manipulation and visualization
# - moments: Skewness and kurtosis for normality assessment
# - nortest: Anderson-Darling test (better for large samples than Shapiro-Wilk)
# - car: Levene's test for homogeneity of variance
# - pwr: Power analysis for sample size adequacy
# - effsize: Effect size calculations with confidence intervals
# - here: Portable file paths (reproducibility best practice)
required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "moments",
  "nortest",
  "car",
  "pwr",
  "effsize",
  "here"
)

# Check and install missing packages
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = TRUE)
}

# Load all packages with suppressed startup messages
invisible(suppressPackageStartupMessages(
  lapply(required_packages, library, character.only = TRUE)
))

# Define portable paths using here::here()
# This ensures scripts work regardless of working directory
script_context <- if (exists("stats_stud_context", inherits = TRUE)) {
  get("stats_stud_context", inherits = TRUE)
} else {
  NULL
}

if (identical(script_context, "public")) {
  figures_dir <- here("outputs", "public", "figures")
  tables_dir <- here("outputs", "public", "tables")
} else if (identical(script_context, "ccsict")) {
  figures_dir <- here("outputs", "ccsict", "figures")
  tables_dir <- here("outputs", "ccsict", "tables")
} else {
  figures_dir <- here("outputs", "figures")
  tables_dir <- here("outputs", "tables")
}

paths <- list(
  public_raw = here("data", "public", "StudentsPerformance.csv"),
  public_clean = here("data", "public", "StudentsPerformance_clean.csv"),
  ccsict_raw = here("data", "ccsict", "ccsict_survey.csv"),
  ccsict_scores = here("data", "ccsict", "program_scores.csv"),
  ccsict_dataset_raw = here("data", "ccsict", "ccsict-dataset-raw.csv"),
  ccsict_dataset = here("data", "ccsict", "ccsict-dataset.csv"),
  ccsict_clean = here("data", "ccsict", "ccsict-dataset-clean.csv"),
  figures = figures_dir,
  tables = tables_dir
)

# Create output directories if they don't exist
dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)

# Utility: Save data frame to CSV with consistent formatting
#' @param df Data frame to save
#' @param file Output file path
#' @param row.names Whether to include row names (default: FALSE)
#' @param na String to use for NA values (default: "")
save_table_csv <- function(df, file, row.names = FALSE, na = "") {
  utils::write.csv(df, file = file, row.names = row.names, na = na)
}

# Utility: Save ggplot with consistent settings
#' @param p ggplot object
#' @param filename Output filename (will be placed in figures directory)
#' @param width Width in inches (default: 7)
#' @param height Height in inches (default: 5)
#' @param dpi Resolution (default: 160)
#' @param device Device type (default: "png")
save_plot <- function(p, filename, width = 7, height = 5, dpi = 160, device = "png") {
  ggplot2::ggsave(
    filename = file.path(paths$figures, filename),
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    device = device
  )
}

# Utility: Comprehensive normality assessment
#' Assess normality using multiple methods suitable for large samples
#' @param x Numeric vector
#' @param var_name Variable name for labeling
#' @return List with test results and recommendations
assess_normality <- function(x, var_name = "variable") {
  x_clean <- stats::na.omit(x)
  n <- length(x_clean)
  
  # Descriptive statistics for normality
  skew <- moments::skewness(x_clean)
  kurt <- moments::kurtosis(x_clean)  # Excess kurtosis (normal = 0)
  
  # Anderson-Darling test (better for large samples than Shapiro-Wilk)
  # Shapiro-Wilk is unreliable for n > 1000
  ad_test <- nortest::ad.test(x_clean)
  
  # Determine if normal based on multiple criteria
  # Reference: Hair et al. (2019) - skewness < |2|, kurtosis < |7| for normal-ish
  is_normal <- abs(skew) < 2 && abs(kurt) < 7 && ad_test$p.value > 0.05
  
  result <- list(
    variable = var_name,
    n = n,
    mean = mean(x_clean),
    sd = sd(x_clean),
    median = median(x_clean),
    skewness = skew,
    kurtosis = kurt,
    ad_statistic = ad_test$statistic,
    ad_pvalue = ad_test$p.value,
    is_normal = is_normal,
    interpretation = ifelse(is_normal, 
                           "Approximately normal (AD p > 0.05, |skew| < 2, |kurtosis| < 7)",
                           "Non-normal (check AD test, skewness, or kurtosis)"),
    recommendation = ifelse(n > 1000,
                         "Large sample: CLT applies, parametric tests robust regardless of normality",
                         ifelse(is_normal, 
                               "Parametric tests appropriate",
                               "Consider non-parametric alternatives or transformation"))
  )
  
  return(result)
}

# Utility: Calculate effect size with confidence interval
#' Calculate Cohen's d with confidence interval
#' @param x Group 1 data
#' @param y Group 2 data
#' @param conf.level Confidence level (default: 0.95)
#' @return Data frame with effect size and CI
cohens_d_ci <- function(x, y, conf.level = 0.95) {
  # Remove NA
  x <- na.omit(x)
  y <- na.omit(y)
  
  n1 <- length(x)
  n2 <- length(y)
  m1 <- mean(x)
  m2 <- mean(y)
  sd1 <- sd(x)
  sd2 <- sd(y)
  
  # Pooled SD
  sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  
  # Cohen's d
  d <- (m1 - m2) / sp
  
  # Hedges' g (small sample correction)
  g <- d * (1 - (3 / (4 * (n1 + n2) - 9)))
  
  # Standard error of d (approximate)
  se_d <- sqrt((n1 + n2) / (n1 * n2) + (d^2) / (2 * (n1 + n2)))
  
  # Confidence interval
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  ci_low <- d - z * se_d
  ci_high <- d + z * se_d
  
  # Interpretation
  interpretation <- ifelse(abs(d) < 0.2, "Negligible",
                          ifelse(abs(d) < 0.5, "Small",
                                 ifelse(abs(d) < 0.8, "Medium", "Large")))
  
  data.frame(
    cohens_d = d,
    hedges_g = g,
    ci_low = ci_low,
    ci_high = ci_high,
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

# Utility: Detect outliers using multiple methods
#' Detect outliers using IQR and modified Z-score
#' @param x Numeric vector
#' @return Data frame with outlier flags
detect_outliers <- function(x) {
  x_clean <- na.omit(x)
  
  # IQR method
  q1 <- quantile(x_clean, 0.25)
  q3 <- quantile(x_clean, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Modified Z-score (using median, more robust)
  med <- median(x_clean)
  mad <- median(abs(x_clean - med))
  modified_z <- 0.6745 * (x_clean - med) / mad
  
  # Summary
  list(
    n_total = length(x),
    n_missing = sum(is.na(x)),
    iqr_outliers = sum(x_clean < lower_bound | x_clean > upper_bound),
    iqr_lower = lower_bound,
    iqr_upper = upper_bound,
    modz_outliers = sum(abs(modified_z) > 3.5),
    modz_threshold = 3.5,
    extreme_outliers = sum(x_clean < (q1 - 3 * iqr) | x_clean > (q3 + 3 * iqr))
  )
}

# Utility: Format p-values for reporting
#' Format p-values for publication
#' @param p P-value
#' @param digits Number of decimal places
#' @return Formatted string
format_pvalue <- function(p, digits = 3) {
  ifelse(p < 0.001, "< 0.001",
         ifelse(p < 0.01, sprintf("%.3f", p),
                sprintf("%.3f", p)))
}

message("Setup complete (enhanced with robust statistical methods).")
message("Loaded packages: ", paste(required_packages, collapse = ", "))
