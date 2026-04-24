### stats-stud: build CCSICT clean analysis dataset
### Purpose: Create a de-identified, paper-ready CCSICT dataset

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Building CCSICT clean dataset...")

if (!file.exists(paths$ccsict_dataset)) {
  stop("Missing analysis dataset: ", paths$ccsict_dataset, ". Run scripts/ccsict/01_build_ccsict_dataset.R first.")
}

dataset <- utils::read.csv(paths$ccsict_dataset, stringsAsFactors = FALSE)

expected_level <- dplyr::case_when(
  dataset$preparedness_score <= 1.80 ~ "Very Low",
  dataset$preparedness_score <= 2.60 ~ "Low",
  dataset$preparedness_score <= 3.40 ~ "Moderate",
  dataset$preparedness_score <= 4.20 ~ "High",
  dataset$preparedness_score <= 5.00 ~ "Very High",
  TRUE ~ NA_character_
)

if (any(is.na(dataset$preparedness_score))) {
  stop("preparedness_score contains missing values.")
}

if (any(dataset$preparedness_score < 1 | dataset$preparedness_score > 5, na.rm = TRUE)) {
  stop("preparedness_score is outside the expected 1 to 5 range.")
}

if (sum(duplicated(dataset$respondent_id)) > 0) {
  stop("Duplicate respondent_id values found in ", paths$ccsict_dataset)
}

if (!all(expected_level == dataset$preparedness_level)) {
  stop("preparedness_level does not match preparedness_score bands.")
}

clean_dataset <- dataset |>
  dplyr::transmute(
    respondent_id,
    sex,
    program,
    preparation_method = prep_method,
    preparedness_score = round(as.numeric(preparedness_score), 4),
    preparedness_level,
    lecture_score = as.numeric(lec_score),
    lab_completed = ifelse(as.integer(lab_completed) == 1L, "Completed", "Not completed")
  )

save_table_csv(clean_dataset, paths$ccsict_clean)

message("Clean dataset saved to: ", paths$ccsict_clean)
