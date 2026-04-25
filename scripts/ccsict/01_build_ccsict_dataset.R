### stats-stud: build CCSICT analysis dataset
### Purpose: Create a leaner CCSICT dataset for descriptive and inferential analysis

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Building CCSICT analysis dataset...")

if (!file.exists(paths$ccsict_dataset_raw)) {
  stop("Missing raw dataset: ", paths$ccsict_dataset_raw, ". Run scripts/ccsict/00_build_ccsict_dataset_raw.R first.")
}

raw_dataset <- utils::read.csv(paths$ccsict_dataset_raw, stringsAsFactors = FALSE)

collapse_preparation_mode <- function(x) {
  dplyr::case_when(
    x %in% c("AI-Assisted", "Hybrid") ~ "with_ai",
    x %in% c("Traditional", "Did not prepare") ~ "without_ai",
    TRUE ~ NA_character_
  )
}

preparedness_level <- dplyr::case_when(
  raw_dataset$preparedness_score <= 1.80 ~ "Very Low",
  raw_dataset$preparedness_score <= 2.60 ~ "Low",
  raw_dataset$preparedness_score <= 3.40 ~ "Moderate",
  raw_dataset$preparedness_score <= 4.20 ~ "High",
  raw_dataset$preparedness_score <= 5.00 ~ "Very High",
  TRUE ~ NA_character_
)

dataset <- raw_dataset |>
  dplyr::transmute(
    respondent_id,
    student_id,
    sex,
    program,
    preparation_mode = collapse_preparation_mode(prep_method),
    preparedness_score = round(preparedness_score, 4),
    preparedness_level = preparedness_level,
    lec_score = as.numeric(lec_score),
    lab_score = as.numeric(lab_score),
    lab_completed = as.integer(lab_completed)
  )

save_table_csv(dataset, paths$ccsict_dataset)

message("Analysis dataset saved to: ", paths$ccsict_dataset)
