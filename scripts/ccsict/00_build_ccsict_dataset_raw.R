### stats-stud: build CCSICT raw merged dataset
### Purpose: Merge survey and score files while retaining item-level preparedness data

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Building CCSICT raw merged dataset...")

if (!file.exists(paths$ccsict_raw)) {
  stop("Missing survey file: ", paths$ccsict_raw)
}

if (!file.exists(paths$ccsict_scores)) {
  stop("Missing score file: ", paths$ccsict_scores)
}

survey_raw <- utils::read.csv(paths$ccsict_raw, check.names = FALSE, stringsAsFactors = FALSE)
scores_raw <- utils::read.csv(paths$ccsict_scores, stringsAsFactors = FALSE)

consent_col <- names(survey_raw)[2]
student_id_col <- names(survey_raw)[3]
sex_col <- names(survey_raw)[4]
program_col <- names(survey_raw)[5]
item_cols <- 6:21
prep_method_col <- names(survey_raw)[22]

consent_ok <- grepl("understand and voluntarily consent", survey_raw[[consent_col]], ignore.case = TRUE) &
  !grepl("do not understand", survey_raw[[consent_col]], ignore.case = TRUE)

survey <- survey_raw[consent_ok, , drop = FALSE]

parse_likert <- function(x) {
  x <- trimws(x)
  out <- sub("^([1-5]).*$", "\\1", x)
  out[!grepl("^[1-5]", x)] <- NA_character_
  as.numeric(out)
}

clean_sex <- function(x) {
  x <- trimws(tolower(x))
  dplyr::case_when(
    x == "male" ~ "Male",
    x == "female" ~ "Female",
    x %in% c("prefer not to say", "prefer not say") ~ "Prefer not to say",
    TRUE ~ x
  )
}

clean_program <- function(x) {
  x <- toupper(trimws(x))
  dplyr::case_when(
    grepl("DSA", x, fixed = TRUE) ~ "BSDSA",
    grepl("CS", x, fixed = TRUE) ~ "BSCS",
    TRUE ~ x
  )
}

clean_prep_method <- function(x) {
  x <- trimws(x)
  dplyr::case_when(
    grepl("^AI-Assisted", x) ~ "AI-Assisted",
    grepl("^Did not prepare", x) ~ "Did not prepare",
    grepl("^Hybrid", x) ~ "Hybrid",
    grepl("^Traditional", x) ~ "Traditional",
    TRUE ~ x
  )
}

item_map <- data.frame(
  source_col = item_cols,
  variable = c(
    "cognitive_notes",
    "cognitive_examples",
    "cognitive_flowcharts",
    "cognitive_own_words",
    "metacognitive_check_flow",
    "metacognitive_self_question",
    "metacognitive_analyze_strategy",
    "metacognitive_alternative_logic",
    "planning_pace_review",
    "planning_set_goals",
    "planning_identify_needs",
    "planning_organize_time",
    "readiness_judge_understanding",
    "readiness_strengths_weaknesses",
    "readiness_review_goals",
    "readiness_easier_solution"
  ),
  scale = c(
    rep("Cognitive Preparation", 4),
    rep("Metacognitive Monitoring", 4),
    rep("Planning and Time Management", 4),
    rep("Readiness Evaluation", 4)
  ),
  stringsAsFactors = FALSE
)

item_values <- lapply(item_map$source_col, function(idx) parse_likert(survey[[idx]]))
item_values <- as.data.frame(item_values, stringsAsFactors = FALSE)
names(item_values) <- item_map$variable

survey_clean <- data.frame(
  timestamp = survey[[1]],
  student_id = trimws(survey[[student_id_col]]),
  sex = clean_sex(survey[[sex_col]]),
  program = clean_program(survey[[program_col]]),
  prep_method = clean_prep_method(survey[[prep_method_col]]),
  item_values,
  stringsAsFactors = FALSE
)

scores_clean <- data.frame(
  student_id = trimws(scores_raw$student_num),
  score_program = clean_program(scores_raw$program),
  lec_score = as.numeric(scores_raw$lec_score),
  lab_score = as.numeric(scores_raw$lab_score),
  stringsAsFactors = FALSE
) |>
  dplyr::distinct(student_id, .keep_all = TRUE)

raw_dataset <- dplyr::left_join(survey_clean, scores_clean, by = "student_id") |>
  dplyr::arrange(program, student_id)

raw_dataset$lab_completed <- ifelse(is.na(raw_dataset$lab_score), NA_integer_, ifelse(raw_dataset$lab_score > 0, 1L, 0L))
raw_dataset$lab_completed_label <- ifelse(raw_dataset$lab_completed == 1L, "Completed", "Not completed")

cognitive_cols <- item_map$variable[item_map$scale == "Cognitive Preparation"]
metacognitive_cols <- item_map$variable[item_map$scale == "Metacognitive Monitoring"]
planning_cols <- item_map$variable[item_map$scale == "Planning and Time Management"]
readiness_cols <- item_map$variable[item_map$scale == "Readiness Evaluation"]

raw_dataset$cognitive_preparation <- rowMeans(raw_dataset[, cognitive_cols], na.rm = FALSE)
raw_dataset$metacognitive_monitoring <- rowMeans(raw_dataset[, metacognitive_cols], na.rm = FALSE)
raw_dataset$planning_time_management <- rowMeans(raw_dataset[, planning_cols], na.rm = FALSE)
raw_dataset$readiness_evaluation <- rowMeans(raw_dataset[, readiness_cols], na.rm = FALSE)
raw_dataset$preparedness_score <- rowMeans(raw_dataset[, item_map$variable], na.rm = FALSE)
raw_dataset$respondent_id <- sprintf("R%02d", seq_len(nrow(raw_dataset)))

raw_dataset <- raw_dataset |>
  dplyr::select(
    respondent_id,
    student_id,
    timestamp,
    sex,
    program,
    prep_method,
    dplyr::all_of(item_map$variable),
    cognitive_preparation,
    metacognitive_monitoring,
    planning_time_management,
    readiness_evaluation,
    preparedness_score,
    lec_score,
    lab_score,
    lab_completed,
    lab_completed_label
  )

save_table_csv(raw_dataset, paths$ccsict_dataset_raw)

message("Raw dataset saved to: ", paths$ccsict_dataset_raw)
