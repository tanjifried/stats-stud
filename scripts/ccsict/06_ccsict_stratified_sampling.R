### stats-stud: CCSICT stratified sampling
### Purpose: Generate stratified sample datasets from the clean CCSICT dataset

stats_stud_context <- "ccsict"
source("scripts/00_setup.R")

message("Loading CCSICT clean dataset for stratified sampling...")

if (!file.exists(paths$ccsict_clean)) {
  stop("Missing clean dataset: ", paths$ccsict_clean, ". Run scripts/ccsict/02_build_ccsict_dataset_clean.R first.")
}

dataset <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)

if (sum(duplicated(dataset$respondent_id)) > 0) {
  stop("Duplicate respondent_id values found in ", paths$ccsict_clean)
}

sample_output_dir <- here::here("data", "ccsict", "samples")
dir.create(sample_output_dir, recursive = TRUE, showWarnings = FALSE)

target_sample_n <- min(30L, nrow(dataset))
base_seed <- 42L

strata_designs <- list(
  program = c("program")
)

allocation_methods <- c("proportional", "equal")

build_stratum_labels <- function(df, cols) {
  if (length(cols) == 1) {
    return(as.character(df[[cols]]))
  }

  apply(df[, cols, drop = FALSE], 1, paste, collapse = " | ")
}

allocate_counts <- function(strata_sizes, target_n, method) {
  target_n <- min(as.integer(target_n), sum(strata_sizes))
  n_strata <- length(strata_sizes)

  if (method == "proportional") {
    raw_targets <- strata_sizes / sum(strata_sizes) * target_n
    counts <- pmin(floor(raw_targets), strata_sizes)
    priority <- order(raw_targets - floor(raw_targets), strata_sizes, decreasing = TRUE)
  } else {
    raw_targets <- rep(target_n / n_strata, n_strata)
    counts <- pmin(floor(raw_targets), strata_sizes)
    priority <- order(strata_sizes - counts, strata_sizes, decreasing = TRUE)
  }

  remaining <- target_n - sum(counts)

  while (remaining > 0) {
    assigned <- FALSE

    for (idx in priority) {
      if (counts[idx] < strata_sizes[idx]) {
        counts[idx] <- counts[idx] + 1L
        remaining <- remaining - 1L
        assigned <- TRUE
      }

      if (remaining == 0) {
        break
      }
    }

    if (!assigned) {
      break
    }
  }

  data.frame(
    requested_n = round(raw_targets, 4),
    sampled_n = as.integer(counts),
    capacity_limited = counts < ceiling(raw_targets),
    stringsAsFactors = FALSE
  )
}

sample_rows <- function(strata_info, stratum_label_all, sample_counts, seed) {
  set.seed(seed)
  sampled_idx <- integer(0)

  for (i in seq_len(nrow(strata_info))) {
    n_take <- sample_counts$sampled_n[i]

    if (n_take == 0) {
      next
    }

    idx <- which(strata_info$stratum_label[i] == stratum_label_all)
    sampled_idx <- c(sampled_idx, sample(idx, size = n_take, replace = FALSE))
  }

  sampled_idx
}

design_rows <- list()
allocation_rows <- list()

for (design_name in names(strata_designs)) {
  strata_cols <- strata_designs[[design_name]]
  stratum_label_all <- build_stratum_labels(dataset, strata_cols)

  strata_tbl <- data.frame(
    stratum_label = unique(stratum_label_all),
    stringsAsFactors = FALSE
  )

  strata_tbl$available_n <- vapply(
    strata_tbl$stratum_label,
    function(label) sum(stratum_label_all == label),
    integer(1)
  )
  for (method_idx in seq_along(allocation_methods)) {
    method <- allocation_methods[[method_idx]]
    design_seed <- base_seed + which(names(strata_designs) == design_name) * 100L + method_idx
    sample_counts <- allocate_counts(strata_tbl$available_n, target_sample_n, method)

    strata_info <- cbind(
      data.frame(
        design = design_name,
        strata_columns = paste(strata_cols, collapse = ","),
        allocation_method = method,
        seed = design_seed,
        stringsAsFactors = FALSE
      ),
      strata_tbl[, c("stratum_label", "available_n")],
      sample_counts
    )
    sampled_idx <- sample_rows(strata_info, stratum_label_all, sample_counts, design_seed)
    sampled_df <- dataset[sampled_idx, , drop = FALSE]
    sampled_df$stratum_label <- build_stratum_labels(sampled_df, strata_cols)
    sampled_df$allocation_method <- method
    sampled_df$sampling_design <- design_name

    sampled_df <- sampled_df |>
      dplyr::select(
        respondent_id,
        sex,
        program,
        preparation_mode,
        preparedness_score,
        preparedness_level,
        lecture_score,
        lab_completed,
        stratum_label,
        allocation_method,
        sampling_design
      ) |>
      dplyr::arrange(stratum_label, respondent_id)

    if (sum(duplicated(sampled_df$respondent_id)) > 0) {
      stop("Duplicate respondent_id values found in sampled dataset for ", design_name, " / ", method)
    }

    output_file <- file.path(
      sample_output_dir,
      paste0("ccsict-clean-stratified-", design_name, "-", method, ".csv")
    )
    save_table_csv(sampled_df, output_file)

    design_rows[[length(design_rows) + 1L]] <- data.frame(
      design = design_name,
      strata_columns = paste(strata_cols, collapse = ","),
      allocation_method = method,
      seed = design_seed,
      population_n = nrow(dataset),
      target_sample_n = target_sample_n,
      actual_sample_n = nrow(sampled_df),
      n_strata = nrow(strata_info),
      min_stratum_n = min(strata_info$available_n),
      max_stratum_n = max(strata_info$available_n),
      capacity_limited = any(strata_info$capacity_limited),
      output_file = output_file,
      stringsAsFactors = FALSE
    )

    allocation_rows[[length(allocation_rows) + 1L]] <- strata_info |>
      dplyr::select(
        design,
        strata_columns,
        allocation_method,
        seed,
        stratum_label,
        available_n,
        requested_n,
        sampled_n,
        capacity_limited
      )
  }
}

design_summary <- dplyr::bind_rows(design_rows)
allocation_summary <- dplyr::bind_rows(allocation_rows)

save_table_csv(
  design_summary,
  file.path(paths$tables, "ccsict_stratified_sampling_designs.csv")
)
save_table_csv(
  allocation_summary,
  file.path(paths$tables, "ccsict_stratified_sampling_summary.csv")
)

message("\n============================================================")
message("CCSICT STRATIFIED SAMPLING COMPLETE")
message("============================================================")
message("Clean dataset source: ", paths$ccsict_clean)
message("Sample outputs saved to: ", sample_output_dir)
message("Sampling tables saved to: ", paths$tables)
message("Target sample size per design: ", target_sample_n)
message("Designs generated: ", nrow(design_summary))
message("============================================================")
