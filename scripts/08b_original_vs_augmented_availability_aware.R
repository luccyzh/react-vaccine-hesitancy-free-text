# Compare original and augmented reason models using availability-aware handling of structured-option missingness.

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)
project_root <- "D:/projects/hda_2026/han"
input_rds <- file.path(
  project_root,
  "outputs",
  "07b_cluster_level_descriptive",
  "df_with_augmented_reasons_novel_themes_and_clusters.rds"
)
existing_step08_csv <- file.path(
  project_root,
  "outputs",
  "08_follow_up_vaccination_models",
  "tables",
  "reason_theme_primary_plot_data.csv"
)
output_dir <- file.path(
  project_root,
  "outputs",
  "08b_original_vs_augmented_availability_aware"
)
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")
run_dir   <- file.path(output_dir, "modelmaker_runs")
qc_dir    <- file.path(output_dir, "qc")
for (d in c(output_dir, table_dir, model_dir, run_dir, qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
modelmaker_candidates <- c(
  "D:/packages/OverReact/R/bulk_run_models_ORs_RDs.R",
  "D:/projects/OverReact/R/bulk_run_models_ORs_RDs.R",
  "D:/shared_working_folder/projects/OverReact/R/bulk_run_models_ORs_RDs.R",
  "D:/projects/vaccine_hesitancy_2024/code/OverReact/R/bulk_run_models_ORs_RDs.R"
)
modelmaker_file <- modelmaker_candidates[file.exists(modelmaker_candidates)][1]
required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "MASS",
  "mgcv"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing package(s): ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})
if (is.na(modelmaker_file) || length(modelmaker_file) == 0) {
  stop(
    "Could not find bulk_run_models_ORs_RDs.R.\nChecked:\n",
    paste(modelmaker_candidates, collapse = "\n")
  )
}
message("Loading ModelMakerMultiRD from: ", modelmaker_file)
source(modelmaker_file)
if (!exists("ModelMakerMultiRD", mode = "function")) {
  stop("ModelMakerMultiRD() was not created after sourcing the ModelMaker file.")
}
reason_labels <- c(
  "1"  = "Worried about side-effects",
  "2"  = "See how well the vaccine works",
  "3"  = "Worried about long-term health effects",
  "4"  = "Worried about travel to vaccination centre",
  "5"  = "Difficult to get to vaccination centre",
  "6"  = "COVID-19 is not a personal risk",
  "7"  = "Existing health condition",
  "8"  = "Against vaccines in general",
  "9"  = "Do not think the vaccine will work for me",
  "10" = "Worried the vaccine will give me COVID-19",
  "11" = "Fear of pain or needles",
  "12" = "Worried about feeling ill",
  "13" = "Already had COVID-19, vaccination not needed",
  "14" = "Pregnant or breastfeeding",
  "15" = "COVID-19 impact is exaggerated",
  "16" = "Do not trust vaccine developers",
  "17" = "If others get vaccinated, I do not need to",
  "18" = "Doses limited, others need the vaccine more",
  "19" = "Other",
  "20" = "Prefer not to say",
  "21" = "Concerned about fertility",
  "22" = "Worried about allergic reaction",
  "23" = "Bad reaction to previous vaccines"
)
cluster_group <- c(
  "1"  = "Cluster 1: Effectiveness and side-effect concerns",
  "2"  = "Cluster 1: Effectiveness and side-effect concerns",
  "3"  = "Cluster 1: Effectiveness and side-effect concerns",
  "12" = "Cluster 1: Effectiveness and side-effect concerns",
  "4"  = "Cluster 2: Access and travel barriers",
  "5"  = "Cluster 2: Access and travel barriers",
  "6"  = "Cluster 3: Low perceived need for vaccination and distrust",
  "13" = "Cluster 3: Low perceived need for vaccination and distrust",
  "15" = "Cluster 3: Low perceived need for vaccination and distrust",
  "16" = "Cluster 3: Low perceived need for vaccination and distrust",
  "7"  = "Cluster 4: Personal health and adverse-reaction concerns",
  "22" = "Cluster 4: Personal health and adverse-reaction concerns",
  "23" = "Cluster 4: Personal health and adverse-reaction concerns",
  "8"  = "Cluster 5: General vaccine opposition and specific fears",
  "9"  = "Cluster 5: General vaccine opposition and specific fears",
  "10" = "Cluster 5: General vaccine opposition and specific fears",
  "11" = "Cluster 5: General vaccine opposition and specific fears",
  "17" = "Cluster 5: General vaccine opposition and specific fears",
  "14" = "Cluster 6: Context-specific and newly identified concerns",
  "20" = "Cluster 6: Context-specific and newly identified concerns",
  "18" = "Cluster 7: Fertility and vaccination-need considerations",
  "21" = "Cluster 7: Fertility and vaccination-need considerations"
)
comparison_order <- c(
  1,2,3,12,
  4,5,
  6,13,15,16,
  7,22,23,
  8,9,10,11,17,
  14,20,
  18,21
)
comparison_reason_numbers <- comparison_order
original_vars  <- paste0("vaccrufuse3_", comparison_reason_numbers)
augmented_vars <- paste0("augmented_existing_", comparison_reason_numbers)
is_positive_flag <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  !is.na(x) & x_chr %in% c("1", "true", "t", "yes", "y")
}
as_binary_factor_na_to_zero <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  x_num <- ifelse(!is.na(x_num) & x_num == 1, 1L, 0L)
  factor(x_num, levels = c(0, 1))
}
as_binary_numeric_preserve_na <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(x_chr))
  out[x_chr %in% c("0","false","f","no","n")] <- 0L
  out[x_chr %in% c("1","true","t","yes","y")] <- 1L
  suppressWarnings({
    x_num <- as.numeric(x_chr)
    out[is.na(out) & !is.na(x_num) & x_num == 0] <- 0L
    out[is.na(out) & !is.na(x_num) & x_num == 1] <- 1L
  })
  out
}
make_availability_aware_augmented <- function(original_raw, augmented_raw) {
  org <- as_binary_numeric_preserve_na(original_raw)
  aug_num <- suppressWarnings(as.numeric(as.character(augmented_raw)))
  aug_pos <- !is.na(aug_num) & aug_num == 1
  out <- rep(NA_integer_, length(org))
  out[org == 1] <- 1L
  out[org == 0 & aug_pos] <- 1L
  out[org == 0 & !aug_pos] <- 0L
  out[is.na(org) & aug_pos] <- 1L
  out[is.na(org) & !aug_pos] <- NA_integer_
  factor(out, levels = c(0, 1))
}
select_final_adjustment <- function(plot_output, expected_stage = 2) {
  adj <- suppressWarnings(as.numeric(plot_output$adjustment))
  available <- sort(unique(adj[!is.na(adj)]))
  if (length(available) == 0) {
    stop("No valid adjustment stage in ModelMaker plot_output.")
  }
  selected <- if (expected_stage %in% available) {
    expected_stage
  } else {
    max(available)
  }
  list(selected = selected, available = available)
}
run_models <- function(dat, predictors, pretty_labels, run_name) {
  run_path <- file.path(run_dir, run_name)
  dir.create(run_path, recursive = TRUE, showWarnings = FALSE)
  cov_name_list <- c(
    pretty_labels,
    age_group_named = "Age group",
    sex = "Sex"
  )
  options(modelmaker.name_map = cov_name_list)
  message(
    "\nRunning ", run_name,
    "\nPredictors: ", length(predictors),
    "\nAdjustment: age_group_named + sex"
  )
  out <- ModelMakerMultiRD(
    dat = dat,
    list_of_variables_of_interest = predictors,
    incremental = TRUE,
    include_crude = TRUE,
    outcome = "subs_vaxxed",
    ncores = 1,
    savepath = run_path,
    cov_name_list = cov_name_list,
    joint_adjustment_vars = c("age_group_named", "sex"),
    include_rd = FALSE,
    glm_control = stats::glm.control(maxit = 500),
    n_sim = 50
  )
  if (is.null(out$df_output) || is.null(out$plot_output)) {
    stop("ModelMaker output incomplete for: ", run_name)
  }
  saveRDS(out, file.path(model_dir, paste0(run_name, ".rds")))
  write_csv(out$df_output,
            file.path(table_dir, paste0(run_name, "_df_output_raw.csv")))
  write_csv(out$plot_output,
            file.path(table_dir, paste0(run_name, "_plot_output_raw.csv")))
  out
}
standardise_output <- function(model_obj, prefix, method_label) {
  pdat <- model_obj$plot_output
  required <- c("Variable","Category","OR","Lower","Upper","adjustment")
  miss <- setdiff(required, names(pdat))
  if (length(miss) > 0) {
    stop("plot_output missing: ", paste(miss, collapse = ", "))
  }
  stage <- select_final_adjustment(pdat, expected_stage = 2)
  pdat <- pdat %>%
    mutate(adjustment_numeric = suppressWarnings(as.numeric(adjustment))) %>%
    filter(adjustment_numeric == stage$selected)
  label_to_num <- setNames(names(reason_labels), unname(reason_labels))
  raw_var <- as.character(pdat$Variable)
  raw_cat <- as.character(pdat$Category)
  num <- rep(NA_character_, nrow(pdat))
  num[grepl(paste0("^", prefix, "_"), raw_var)] <-
    sub(paste0("^", prefix, "_"), "",
        raw_var[grepl(paste0("^", prefix, "_"), raw_var)])
  idx <- is.na(num) & raw_var %in% names(label_to_num)
  num[idx] <- unname(label_to_num[raw_var[idx]])
  idx <- is.na(num) & raw_cat %in% names(label_to_num)
  num[idx] <- unname(label_to_num[raw_cat[idx]])
  idx2 <- is.na(num) & grepl(paste0("^", prefix, "_"), raw_cat)
  num[idx2] <- sub(
    paste0("^", prefix, "_"),
    "",
    raw_cat[idx2]
  )
  pdat$reason_num <- suppressWarnings(as.integer(num))
  pdat <- pdat %>%
    filter(reason_num %in% comparison_reason_numbers) %>%
    mutate(
      predictor_raw = paste0("vaccrufuse3_", reason_num),
      augmented_predictor_raw = paste0("augmented_existing_", reason_num),
      predictor_label = unname(reason_labels[as.character(reason_num)]),
      cluster_group = unname(cluster_group[as.character(reason_num)]),
      row_order = match(reason_num, comparison_order),
      model_method = method_label,
      sig_status = ifelse(
        Lower <= 1 & Upper >= 1,
        "CI crosses 1",
        "CI excludes 1"
      ),
      or_ci = sprintf("%.2f (%.2f–%.2f)", OR, Lower, Upper)
    ) %>%
    arrange(row_order)
  pdat
}
if (!file.exists(input_rds)) {
  stop("Missing downstream RDS: ", input_rds)
}
df <- readRDS(input_rds)
required_vars <- c(
  "analysis_4_vax_any_hes_or_refused_vs_vaxxed",
  "nhs_vaccine_status",
  "age_group_named",
  "sex",
  original_vars,
  augmented_vars
)
missing_vars <- setdiff(required_vars, names(df))
if (length(missing_vars) > 0) {
  stop("Missing variables:\n", paste(missing_vars, collapse = "\n"))
}
definition4_flag <- is_positive_flag(
  df$analysis_4_vax_any_hes_or_refused_vs_vaxxed
)
df_mod <- df %>%
  filter(definition4_flag) %>%
  filter(!is.na(nhs_vaccine_status)) %>%
  filter(nhs_vaccine_status != "Vaccine recorded before REACT survey") %>%
  mutate(
    subs_vaxxed = case_when(
      nhs_vaccine_status == "NHS vaccination recorded" ~ 0L,
      TRUE ~ 1L
    ),
    age_group_named = factor(age_group_named),
    sex = factor(sex)
  )
cohort_qc <- data.frame(
  measure = c(
    "hesitant_cohort",
    "followup_cohort",
    "subsequently_vaccinated",
    "persistent_hesitancy",
    "persistent_hesitancy_percent"
  ),
  value = c(
    sum(definition4_flag),
    nrow(df_mod),
    sum(df_mod$subs_vaxxed == 0),
    sum(df_mod$subs_vaxxed == 1),
    100 * mean(df_mod$subs_vaxxed == 1)
  )
)
write_csv(cohort_qc, file.path(qc_dir, "cohort_qc.csv"))
print(cohort_qc)
if (sum(definition4_flag) != 37982) {
  warning("Hesitant cohort is not 37,982.")
}
if (nrow(df_mod) != 24229) {
  warning("Follow-up cohort is not 24,229.")
}
for (j in comparison_reason_numbers) {
  orig_nm <- paste0("vaccrufuse3_", j)
  aug_nm  <- paste0("augmented_existing_", j)
  df_mod[[paste0("orig_current_", j)]] <-
    as_binary_factor_na_to_zero(df_mod[[orig_nm]])
  df_mod[[paste0("aug_current_", j)]] <-
    as_binary_factor_na_to_zero(df_mod[[aug_nm]])
  org_keep <- as_binary_numeric_preserve_na(df_mod[[orig_nm]])
  df_mod[[paste0("orig_available_", j)]] <-
    factor(org_keep, levels = c(0,1))
  df_mod[[paste0("aug_available_", j)]] <-
    make_availability_aware_augmented(
      original_raw = df_mod[[orig_nm]],
      augmented_raw = df_mod[[aug_nm]]
    )
}
orig_current_vars   <- paste0("orig_current_", comparison_reason_numbers)
aug_current_vars    <- paste0("aug_current_", comparison_reason_numbers)
orig_available_vars <- paste0("orig_available_", comparison_reason_numbers)
aug_available_vars  <- paste0("aug_available_", comparison_reason_numbers)
pretty_current_orig <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  orig_current_vars
)
pretty_current_aug <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  aug_current_vars
)
pretty_available_orig <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  orig_available_vars
)
pretty_available_aug <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  aug_available_vars
)
denom_qc <- lapply(comparison_reason_numbers, function(j) {
  o_cur <- df_mod[[paste0("orig_current_", j)]]
  a_cur <- df_mod[[paste0("aug_current_", j)]]
  o_av  <- df_mod[[paste0("orig_available_", j)]]
  a_av  <- df_mod[[paste0("aug_available_", j)]]
  data.frame(
    reason_num = j,
    reason_label = unname(reason_labels[as.character(j)]),
    n_followup = nrow(df_mod),
    current_original_nonmissing = sum(!is.na(o_cur)),
    current_augmented_nonmissing = sum(!is.na(a_cur)),
    availability_original_nonmissing = sum(!is.na(o_av)),
    availability_augmented_nonmissing = sum(!is.na(a_av)),
    availability_original_positive =
      sum(as.character(o_av) == "1", na.rm = TRUE),
    availability_augmented_positive =
      sum(as.character(a_av) == "1", na.rm = TRUE),
    added_positive_when_original_missing =
      sum(is.na(o_av) & as.character(a_av) == "1", na.rm = TRUE)
  )
})
denom_qc <- do.call(rbind, denom_qc)
write_csv(
  denom_qc,
  file.path(qc_dir, "reason_specific_denominator_and_backfill_qc.csv")
)
orig_current_model <- run_models(
  df_mod,
  orig_current_vars,
  pretty_current_orig,
  "original_current_method_age_sex"
)
aug_current_model <- run_models(
  df_mod,
  aug_current_vars,
  pretty_current_aug,
  "augmented_current_method_age_sex"
)
orig_current <- standardise_output(
  orig_current_model,
  prefix = "orig_current",
  method_label = "Original structured — current project NA→0 method"
)
aug_current <- standardise_output(
  aug_current_model,
  prefix = "aug_current",
  method_label = "Augmented — current project NA→0 method"
)
orig_available_model <- run_models(
  df_mod,
  orig_available_vars,
  pretty_available_orig,
  "original_availability_aware_age_sex"
)
aug_available_model <- run_models(
  df_mod,
  aug_available_vars,
  pretty_available_aug,
  "augmented_availability_aware_age_sex"
)
orig_available <- standardise_output(
  orig_available_model,
  prefix = "orig_available",
  method_label = "Original structured — availability-aware"
)
aug_available <- standardise_output(
  aug_available_model,
  prefix = "aug_available",
  method_label = "Augmented — availability-aware"
)
minimal_cols <- c(
  "reason_num",
  "predictor_raw",
  "augmented_predictor_raw",
  "predictor_label",
  "cluster_group",
  "row_order",
  "OR",
  "Lower",
  "Upper",
  "or_ci",
  "sig_status",
  "model_method"
)
write_csv(
  orig_current[, intersect(minimal_cols, names(orig_current))],
  file.path(table_dir, "current_method_original_primary_MINIMAL.csv")
)
write_csv(
  aug_current[, intersect(minimal_cols, names(aug_current))],
  file.path(table_dir, "current_method_augmented_primary_MINIMAL.csv")
)
write_csv(
  orig_available[, intersect(minimal_cols, names(orig_available))],
  file.path(table_dir, "availability_aware_original_primary_MINIMAL.csv")
)
write_csv(
  aug_available[, intersect(minimal_cols, names(aug_available))],
  file.path(table_dir, "availability_aware_augmented_primary_MINIMAL.csv")
)
join_minimal <- function(orig, aug, method_name) {
  o <- orig %>%
    select(
      reason_num,
      predictor_label,
      cluster_group,
      row_order,
      OR_original = OR,
      Lower_original = Lower,
      Upper_original = Upper
    )
  a <- aug %>%
    select(
      reason_num,
      OR_augmented = OR,
      Lower_augmented = Lower,
      Upper_augmented = Upper
    )
  out <- left_join(o, a, by = "reason_num") %>%
    mutate(
      method = method_name,
      OR_change = OR_augmented - OR_original,
      logOR_change = log(OR_augmented) - log(OR_original)
    ) %>%
    arrange(row_order)
  out
}
current_compare <- join_minimal(
  orig_current,
  aug_current,
  "Current project NA→0 method"
)
available_compare <- join_minimal(
  orig_available,
  aug_available,
  "Option-availability-aware sensitivity"
)
write_csv(
  current_compare,
  file.path(table_dir, "current_method_original_vs_augmented_primary.csv")
)
write_csv(
  available_compare,
  file.path(table_dir, "availability_aware_original_vs_augmented_primary.csv")
)
if (file.exists(existing_step08_csv)) {
  old <- read_csv(existing_step08_csv, show_col_types = FALSE)
  old$reason_num <- NA_integer_
  if ("predictor_raw" %in% names(old)) {
    x <- as.character(old$predictor_raw)
    idx <- grepl("^augmented_existing_[0-9]+$", x)
    old$reason_num[idx] <- as.integer(
      sub("^augmented_existing_", "", x[idx])
    )
  }
  if ("predictor_label" %in% names(old)) {
    label_to_num <- setNames(
      as.integer(names(reason_labels)),
      unname(reason_labels)
    )
    idx <- is.na(old$reason_num) &
      as.character(old$predictor_label) %in% names(label_to_num)
    old$reason_num[idx] <- unname(
      label_to_num[as.character(old$predictor_label[idx])]
    )
  }
  if ("adjustment" %in% names(old)) {
    adj_num <- suppressWarnings(as.numeric(old$adjustment))
    if (any(adj_num == 2, na.rm = TRUE)) {
      old <- old[adj_num == 2, , drop = FALSE]
    }
  }
  if (all(c("OR","Lower","Upper") %in% names(old))) {
    old_small <- old %>%
      filter(reason_num %in% comparison_reason_numbers) %>%
      select(
        reason_num,
        OR_step08 = OR,
        Lower_step08 = Lower,
        Upper_step08 = Upper
      )
    check <- aug_current %>%
      select(
        reason_num,
        OR_rerun = OR,
        Lower_rerun = Lower,
        Upper_rerun = Upper
      ) %>%
      left_join(old_small, by = "reason_num") %>%
      mutate(
        abs_OR_difference = abs(OR_rerun - OR_step08),
        abs_Lower_difference = abs(Lower_rerun - Lower_step08),
        abs_Upper_difference = abs(Upper_rerun - Upper_step08)
      )
    write_csv(
      check,
      file.path(qc_dir, "current_augmented_vs_existing_step08_check.csv")
    )
    cat("\nMaximum absolute OR difference vs existing Step 08: ",
        max(check$abs_OR_difference, na.rm = TRUE), "\n", sep = "")
  }
} else {
  warning(
    "Existing Step 08 plot CSV not found, so exact augmented re-run comparison ",
    "could not be automated."
  )
}
sensitivity_change <- current_compare %>%
  select(
    reason_num,
    predictor_label,
    row_order,
    OR_original_current = OR_original,
    OR_augmented_current = OR_augmented
  ) %>%
  left_join(
    available_compare %>%
      select(
        reason_num,
        OR_original_availability = OR_original,
        OR_augmented_availability = OR_augmented
      ),
    by = "reason_num"
  ) %>%
  mutate(
    original_OR_difference =
      OR_original_availability - OR_original_current,
    augmented_OR_difference =
      OR_augmented_availability - OR_augmented_current
  ) %>%
  arrange(row_order)
write_csv(
  sensitivity_change,
  file.path(qc_dir, "current_vs_availability_aware_OR_comparison.csv")
)
cat("\n============================================================\n")
cat("STEP 08B ALIGNED ORIGINAL-vs-AUGMENTED COMPARISON COMPLETED\n")
cat("============================================================\n")
cat("SCIENTIFIC PRIMARY comparison method:\n")
cat("  Original structured reasons PRESERVE NA where unavailable/missing.\n")
cat("  Augmented reasons preserve that unknown status unless free text provides\n")
cat("  positive evidence for the reason.\n\n")
cat("CURRENT STEP-08 branch:\n")
cat("  NA->0 is retained ONLY as a reproduction / implementation QC check.\n\n")
cat("KEY OUTPUT FILES:\n")
cat("1) MAIN: ", file.path(table_dir,
    "availability_aware_original_vs_augmented_primary.csv"), "\n", sep = "")
cat("2) ", file.path(table_dir,
    "availability_aware_original_primary_MINIMAL.csv"), "\n", sep = "")
cat("3) ", file.path(table_dir,
    "availability_aware_augmented_primary_MINIMAL.csv"), "\n", sep = "")
cat("4) QC: ", file.path(qc_dir,
    "reason_specific_denominator_and_backfill_qc.csv"), "\n", sep = "")
cat("5) REPRODUCTION QC: ", file.path(table_dir,
    "current_method_original_vs_augmented_primary.csv"), "\n", sep = "")
cat("6) ", file.path(qc_dir,
    "current_augmented_vs_existing_step08_check.csv"), "\n", sep = "")
cat("============================================================\n")
