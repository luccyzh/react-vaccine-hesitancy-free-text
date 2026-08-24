# Run final reason-, theme- and cluster-level NHS-linked outcome models with availability-aware augmented reasons.

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)
project_root <- "D:/projects/hda_2026/han"
input_rds <- file.path(
  project_root,
  "outputs",
  "07b_cluster_level_descriptive",
  "df_with_augmented_reasons_novel_themes_and_clusters.rds"
)
output_dir <- file.path(
  project_root,
  "outputs",
  "08_FINAL_ALL_MODELS_CORRECTED"
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
required_packages <- c("dplyr", "tidyr", "readr", "stringr", "MASS", "mgcv")
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
source(modelmaker_file)
source('D:/packages/OverReact/R/modeling_helpers.R')
source('D:/packages/OverReact/R/specify_decimal.R')
if (!exists("ModelMakerMultiRD", mode = "function")) {
  stop("ModelMakerMultiRD() was not created.")
}
if (!exists("specifyDecimal", mode = "function")) {
  stop('Missing specifyDecimal()')
}
if (!exists(".react_detect_outcome_family", mode = "function")) {
  stop('Missing.react_detect_outcome_family()')
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
novel_labels <- c(
  novel_brand_preference = "Vaccine brand preference and choice",
  novel_access_availability_dosing = "Access, availability and dosing arrangements",
  novel_ethical_religious_animal = "Ethical, religious and animal-related concerns",
  novel_government_distrust_autonomy = "Government distrust, coercion and loss of autonomy",
  novel_mrna_gene_technology = "Concerns about mRNA and gene-therapy technology",
  novel_uncertainty_decision_pending = "Uncertainty or decision not yet made"
)
cluster_labels <- c(
  cluster_1 = "Effectiveness and side-effect concerns",
  cluster_2 = "Access and travel barriers",
  cluster_3 = "Low perceived need for vaccination and distrust",
  cluster_4 = "Personal health and adverse-reaction concerns",
  cluster_5 = "General vaccine opposition and specific fears",
  cluster_6 = "Context-specific and newly identified concerns",
  cluster_7 = "Fertility and vaccination-need considerations"
)
is_positive_flag <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  !is.na(x) & x_chr %in% c("1","true","t","yes","y")
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
as_binary_factor_preserve_na <- function(x) {
  factor(as_binary_numeric_preserve_na(x), levels = c(0,1))
}
as_binary_factor_zero_one <- function(x) {
  z <- as_binary_numeric_preserve_na(x)
  z[is.na(z)] <- 0L
  factor(z, levels = c(0,1))
}
make_availability_aware_augmented <- function(original_raw, augmented_raw) {
  org <- as_binary_numeric_preserve_na(original_raw)
  aug <- as_binary_numeric_preserve_na(augmented_raw)
  aug_pos <- !is.na(aug) & aug == 1
  out <- rep(NA_integer_, length(org))
  out[org == 1] <- 1L
  out[org == 0 & aug_pos] <- 1L
  out[org == 0 & !aug_pos] <- 0L
  out[is.na(org) & aug_pos] <- 1L
  out[is.na(org) & !aug_pos] <- NA_integer_
  factor(out, levels = c(0,1))
}
select_final_adjustment <- function(plot_output, expected_stage) {
  adj <- suppressWarnings(as.numeric(plot_output$adjustment))
  available <- sort(unique(adj[!is.na(adj)]))
  if (length(available) == 0) {
    stop("No valid adjustment stages in ModelMaker output.")
  }
  selected <- if (expected_stage %in% available) {
    expected_stage
  } else {
    max(available)
  }
  list(selected = selected, available = available)
}
run_models <- function(dat, predictors, labels, covariates, run_name) {
  run_path <- file.path(run_dir, run_name)
  dir.create(run_path, recursive = TRUE, showWarnings = FALSE)
  cov_name_list <- c(
    labels,
    age_group_named = "Age group",
    sex = "Sex",
    round_midpoint = "Date of survey round"
  )
  options(modelmaker.name_map = cov_name_list)
  out <- ModelMakerMultiRD(
    dat = dat,
    list_of_variables_of_interest = predictors,
    incremental = TRUE,
    include_crude = TRUE,
    outcome = "subs_vaxxed",
    ncores = 1,
    savepath = run_path,
    cov_name_list = cov_name_list,
    joint_adjustment_vars = covariates,
    include_rd = FALSE,
    glm_control = stats::glm.control(maxit = 500),
    n_sim = 50
  )
  if (is.null(out$df_output) || is.null(out$plot_output)) {
    stop("ModelMaker output incomplete for ", run_name)
  }
  saveRDS(out, file.path(model_dir, paste0(run_name, ".rds")))
  write_csv(out$df_output,
            file.path(table_dir, paste0(run_name, "_df_output_raw.csv")))
  write_csv(out$plot_output,
            file.path(table_dir, paste0(run_name, "_plot_output_raw.csv")))
  out
}
standardise <- function(model_obj, predictor_map, expected_stage, analysis_label) {
  pdat <- model_obj$plot_output
  required <- c("Variable","Category","OR","Lower","Upper","adjustment")
  miss <- setdiff(required, names(pdat))
  if (length(miss) > 0) {
    stop("plot_output missing: ", paste(miss, collapse = ", "))
  }
  stage <- select_final_adjustment(pdat, expected_stage)
  normalise_name<-function(x){
    x<-tolower(trimws(as.character(x)))
    x<-gsub('[^a-z0-9]+','_',x)
    x<-gsub('^_+|_+$','',x)
    x
  }
  pdat <- pdat %>%
    mutate(adjustment_numeric = suppressWarnings(as.numeric(adjustment))) %>%
    filter(adjustment_numeric == stage$selected,as.character(Category)=='1',!is.na(OR))
  map_names <- names(predictor_map)
  pretty_lookup <- setNames(
    map_names,
    normalise_name(unname(predictor_map))
  )
  variable_normalised <- normalise_name(pdat$Variable)
  pred <- rep(NA_character_, nrow(pdat))
   idx <- is.na(pred) & variable_normalised %in% map_names
   pred[idx] <- variable_normalised[idx]
   idx <- is.na(pred) & variable_normalised %in% names(pretty_lookup)
 pred[idx] <- unname(pretty_lookup[variable_normalised[idx]])
   if('term_raw' %in% names(pdat)){
    term_raw <-normalise_name(pdat$term_raw)
     idx<-is.na(pred) & term_raw %in% map_names
     pred[idx] <- term_raw[idx]
    term_without_level <-sub('1$','',term_raw)
     idx<-is.na(pred) & term_without_level %in% map_names
     pred[idx] <- term_without_level[idx]
  }
  if(any(is.na(pred))){
    stop(
      'Could not match predictors:',
      paste(
        unique(pdat$Variable[is.na(pred)]),
        collapse = ';'
      )
    )
  }
  pdat$predictor <- pred
  pdat$predictor_label <- unname(predictor_map[pdat$predictor])
  pdat$analysis <- analysis_label
  pdat$sig_status <- ifelse(
    pdat$Lower <= 1 & pdat$Upper >= 1,
    "CI crosses 1",
    "CI excludes 1"
  )
  pdat$or_ci <- sprintf("%.2f (%.2f-%.2f)", pdat$OR, pdat$Lower, pdat$Upper)
  pdat 
}
if (!file.exists(input_rds)) stop("Missing input RDS: ", input_rds)
df <- readRDS(input_rds)
required_vars <- c(
  "analysis_4_vax_any_hes_or_refused_vs_vaxxed",
  "linkagesf",
  "nhs_vaccine_status",
  "age_group_named",
  "sex",
  "study_round",
  "date"
)
missing_core <- setdiff(required_vars, names(df))
if (length(missing_core) > 0) {
  stop("Missing core variables:\n", paste(missing_core, collapse = "\n"))
}
df$definition4_flag <- is_positive_flag(
  df$analysis_4_vax_any_hes_or_refused_vs_vaxxed
)
df_mod <- df %>%
  filter(!is.na(linkagesf) & linkagesf == 1) %>%
  filter(definition4_flag) %>%
  filter(!is.na(nhs_vaccine_status)) %>%
  filter(nhs_vaccine_status != "Vaccine recorded before REACT survey") %>%
  mutate(
    subs_vaxxed = case_when(
      nhs_vaccine_status == "NHS vaccination recorded" ~ 0L,
      nhs_vaccine_status == "No NHS vaccine on record" ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(subs_vaxxed))
if (!inherits(df_mod$date, "Date")) {
  df_mod$date <- as.Date(df_mod$date)
}
df_mod <- df_mod %>%
  group_by(study_round) %>%
  mutate(
    round_midpoint = as.factor(
      as.character(
        stats::median(date, na.rm = TRUE)
      )
    )
  ) %>%
  ungroup()
if ("2021-01-27" %in% levels(df_mod$round_midpoint)) {
  df_mod$round_midpoint <- relevel(df_mod$round_midpoint, ref = "2021-01-27")
}
df_mod$age_group_named <- factor(df_mod$age_group_named)
df_mod$sex <- factor(df_mod$sex)
cohort_qc <- data.frame(
  measure = c(
    "hesitant_cohort",
    "followup_cohort",
    "subsequently_vaccinated",
    "persistent_hesitancy",
    "persistent_hesitancy_percent"
  ),
  value = c(
    sum(df$definition4_flag),
    nrow(df_mod),
    sum(df_mod$subs_vaxxed == 0),
    sum(df_mod$subs_vaxxed == 1),
    100 * mean(df_mod$subs_vaxxed == 1)
  )
)
write_csv(cohort_qc, file.path(qc_dir, "cohort_qc.csv"))
print(cohort_qc)
if (sum(df$definition4_flag) != 37982) warning("Hesitant cohort != 37,982")
if (nrow(df_mod) != 24229) warning("Follow-up cohort != 24,229")
original_temp_vars <- character(0)
augmented_temp_vars <- character(0)
for (j in comparison_reason_numbers) {
  orig_nm <- paste0("vaccrufuse3_", j)
  aug_nm  <- paste0("augmented_existing_", j)
  if (!(orig_nm %in% names(df_mod))) {
    stop("Missing original reason variable: ", orig_nm)
  }
  if (!(aug_nm %in% names(df_mod))) {
    stop("Missing augmented reason variable: ", aug_nm)
  }
  orig_temp <- paste0("orig_reason_", j)
  aug_temp  <- paste0("aug_reason_", j)
  df_mod[[orig_temp]] <- as_binary_factor_preserve_na(df_mod[[orig_nm]])
  df_mod[[aug_temp]] <- make_availability_aware_augmented(
    original_raw = df_mod[[orig_nm]],
    augmented_raw = df_mod[[aug_nm]]
  )
  original_temp_vars <- c(original_temp_vars, orig_temp)
  augmented_temp_vars <- c(augmented_temp_vars, aug_temp)
}
original_map <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  original_temp_vars
)
augmented_map <- setNames(
  unname(reason_labels[as.character(comparison_reason_numbers)]),
  augmented_temp_vars
)
denom_qc <- do.call(
  rbind,
  lapply(seq_along(comparison_reason_numbers), function(i) {
    j <- comparison_reason_numbers[i]
    o <- df_mod[[original_temp_vars[i]]]
    a <- df_mod[[augmented_temp_vars[i]]]
    data.frame(
      reason_num = j,
      reason_label = unname(reason_labels[as.character(j)]),
      followup_cohort_n = nrow(df_mod),
      original_nonmissing_n = sum(!is.na(o)),
      original_positive_n = sum(as.character(o) == "1", na.rm = TRUE),
      augmented_nonmissing_n = sum(!is.na(a)),
      augmented_positive_n = sum(as.character(a) == "1", na.rm = TRUE),
      additional_positive_n =
        sum(as.character(a) == "1", na.rm = TRUE) -
        sum(as.character(o) == "1", na.rm = TRUE)
    )
  })
)
write_csv(
  denom_qc,
  file.path(qc_dir, "reason_specific_model_denominators.csv")
)
novel_vars <- names(novel_labels)
novel_vars <- novel_vars[novel_vars %in% names(df_mod)]
for (v in novel_vars) {
  df_mod[[v]] <- as_binary_factor_zero_one(df_mod[[v]])
}
novel_map <- novel_labels[novel_vars]
cluster_vars <- names(cluster_labels)
cluster_vars <- cluster_vars[cluster_vars %in% names(df_mod)]
if (length(cluster_vars) < 7) {
  candidates <- grep(
    "^cluster_[1-7]$|^primary_cluster_[1-7]$|^cluster_primary_[1-7]$",
    names(df_mod),
    value = TRUE
  )
  if (length(candidates) >= 7) {
    candidates <- candidates[1:7]
    cluster_vars <- candidates
    cluster_labels <- setNames(unname(cluster_labels), cluster_vars)
  }
}
if (length(cluster_vars) != 7) {
  stop(
    "Could not identify all seven cluster indicators.\nFound: ",
    paste(cluster_vars, collapse = ", ")
  )
}
for (v in cluster_vars) {
  df_mod[[v]] <- as_binary_factor_zero_one(df_mod[[v]])
}
cluster_map <- cluster_labels[cluster_vars]
original_primary_model <- run_models(
  dat = df_mod,
  predictors = original_temp_vars,
  labels = original_map,
  covariates = c("age_group_named", "sex"),
  run_name = "original_structured_primary_age_sex"
)
original_primary <- standardise(
  original_primary_model,
  predictor_map = original_map,
  expected_stage = 2,
  analysis_label = "Original structured reasons — age + sex"
)
original_primary$reason_num <- as.integer(
  sub("^orig_reason_", "", original_primary$predictor)
)
original_primary$row_order <- match(
  original_primary$reason_num,
  comparison_order
)
original_primary$cluster_group <- unname(
  cluster_group[as.character(original_primary$reason_num)]
)
original_primary <- original_primary %>% arrange(row_order)
write_csv(
  original_primary,
  file.path(table_dir, "original_reason_primary_corrected.csv")
)
reason_theme_primary_predictors <- c(
  augmented_temp_vars,
  novel_vars
)
reason_theme_primary_map <- c(
  augmented_map,
  novel_map
)
reason_theme_primary_model <- run_models(
  dat = df_mod,
  predictors = reason_theme_primary_predictors,
  labels = reason_theme_primary_map,
  covariates = c("age_group_named", "sex"),
  run_name = "reason_theme_primary_corrected_age_sex"
)
reason_theme_primary <- standardise(
  reason_theme_primary_model,
  predictor_map = reason_theme_primary_map,
  expected_stage = 2,
  analysis_label = "Augmented reasons/themes — age + sex"
)
reason_theme_primary$predictor_type <- ifelse(
  grepl("^aug_reason_", reason_theme_primary$predictor),
  "Augmented existing reason",
  "Novel theme"
)
reason_theme_primary$reason_num <- ifelse(
  reason_theme_primary$predictor_type == "Augmented existing reason",
  as.integer(sub("^aug_reason_", "", reason_theme_primary$predictor)),
  NA_integer_
)
reason_theme_primary$row_order <- ifelse(
  !is.na(reason_theme_primary$reason_num),
  match(reason_theme_primary$reason_num, comparison_order),
  100 + match(reason_theme_primary$predictor, novel_vars)
)
reason_theme_primary <- reason_theme_primary %>% arrange(row_order)
write_csv(
  reason_theme_primary,
  file.path(table_dir, "reason_theme_primary_corrected.csv")
)
reason_theme_round_model <- run_models(
  dat = df_mod,
  predictors = reason_theme_primary_predictors,
  labels = reason_theme_primary_map,
  covariates = c("age_group_named", "sex", "round_midpoint"),
  run_name = "reason_theme_round_corrected_age_sex_round"
)
reason_theme_round <- standardise(
  reason_theme_round_model,
  predictor_map = reason_theme_primary_map,
  expected_stage = 3,
  analysis_label = "Augmented reasons/themes — age + sex + round"
)
reason_theme_round$predictor_type <- ifelse(
  grepl("^aug_reason_", reason_theme_round$predictor),
  "Augmented existing reason",
  "Novel theme"
)
reason_theme_round$reason_num <- ifelse(
  reason_theme_round$predictor_type == "Augmented existing reason",
  as.integer(sub("^aug_reason_", "", reason_theme_round$predictor)),
  NA_integer_
)
reason_theme_round$row_order <- ifelse(
  !is.na(reason_theme_round$reason_num),
  match(reason_theme_round$reason_num, comparison_order),
  100 + match(reason_theme_round$predictor, novel_vars)
)
reason_theme_round <- reason_theme_round %>% arrange(row_order)
write_csv(
  reason_theme_round,
  file.path(table_dir, "reason_theme_round_corrected.csv")
)
cluster_primary_model <- run_models(
  dat = df_mod,
  predictors = cluster_vars,
  labels = cluster_map,
  covariates = c("age_group_named", "sex"),
  run_name = "cluster_primary_corrected_age_sex"
)
cluster_primary <- standardise(
  cluster_primary_model,
  predictor_map = cluster_map,
  expected_stage = 2,
  analysis_label = "Cluster level — age + sex"
)
cluster_primary$cluster_number <- seq_len(nrow(cluster_primary))
write_csv(
  cluster_primary,
  file.path(table_dir, "cluster_primary_corrected.csv")
)
cluster_round_model <- run_models(
  dat = df_mod,
  predictors = cluster_vars,
  labels = cluster_map,
  covariates = c("age_group_named", "sex", "round_midpoint"),
  run_name = "cluster_round_corrected_age_sex_round"
)
cluster_round <- standardise(
  cluster_round_model,
  predictor_map = cluster_map,
  expected_stage = 3,
  analysis_label = "Cluster level — age + sex + round"
)
cluster_round$cluster_number <- seq_len(nrow(cluster_round))
write_csv(
  cluster_round,
  file.path(table_dir, "cluster_round_corrected.csv")
)
orig_comp <- original_primary %>%
  select(
    reason_num,
    predictor_label,
    cluster_group,
    row_order,
    OR_original = OR,
    Lower_original = Lower,
    Upper_original = Upper
  )
aug_comp <- reason_theme_primary %>%
  filter(predictor_type == "Augmented existing reason") %>%
  select(
    reason_num,
    OR_augmented = OR,
    Lower_augmented = Lower,
    Upper_augmented = Upper
  )
original_vs_augmented <- left_join(
  orig_comp,
  aug_comp,
  by = "reason_num"
) %>%
  mutate(
    OR_change = OR_augmented - OR_original,
    logOR_change = log(OR_augmented) - log(OR_original)
  ) %>%
  arrange(row_order)
write_csv(
  original_vs_augmented,
  file.path(table_dir, "original_vs_augmented_comparison_corrected.csv")
)
make_minimal <- function(dat, extra_cols = character(0)) {
  keep <- c(
    "predictor",
    "predictor_label",
    "OR",
    "Lower",
    "Upper",
    "or_ci",
    "sig_status",
    "analysis",
    extra_cols
  )
  dat[, intersect(keep, names(dat)), drop = FALSE]
}
write_csv(
  make_minimal(
    original_primary,
    c("reason_num","cluster_group","row_order")
  ),
  file.path(table_dir, "FOREST_original_reason_primary.csv")
)
write_csv(
  make_minimal(
    reason_theme_primary,
    c("predictor_type","reason_num","row_order")
  ),
  file.path(table_dir, "FOREST_reason_theme_primary.csv")
)
write_csv(
  make_minimal(
    reason_theme_round,
    c("predictor_type","reason_num","row_order")
  ),
  file.path(table_dir, "FOREST_reason_theme_round.csv")
)
write_csv(
  make_minimal(
    cluster_primary,
    c("cluster_number")
  ),
  file.path(table_dir, "FOREST_cluster_primary.csv")
)
write_csv(
  make_minimal(
    cluster_round,
    c("cluster_number")
  ),
  file.path(table_dir, "FOREST_cluster_round.csv")
)
cat("\n============================================================\n")
cat("08 FINAL ALL MODELS CORRECTED — COMPLETED\n")
cat("============================================================\n")
cat("Follow-up cohort: ", nrow(df_mod), "\n", sep = "")
cat("Persistent hesitancy: ",
    sum(df_mod$subs_vaxxed == 1), "\n", sep = "")
cat("Subsequently vaccinated: ",
    sum(df_mod$subs_vaxxed == 0), "\n\n", sep = "")
cat("EXPORT THESE FOREST-PLOT CSVs:\n")
cat("1) tables/FOREST_original_reason_primary.csv\n")
cat("2) tables/FOREST_reason_theme_primary.csv\n")
cat("3) tables/FOREST_reason_theme_round.csv\n")
cat("4) tables/FOREST_cluster_primary.csv\n")
cat("5) tables/FOREST_cluster_round.csv\n")
cat("6) tables/original_vs_augmented_comparison_corrected.csv\n")
cat("7) qc/reason_specific_model_denominators.csv\n")
cat("============================================================\n")
