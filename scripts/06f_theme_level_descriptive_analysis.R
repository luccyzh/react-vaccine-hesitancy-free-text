# ==============================================================================
# STEP 06F: THEME-LEVEL DESCRIPTIVE ANALYSIS
# ==============================================================================
# Purpose
#   Describe the valid free-text cohort and the six final novel themes before
#   adjusted regression modelling.
#
# Outputs include
#   - free-text cohort and novel-theme counts/prevalence
#   - participant characteristics by novel theme (age group and sex)
#   - study-round distribution by theme
#   - subsequent NHS vaccination status/proportion by theme
#   - comparison of the original broad "Other" group with six decomposed themes
#
# Notes
#   - This is descriptive analysis only; it does not estimate adjusted ORs.
#   - Topic -1 participants remain in the overall free-text cohort and retain
#     their structured reasons, but they have no text-derived novel theme.
#   - Participants vaccinated before the REACT survey are excluded only from
#     the subsequent-vaccination summaries, matching the project outcome definition.
# ============================================================================== 

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 0. PATHS
# ------------------------------------------------------------------------------
project_root <- "D:/projects/hda_2026/han"

input_rds <- file.path(
  project_root,
  "outputs",
  "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)

output_dir <- file.path(project_root, "outputs", "06f_theme_level_descriptive")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. SETTINGS AND VARIABLE DICTIONARY
# ------------------------------------------------------------------------------
primary_hesitancy_var <- "analysis_4_vax_any_hes_or_refused_vs_vaxxed"
free_text_topic_var <- "topic_id"
other_original_var <- "vaccrufuse3_19"

novel_cols <- c(
  "novel_brand_preference",
  "novel_access_availability_dosing",
  "novel_ethical_religious_animal",
  "novel_government_distrust_autonomy",
  "novel_mrna_gene_technology",
  "novel_uncertainty_decision_pending"
)

novel_labels <- c(
  "Vaccine brand preference and choice",
  "Access, availability and dosing arrangements",
  "Ethical, religious and animal-related concerns",
  "Government distrust, coercion and loss of autonomy",
  "Concerns about mRNA and gene-therapy technology",
  "Uncertainty or decision not yet made"
)

novel_dictionary <- data.frame(
  variable = novel_cols,
  theme = novel_labels,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 2. HELPERS
# ------------------------------------------------------------------------------
is_positive_flag <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  !is.na(x) & x_chr %in% c("1", "true", "t", "yes", "y")
}

as_binary <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(is.na(x_num), NA_integer_, as.integer(x_num == 1))
}

safe_percent <- function(n, denominator) {
  ifelse(denominator > 0, 100 * n / denominator, NA_real_)
}

# Produce counts and column percentages for one categorical characteristic.
make_categorical_summary <- function(data, group_label, variable_name) {
  if (!variable_name %in% names(data)) return(NULL)
  x <- as.character(data[[variable_name]])
  x[is.na(x) | trimws(x) == ""] <- "Missing"
  tab <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(tab) <- c("level", "n")
  tab$percent <- safe_percent(tab$n, sum(tab$n))
  tab$group <- group_label
  tab$characteristic <- variable_name
  tab[, c("group", "characteristic", "level", "n", "percent")]
}

# ------------------------------------------------------------------------------
# 3. LOAD AND VALIDATE
# ------------------------------------------------------------------------------
if (!file.exists(input_rds)) {
  stop("Missing Step 06 enriched RDS: ", input_rds)
}

df <- readRDS(input_rds)
if (!is.data.frame(df)) stop("Step 06 enriched RDS did not contain a data.frame.")

required_cols <- c(primary_hesitancy_var, free_text_topic_var, novel_cols)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Required variable(s) missing:\n", paste(missing_cols, collapse = "\n"))
}

# Valid free-text cohort: participants with a response-level BERTopic assignment,
# including topic -1 outliers.
free_text_flag <- !is.na(df[[free_text_topic_var]])
df_ft <- df[free_text_flag, , drop = FALSE]

if (nrow(df_ft) != 4102) {
  warning(
    "Valid free-text cohort is ", nrow(df_ft),
    ", not the previously observed 4,102. Check topic_id and Step 06 output."
  )
}

# Coerce theme variables to explicit 0/1. Step 06 should already contain no NA
# in these columns; stop if that assumption is violated.
for (v in novel_cols) {
  df_ft[[v]] <- as_binary(df_ft[[v]])
}
if (anyNA(df_ft[novel_cols])) {
  stop("Novel-theme indicators contain NA in the valid free-text cohort.")
}

# ------------------------------------------------------------------------------
# 4. COHORT AND THEME PREVALENCE
# ------------------------------------------------------------------------------
cohort_summary <- data.frame(
  measure = c(
    "rows_in_enriched_rds",
    "definition4_hesitant_cohort",
    "valid_free_text_cohort",
    "topic_minus_1_outliers",
    "non_outlier_free_text_responses",
    "participants_with_any_novel_theme",
    "participants_with_no_novel_theme"
  ),
  n = c(
    nrow(df),
    sum(is_positive_flag(df[[primary_hesitancy_var]])),
    nrow(df_ft),
    sum(df_ft[[free_text_topic_var]] == -1, na.rm = TRUE),
    sum(df_ft[[free_text_topic_var]] != -1, na.rm = TRUE),
    sum(rowSums(df_ft[novel_cols]) > 0),
    sum(rowSums(df_ft[novel_cols]) == 0)
  ),
  stringsAsFactors = FALSE
)
write.csv(cohort_summary, file.path(output_dir, "cohort_summary.csv"), row.names = FALSE)

novel_theme_summary <- data.frame(
  variable = novel_cols,
  theme = novel_labels,
  n = vapply(df_ft[novel_cols], sum, numeric(1)),
  denominator_free_text = nrow(df_ft),
  stringsAsFactors = FALSE
)
novel_theme_summary$percent_of_free_text <- safe_percent(
  novel_theme_summary$n,
  novel_theme_summary$denominator_free_text
)
write.csv(
  novel_theme_summary,
  file.path(output_dir, "novel_theme_prevalence_freetext_cohort.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 5. PARTICIPANT CHARACTERISTICS: OVERALL VS SIX THEMES
# ------------------------------------------------------------------------------
characteristic_vars <- intersect(
  c("age_group_named", "sex", "study_round"),
  names(df_ft)
)

characteristic_tables <- list()
idx <- 1L

for (v in characteristic_vars) {
  characteristic_tables[[idx]] <- make_categorical_summary(
    df_ft,
    "Overall valid free-text cohort",
    v
  )
  idx <- idx + 1L
}

for (i in seq_along(novel_cols)) {
  theme_df <- df_ft[df_ft[[novel_cols[i]]] == 1, , drop = FALSE]
  for (v in characteristic_vars) {
    characteristic_tables[[idx]] <- make_categorical_summary(
      theme_df,
      novel_labels[i],
      v
    )
    idx <- idx + 1L
  }
}

characteristics_long <- do.call(rbind, characteristic_tables)
write.csv(
  characteristics_long,
  file.path(output_dir, "participant_characteristics_overall_and_by_theme_long.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------------------
# 6. SUBSEQUENT NHS VACCINATION STATUS BY THEME
# ------------------------------------------------------------------------------
# Reproduce the binary outcome definition locally for descriptive summaries:
#   0 = NHS vaccination recorded after survey
#   1 = no NHS vaccine on record (remained unvaccinated)
# Participants vaccinated before the survey are excluded.
if ("nhs_vaccine_status" %in% names(df_ft)) {
  df_vax <- df_ft[
    !is.na(df_ft$nhs_vaccine_status) &
      df_ft$nhs_vaccine_status != "Vaccine recorded before REACT survey",
    ,
    drop = FALSE
  ]

  df_vax$subs_vaxxed <- ifelse(
    df_vax$nhs_vaccine_status == "NHS vaccination recorded",
    0L,
    1L
  )

  vaccination_rows <- list()
  j <- 1L

  add_vax_row <- function(data, group_name, predictor = NULL) {
    if (!is.null(predictor)) {
      data <- data[is_positive_flag(data[[predictor]]), , drop = FALSE]
    }
    n_total <- nrow(data)
    n_subsequently_vaccinated <- sum(data$subs_vaxxed == 0, na.rm = TRUE)
    n_remained_unvaccinated <- sum(data$subs_vaxxed == 1, na.rm = TRUE)
    data.frame(
      group = group_name,
      n_with_outcome = n_total,
      n_subsequently_vaccinated = n_subsequently_vaccinated,
      percent_subsequently_vaccinated = safe_percent(
        n_subsequently_vaccinated,
        n_total
      ),
      n_remained_unvaccinated = n_remained_unvaccinated,
      percent_remained_unvaccinated = safe_percent(
        n_remained_unvaccinated,
        n_total
      ),
      stringsAsFactors = FALSE
    )
  }

  vaccination_rows[[j]] <- add_vax_row(
    df_vax,
    "Overall valid free-text cohort"
  )
  j <- j + 1L

  # Original broad Other indicator. In this selected free-text cohort this may
  # equal the overall group; retaining it makes the comparison explicit.
  if (other_original_var %in% names(df_vax)) {
    vaccination_rows[[j]] <- add_vax_row(
      df_vax,
      "Original broad Other indicator",
      other_original_var
    )
    j <- j + 1L
  }

  for (i in seq_along(novel_cols)) {
    vaccination_rows[[j]] <- add_vax_row(
      df_vax,
      novel_labels[i],
      novel_cols[i]
    )
    j <- j + 1L
  }

  vaccination_summary <- do.call(rbind, vaccination_rows)
  write.csv(
    vaccination_summary,
    file.path(output_dir, "subsequent_vaccination_by_theme.csv"),
    row.names = FALSE,
    na = ""
  )

  # Plot subsequent vaccination proportion, with n displayed in labels.
  png(
    file.path(figure_dir, "subsequent_vaccination_proportion_by_theme.png"),
    width = 2200,
    height = 1500,
    res = 200
  )
  par(mar = c(5, 18, 4, 2))
  plot_data <- vaccination_summary[nrow(vaccination_summary):1, , drop = FALSE]
  barplot(
    plot_data$percent_subsequently_vaccinated,
    names.arg = paste0(plot_data$group, " (n=", plot_data$n_with_outcome, ")"),
    horiz = TRUE,
    las = 1,
    xlab = "Subsequently vaccinated (%)",
    main = "Subsequent NHS-recorded vaccination by free-text theme",
    xlim = c(0, 100)
  )
  dev.off()
} else {
  warning(
    "nhs_vaccine_status was not found. Theme-level vaccination summaries were skipped."
  )
}

# ------------------------------------------------------------------------------
# 7. THEME PREVALENCE FIGURE
# ------------------------------------------------------------------------------
png(
  file.path(figure_dir, "novel_theme_prevalence_freetext_cohort.png"),
  width = 2000,
  height = 1300,
  res = 200
)
par(mar = c(5, 19, 4, 2))
plot_prev <- novel_theme_summary[nrow(novel_theme_summary):1, , drop = FALSE]
barplot(
  plot_prev$percent_of_free_text,
  names.arg = paste0(plot_prev$theme, " (n=", plot_prev$n, ")"),
  horiz = TRUE,
  las = 1,
  xlab = "Prevalence among valid free-text respondents (%)",
  main = "Prevalence of the six final novel themes"
)
dev.off()

write.csv(
  novel_dictionary,
  file.path(output_dir, "novel_theme_dictionary.csv"),
  row.names = FALSE
)

cat("\nSTEP 06F COMPLETED SUCCESSFULLY.\n")
cat("Valid free-text cohort:", nrow(df_ft), "\n")
cat("Novel themes described:", length(novel_cols), "\n")
cat("Outputs saved to:", output_dir, "\n")
