# ============================================================
# 06d_mapping_qc_and_comparison_plots.R
#
# Purpose:
#   Produce QC tables and descriptive comparison figures after
#   participant-level mapping, before clustering/regression.
#
# Key outputs:
#   1) Original vs augmented counts for VACCRUFUSE3_1-23
#   2) Structured/text overlap and text-only capture
#   3) Novel-theme counts
#   4) All-zero participant checks for:
#        a) 23 augmented + 6 novel themes
#        b) 23 augmented + 5 substantive novel themes
#
# This script does NOT run clustering or regression.
# ============================================================

project_root <- "D:/projects/hda_2026/han"
input_rds <- file.path(
  project_root, "outputs", "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)
output_dir <- file.path(
  project_root, "outputs", "06d_mapping_qc_and_comparison"
)
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_rds)) {
  stop("Required input not found: ", input_rds)
}

df <- readRDS(input_rds)

existing_labels <- c(
  "General side-effect concerns",
  "Effectiveness / more evidence",
  "Long-term health effects",
  "Travel barriers",
  "Difficulty reaching vaccination centre",
  "Low perceived personal COVID-19 risk",
  "Existing health condition",
  "Against vaccines in general",
  "Doubts vaccine will work personally",
  "Concern vaccine may cause COVID-19",
  "Fear of pain or needles",
  "Concern about feeling ill",
  "Prior COVID-19 / vaccination not needed",
  "Pregnancy or breastfeeding",
  "COVID-19 impact perceived as exaggerated",
  "Distrust in vaccine developers / development",
  "Others vaccinated, so own vaccination unnecessary",
  "Others need limited doses more",
  "Other",
  "Prefer not to say",
  "Fertility / trying to conceive",
  "Allergic reaction concerns",
  "Previous bad vaccine reaction"
)

original_cols <- paste0("vaccrufuse3_", 1:23)
text_cols <- paste0("text_existing_", 1:23)
augmented_cols <- paste0("augmented_existing_", 1:23)

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

required_cols <- c("u_passcode", original_cols, text_cols, augmented_cols, novel_cols)
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop("Input RDS is missing columns: ", paste(missing_cols, collapse = ", "))
}

binary_count <- function(x) {
  sum(!is.na(x) & as.numeric(x) == 1)
}

# ------------------------------------------------------------
# DEFINE ANALYSIS COHORTS
# ------------------------------------------------------------
primary_hesitancy_var<-'analysis_4_vax_any_hes_or_refused_vs_vaxxed'

if(!primary_hesitancy_var %in% names(df)){
  stop(
    'Primary hesitancy varible not found: ',
    primary_hesitancy_var
  )
}

# 1. Valid free-text cohort: used for structured vs text overlap
df_freetext <- df[
  !is.na(df$topic_id),
  ,
  drop = FALSE
]

#2. Definition4 reasons-eligible cohort
definition4_flag<-
  !is.na(df[[primary_hesitancy_var]])&
  as.numeric(df[[primary_hesitancy_var]]) == 1

reason_observed_flag<-
  rowSums(!is.na(df[original_cols])) > 0 

df_definition4_reasons <- df[
  definition4_flag & reason_observed_flag,
  ,
  drop = FALSE
]

cat('Valid free-text cohort:',
nrow(df_freetext))
cat('Definition 4 hesitant cohort:',
sum(definition4_flag))
cat('Definition 4 reasons-eligible cohort:',
nrow(df_definition4_reasons))


# ------------------------------------------------------------
# 1. EXISTING REASON CAPTURE
# ------------------------------------------------------------
existing_capture_definition4 <- data.frame(
  reason_number = 1:23,
  reason_label = existing_labels,
  original_n = sapply(df_definition4_reasons[original_cols], binary_count),
  text_derived_n = sapply(df_definition4_reasons[text_cols], binary_count),
  augmented_n = sapply(df_definition4_reasons[augmented_cols], binary_count),
  stringsAsFactors = FALSE
)

existing_capture_definition4$additional_text_capture_n <-
  existing_capture_definition4$augmented_n - existing_capture_definition4$original_n

overlap_n <- integer(23)
text_only_n <- integer(23)
structured_only_n <- integer(23)
neither_n <- integer(23)

for (k in 1:23) {
  original_positive <- !is.na(df_freetext[[original_cols[k]]]) &
    as.numeric(df_freetext[[original_cols[k]]]) == 1
  text_positive <- !is.na(df_freetext[[text_cols[k]]]) &
    as.numeric(df_freetext[[text_cols[k]]]) == 1

  overlap_n[k] <- sum(original_positive & text_positive)
  text_only_n[k] <- sum(!original_positive & text_positive)
  structured_only_n[k] <- sum(original_positive & !text_positive)
  neither_n[k] <- sum(!original_positive & !text_positive)
}

overlap_freetext<-data.frame(
  reason_number = 1:23,
  reason_label = existing_labels,
  original_n = sapply(
    df_freetext[original_cols],
    binary_count
  ),
  text_derived_n = sapply(
    df_freetext[text_cols],
    binary_count
  ),
  overlap_n = overlap_n,
  text_only_n = text_only_n,
  structured_only_n = structured_only_n,
  neither_n = neither_n,
  stringsAsFactors = FALSE
)

overlap_freetext$text_overlap_percent <- ifelse(
  overlap_freetext$text_derived_n > 0,
  100 * overlap_freetext$overlap_n /
    overlap_freetext$text_derived_n,
  NA_real_
)

write.csv(
  existing_capture_definition4,
  file.path(output_dir, "existing_reason_original_vs_augmented_definition4.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  overlap_freetext,
  file.path(output_dir, "existing_reason_overlap_freetext_cohort.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 2. NOVEL THEME COUNTS
# ------------------------------------------------------------
novel_summary <- data.frame(
  variable = novel_cols,
  theme = novel_labels,
  n = sapply(df[novel_cols], binary_count),
  stringsAsFactors = FALSE
)
novel_summary <- novel_summary[order(-novel_summary$n), ]

write.csv(
  novel_summary,
  file.path(output_dir, "novel_theme_counts.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 3. ALL-ZERO CHECKS
# ------------------------------------------------------------
# Restrict the diagnostic to participants with a valid mapped topic
# or any text-derived/novel information.
free_text_flag <- !is.na(df$topic_id) |
  rowSums(df[text_cols], na.rm = TRUE) > 0 |
  rowSums(df[novel_cols], na.rm = TRUE) > 0

cluster_vars_6 <- c(augmented_cols, novel_cols)
cluster_vars_5 <- c(augmented_cols, novel_cols[novel_cols !=
  "novel_uncertainty_decision_pending"])

row_sum_6 <- rowSums(df[cluster_vars_6], na.rm = TRUE)
row_sum_5 <- rowSums(df[cluster_vars_5], na.rm = TRUE)

all_zero_summary <- data.frame(
  diagnostic = c(
    "Free-text participants checked",
    "All zero using 23 augmented + 6 novel themes",
    "All zero using 23 augmented + 5 substantive novel themes",
    "Become all-zero only after excluding uncertainty",
    "Uncertainty positive and all-zero on 5-theme matrix"
  ),
  n = c(
    sum(free_text_flag),
    sum(free_text_flag & row_sum_6 == 0),
    sum(free_text_flag & row_sum_5 == 0),
    sum(free_text_flag & row_sum_6 > 0 & row_sum_5 == 0),
    sum(
      free_text_flag &
      df$novel_uncertainty_decision_pending == 1 &
      row_sum_5 == 0
    )
  ),
  stringsAsFactors = FALSE
)

write.csv(
  all_zero_summary,
  file.path(output_dir, "clustering_all_zero_check.csv"),
  row.names = FALSE
)

all_zero_participants <- df[
  free_text_flag & row_sum_5 == 0,
  intersect(
    c(
      "u_passcode", "topic_id",
      "novel_uncertainty_decision_pending",
      original_cols, text_cols, augmented_cols, novel_cols
    ),
    names(df)
  ),
  drop = FALSE
]

write.csv(
  all_zero_participants,
  file.path(output_dir, "participants_all_zero_for_primary_clustering.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------
# 4. FIGURES
# ------------------------------------------------------------
png(
  file.path(figure_dir, "original_vs_augmented_existing_counts.png"),
  width = 1800, height = 1300, res = 180
)
par(mar = c(11, 5, 4, 2))
ylim_max <- max(existing_capture$augmented_n, na.rm = TRUE) * 1.1
bp <- barplot(
  rbind(existing_capture$original_n, existing_capture$augmented_n),
  beside = TRUE,
  names.arg = paste0("R", existing_capture$reason_number),
  las = 2,
  ylim = c(0, ylim_max),
  ylab = "Number of participants",
  main = "Original versus augmented existing hesitancy reasons"
)
legend(
  "topright",
  legend = c("Original structured", "Augmented with free text"),
  fill = c("grey70", "grey30"),
  bty = "n"
)
dev.off()

png(
  file.path(figure_dir, "additional_text_capture_by_existing_reason.png"),
  width = 1800, height = 1300, res = 180
)
par(mar = c(11, 5, 4, 2))
barplot(
  existing_capture$additional_text_capture_n,
  names.arg = paste0("R", existing_capture$reason_number),
  las = 2,
  ylab = "Additional participants captured",
  main = "Additional capture from free-text mapping"
)
abline(h = 0, lty = 2)
dev.off()

png(
  file.path(figure_dir, "novel_theme_counts.png"),
  width = 1800, height = 1200, res = 180
)
par(mar = c(5, 14, 4, 2))
barplot(
  rev(novel_summary$n),
  names.arg = rev(novel_summary$theme),
  horiz = TRUE,
  las = 1,
  xlab = "Number of participants",
  main = "Novel free-text theme counts"
)
dev.off()

# ------------------------------------------------------------
# 5. RUN SUMMARY
# ------------------------------------------------------------
run_summary <- data.frame(
  measure = c(
    "rows_in_full_enriched_dataset",
    "free_text_participants_checked",
    "total_additional_existing_reason_assignments",
    "participants_all_zero_after_excluding_uncertainty"
  ),
  value = c(
    nrow(df),
    sum(free_text_flag),
    sum(existing_capture$additional_text_capture_n),
    sum(free_text_flag & row_sum_5 == 0)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  run_summary,
  file.path(output_dir, "run_summary.csv"),
  row.names = FALSE
)

cat("\nStep 06d completed successfully.\n")
cat("Outputs saved to: ", output_dir, "\n", sep = "")
cat("Review clustering_all_zero_check.csv before clustering.\n")
