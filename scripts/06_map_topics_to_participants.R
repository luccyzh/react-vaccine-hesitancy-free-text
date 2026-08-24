# ============================================================
# 06_map_topics_to_participants.R
# Purpose:
#   1) Join the manually reviewed response-level BERTopic mapping back to people.
#   2) Create text-derived indicators for existing questionnaire reasons.
#   3) Create six participant-level novel-theme binary indicators.
#   4) Describe structured-vs-free-text overlap/discrepancy.
#   5) Export analysis-ready primary and augmented downstream datasets.
#
# Analysis definitions
#   - Original vaccrufuse3_1 ... vaccrufuse3_23 are NEVER overwritten.
#   - Primary downstream variables = augmented existing reasons + novel themes.
#   - text_existing_* and original VACCRUFUSE3 variables are retained for audit/comparison.
#   - Original VACCRUFUSE3 variables are never overwritten.
# ============================================================

# ----------------------------
# 0. PATHS
# ----------------------------
project_root <- "D:/projects/hda_2026/han"
raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"

response_topics_csv <- file.path(
  project_root, "outputs", "02_response_bertopic", "response_document_topics.csv"
)

# Completed manual review workbook.
manual_mapping_file <- file.path(
  project_root, "outputs", "05_topic_review", "response_topic_mapping_review.txt"
)

existing_prevalence_csv <- file.path(
  project_root, "outputs", "01_data_preparation", "existing_option_prevalence.csv"
)

stage_dir <- file.path(project_root, "outputs", "06_participant_mapping")
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1. LOAD INPUTS
# ----------------------------
required_files <- c(raw_rds, response_topics_csv, manual_mapping_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required file(s):\n", paste(missing_files, collapse = "\n"),
    "\n\nExpected completed manual review sheet:\n", manual_mapping_file
  )
}

df <- readRDS(raw_rds)
docs <- read.csv(response_topics_csv, stringsAsFactors = FALSE, check.names = FALSE)
mapping <- read.delim(manual_mapping_file,stringsAsFactors = FALSE, check.names = FALSE,fileEncoding = 'UTF-16LE')

# ----------------------------
# 2. STANDARDISE COLUMN NAMES
# ----------------------------
normalise_name <- function(x) {
  x <- trimws(x)
  x <- gsub("\\?", "", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

names(mapping) <- normalise_name(names(mapping))
names(docs) <- normalise_name(names(docs))

# Accept the likely manual-sheet column names.
rename_first_match <- function(data, target, candidates) {
  hit <- intersect(candidates, names(data))
  if (!(target %in% names(data)) && length(hit) > 0) {
    names(data)[names(data) == hit[1]] <- target
  }
  data
}

mapping <- rename_first_match(mapping, "topic_id", c("topic", "topicid"))
mapping <- rename_first_match(mapping, "existing", c("existing_", "is_existing"))
mapping <- rename_first_match(mapping, "existing_code", c("existingcode", "questionnaire_code"))
mapping <- rename_first_match(mapping, "novel_theme", c("noveltheme"))
mapping <- rename_first_match(mapping, "final_theme", c("finaltheme", "broader_theme"))

docs <- rename_first_match(docs, "topic", c("topic_id", "topicid"))
docs <- rename_first_match(docs, "participant_id", c("u_passcode", "participantid"))
docs <- rename_first_match(docs, "response_id", c("responseid"))

required_mapping_cols <- c("topic_id", "existing", "existing_code", "final_theme")
required_doc_cols <- c("participant_id", "response_id", "topic")

missing_mapping_cols <- setdiff(required_mapping_cols, names(mapping))
missing_doc_cols <- setdiff(required_doc_cols, names(docs))

if (length(missing_mapping_cols) > 0) {
  stop("Manual mapping is missing columns: ", paste(missing_mapping_cols, collapse = ", "))
}
if (length(missing_doc_cols) > 0) {
  stop("Document-topic file is missing columns: ", paste(missing_doc_cols, collapse = ", "))
}

mapping$topic_id <- as.integer(mapping$topic_id)
docs$topic <- as.integer(docs$topic)
docs$participant_id <- as.character(docs$participant_id)
docs$response_id <- as.character(docs$response_id)

mapping$existing <- trimws(tolower(as.character(mapping$existing)))
mapping$existing <- ifelse(
  mapping$existing %in% c("yes", "y", "1", "true"), "Yes",
  ifelse(mapping$existing %in% c("no", "n", "0", "false"), "No", NA_character_)
)
mapping$existing_code <- toupper(trimws(as.character(mapping$existing_code)))
mapping$final_theme <- trimws(as.character(mapping$final_theme))
if ("novel_theme" %in% names(mapping)) {
  mapping$novel_theme <- trimws(as.character(mapping$novel_theme))
}

# Keep only actual BERTopic topics; -1 is handled separately as an outlier.
mapping <- mapping[!is.na(mapping$topic_id) & mapping$topic_id != -1, , drop = FALSE]

# ----------------------------
# 3. STRICT QC OF MANUAL MAPPING
# ----------------------------
if (anyDuplicated(mapping$topic_id) > 0) {
  duplicated_topics <- unique(mapping$topic_id[duplicated(mapping$topic_id)])
  stop("Duplicate topic IDs in manual mapping: ", paste(duplicated_topics, collapse = ", "))
}

if (any(is.na(mapping$existing))) {
  stop(
    "Some mapped topics do not have Existing = Yes/No. Topic IDs: ",
    paste(mapping$topic_id[is.na(mapping$existing)], collapse = ", ")
  )
}

bad_existing <- mapping$existing == "Yes" &
  (is.na(mapping$existing_code) | mapping$existing_code == "")
if (any(bad_existing)) {
  stop(
    "Existing topics missing Existing_code. Topic IDs: ",
    paste(mapping$topic_id[bad_existing], collapse = ", ")
  )
}

bad_novel <- mapping$existing == "No" &
  (is.na(mapping$final_theme) | mapping$final_theme == "")
if (any(bad_novel)) {
  stop(
    "Novel topics missing Final_theme. Topic IDs: ",
    paste(mapping$topic_id[bad_novel], collapse = ", ")
  )
}

# Existing codes must be VACCRUFUSE3_1 ... VACCRUFUSE3_23.
existing_rows <- mapping$existing == "Yes"
valid_code_pattern <- grepl("^VACCRUFUSE3_([1-9]|1[0-9]|2[0-3])$", mapping$existing_code)
if (any(existing_rows & !valid_code_pattern)) {
  stop(
    "Invalid Existing_code values: ",
    paste(unique(mapping$existing_code[existing_rows & !valid_code_pattern]), collapse = ", ")
  )
}

model_topic_ids <- sort(unique(docs$topic[docs$topic != -1]))
mapped_topic_ids <- sort(mapping$topic_id)
missing_from_mapping <- setdiff(model_topic_ids, mapped_topic_ids)
extra_in_mapping <- setdiff(mapped_topic_ids, model_topic_ids)

if (length(missing_from_mapping) > 0 || length(extra_in_mapping) > 0) {
  stop(
    "Topic mapping does not exactly match response BERTopic output.\n",
    "Missing from mapping: ", paste(missing_from_mapping, collapse = ", "), "\n",
    "Extra in mapping: ", paste(extra_in_mapping, collapse = ", ")
  )
}

# QC reference counts: 88 non-outlier topics, 69 existing, 19 additional topics, 6 final themes.
qc_expectations <- data.frame(
  measure = c(
    "non_outlier_topics",
    "topics_mapped_existing",
    "topics_mapped_novel",
    "final_novel_themes",
    "response_rows",
    "unique_free_text_participants",
    "outlier_responses"
  ),
  observed = c(
    length(model_topic_ids),
    sum(mapping$existing == "Yes"),
    sum(mapping$existing == "No"),
    length(unique(mapping$final_theme[mapping$existing == "No"])),
    nrow(docs),
    length(unique(docs$participant_id)),
    sum(docs$topic == -1)
  ),
  expected = c(88, 69, 19, 6, 4102, 4102, 1249),
  stringsAsFactors = FALSE
)
qc_expectations$matches_expected <- qc_expectations$observed == qc_expectations$expected
write.csv(qc_expectations, file.path(stage_dir, "mapping_qc_summary.csv"), row.names = FALSE)

if (any(!qc_expectations$matches_expected)) {
  warning(
    "Some observed counts differ from the reference QC counts. ",
    "Review outputs/06_participant_mapping/mapping_qc_summary.csv before downstream analysis."
  )
}

# ----------------------------
# 4. JOIN TOPIC REVIEW BACK TO EACH RESPONSE/PARTICIPANT
# ----------------------------
participant_topic <- merge(
  docs,
  mapping,
  by.x = "topic",
  by.y = "topic_id",
  all.x = TRUE,
  sort = FALSE
)

participant_topic$topic_outlier <- as.integer(participant_topic$topic == -1)
participant_topic$mapping_status <- ifelse(
  participant_topic$topic == -1,
  "BERTopic outlier",
  ifelse(participant_topic$existing == "Yes", "Existing option", "Novel theme")
)

# Outliers deliberately have no existing/new classification.
participant_topic$existing[participant_topic$topic == -1] <- NA_character_
participant_topic$existing_code[participant_topic$topic == -1] <- NA_character_
participant_topic$final_theme[participant_topic$topic == -1] <- NA_character_

if (anyDuplicated(participant_topic$response_id) > 0) {
  stop("response_id became duplicated after joining manual mapping")
}

# The analysis dataset contains one valid free-text response per participant.
if (anyDuplicated(participant_topic$participant_id) > 0) {
  stop(
    "More than one valid response was found for at least one participant. ",
    "Do not continue until a participant-level union rule is agreed."
  )
}

write.csv(
  participant_topic,
  file.path(stage_dir, "participant_topic_mapping.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 5. CREATE TEXT-DERIVED EXISTING BINARY VARIABLES
# ----------------------------
participant_binary <- data.frame(
  u_passcode = participant_topic$participant_id,
  response_id = participant_topic$response_id,
  topic_id = participant_topic$topic,
  topic_outlier = participant_topic$topic_outlier,
  stringsAsFactors = FALSE
)

for (k in 1:23) {
  code_k <- paste0("VACCRUFUSE3_", k)
  participant_binary[[paste0("text_existing_", k)]] <- as.integer(
    !is.na(participant_topic$existing)&
    participant_topic$existing == "Yes" & 
      !is.na(participant_topic$existing_code)&
      participant_topic$existing_code == code_k
  )
}

# ----------------------------
# 6. CREATE SIX NOVEL-THEME BINARY VARIABLES
# ----------------------------
# Final-theme labels used in the reviewed mapping.
novel_theme_dictionary <- data.frame(
  final_theme = c(
    "Vaccine brand preference and choice",
    "Access, availability and dosing arrangements",
    "Ethical, religious and animal-related concerns",
    "Government distrust, coercion and loss of autonomy",
    "Concerns about mRNA and gene-therapy technology",
    "Uncertainty or decision not yet made"
  ),
  variable = c(
    "novel_brand_preference",
    "novel_access_availability_dosing",
    "novel_ethical_religious_animal",
    "novel_government_distrust_autonomy",
    "novel_mrna_gene_technology",
    "novel_uncertainty_decision_pending"
  ),
  stringsAsFactors = FALSE
)

observed_final_themes <- sort(unique(mapping$final_theme[mapping$existing == "No"]))
missing_dictionary_themes <- setdiff(observed_final_themes, novel_theme_dictionary$final_theme)
unused_dictionary_themes <- setdiff(novel_theme_dictionary$final_theme, observed_final_themes)

if (length(missing_dictionary_themes) > 0 || length(unused_dictionary_themes) > 0) {
  stop(
    "Final-theme names do not match the six-theme dictionary.\n",
    "Themes missing from dictionary: ", paste(missing_dictionary_themes, collapse = " | "), "\n",
    "Dictionary themes not found in mapping: ", paste(unused_dictionary_themes, collapse = " | "), "\n",
    "Final_theme labels must exactly match the dictionary in Section 6."
  )
}

for (i in seq_len(nrow(novel_theme_dictionary))) {
  participant_binary[[novel_theme_dictionary$variable[i]]] <- as.integer(
    !is.na(participant_topic$existing)&
    participant_topic$existing == "No" &
      !is.na(participant_topic$final_theme)&
      participant_topic$final_theme == novel_theme_dictionary$final_theme[i]
  )
}

# General indicators useful for downstream filtering/QC.
novel_cols <- novel_theme_dictionary$variable
participant_binary$any_text_existing <- as.integer(
  rowSums(participant_binary[paste0("text_existing_", 1:23)],na.rm=TRUE) > 0
)
participant_binary$any_novel_theme <- as.integer(
  rowSums(participant_binary[novel_cols],na.rm=TRUE) > 0
)

# Every non-outlier response must be either existing or novel, never both.
non_outlier <- participant_binary$topic_outlier == 0
classification_total <- participant_binary$any_text_existing + participant_binary$any_novel_theme
if (any(classification_total[non_outlier] != 1)) {
  stop("Some non-outlier participants were not mapped to exactly one existing or novel category")
}
if (any(classification_total[!non_outlier] != 0)) {
  stop("Outlier participants unexpectedly received an existing or novel theme")
}

# ----------------------------
# 7. JOIN ORIGINAL STRUCTURED OPTIONS TO FREE-TEXT PARTICIPANTS
# ----------------------------
combined_vars <- paste0("vaccrufuse3_", 1:23)
required_df_vars <- c("u_passcode", combined_vars)
missing_df_vars <- setdiff(required_df_vars, names(df))
if (length(missing_df_vars) > 0) {
  stop("Raw RDS is missing variables: ", paste(missing_df_vars, collapse = ", "))
}

if (anyDuplicated(df$u_passcode) > 0) {
  stop("u_passcode is not unique in the loaded RDS; participant mapping would be ambiguous")
}

structured_small <- df[, required_df_vars, drop = FALSE]
structured_small$u_passcode <- as.character(structured_small$u_passcode)
participant_binary$u_passcode <- as.character(participant_binary$u_passcode)

participant_binary <- merge(
  participant_binary,
  structured_small,
  by = "u_passcode",
  all.x = TRUE,
  sort = FALSE
)

if (nrow(participant_binary) != nrow(participant_topic)) {
  stop("Participant count changed when joining original structured variables")
}

# ----------------------------
# 8. CREATE AUGMENTED EXISTING VARIABLES (PRIMARY ANALYSIS INPUT)
# ----------------------------
for (k in 1:23) {
  structured_col <- paste0("vaccrufuse3_", k)
  text_col <- paste0("text_existing_", k)
  augmented_col <- paste0("augmented_existing_", k)

  structured_selected <- participant_binary[[structured_col]] == 1
  text_selected <- participant_binary[[text_col]] == 1

  # Preserve missingness when the option was not observed and text gives no evidence.
  participant_binary[[augmented_col]] <- ifelse(
    text_selected,
    1L,
    ifelse(
      is.na(participant_binary[[structured_col]]),
      NA_integer_,
      as.integer(structured_selected)
    )
  )
}

write.csv(
  participant_binary,
  file.path(stage_dir, "participant_binary_mapping.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 9. A. STRUCTURED VS FREE-TEXT OVERLAP / DISCREPANCY
# ----------------------------
overlap_rows <- lapply(1:23, function(k) {
  structured <- participant_binary[[paste0("vaccrufuse3_", k)]]
  text_flag <- participant_binary[[paste0("text_existing_", k)]]

  structured_yes <- !is.na(structured) & structured == 1
  structured_no <- !is.na(structured) & structured != 1
  structured_missing <- is.na(structured)

  data.frame(
    option_number = k,
    variable = paste0("vaccrufuse3_", k),
    free_text_cohort_n = nrow(participant_binary),
    structured_nonmissing_n = sum(!is.na(structured)),
    structured_yes_text_yes = sum(structured_yes & text_flag == 1),
    structured_yes_text_no = sum(structured_yes & text_flag == 0),
    structured_no_text_yes = sum(structured_no & text_flag == 1),
    structured_no_text_no = sum(structured_no & text_flag == 0),
    structured_missing_text_yes = sum(structured_missing & text_flag == 1),
    structured_missing_text_no = sum(structured_missing & text_flag == 0),
    text_existing_total_n = sum(text_flag == 1),
    additional_text_capture_n = sum((structured_no | structured_missing) & text_flag == 1),
    additional_text_capture_pct_of_text_mapped = ifelse(
      sum(text_flag == 1) > 0,
      sum((structured_no | structured_missing) & text_flag == 1) / sum(text_flag == 1),
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
})
existing_overlap_summary <- do.call(rbind, overlap_rows)

# Add labels where available.
if (file.exists(existing_prevalence_csv)) {
  existing_prev <- read.csv(existing_prevalence_csv, stringsAsFactors = FALSE, check.names = FALSE)
  label_cols <- intersect(c("option_label", "desc_short", "description"), names(existing_prev))
  if ("option_number" %in% names(existing_prev) && length(label_cols) > 0) {
    labels <- existing_prev[, c("option_number", label_cols[1]), drop = FALSE]
    names(labels)[2] <- "option_label"
    existing_overlap_summary <- merge(
      existing_overlap_summary, labels, by = "option_number", all.x = TRUE, sort = FALSE
    )
  }
}

write.csv(
  existing_overlap_summary,
  file.path(stage_dir, "existing_overlap_summary.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 10. B. NOVEL THEMES: COUNTS, PREVALENCE AND BENCHMARK
# ----------------------------
free_text_denominator <- nrow(participant_binary)
non_outlier_denominator <- sum(participant_binary$topic_outlier == 0)

# The methodological benchmark is the minimum option-specific prevalence,
# excluding Other (19) and Prefer not to say (20).
benchmark_prevalence <- NA_real_
benchmark_option <- NA_integer_
benchmark_label <- NA_character_

if (file.exists(existing_prevalence_csv)) {
  existing_prev <- read.csv(existing_prevalence_csv, stringsAsFactors = FALSE, check.names = FALSE)
  benchmark_candidates <- existing_prev[
    !(existing_prev$option_number %in% c(19, 20)) &
      !is.na(existing_prev$prevalence_option_specific) &
      existing_prev$selected_n > 0,
    , drop = FALSE
  ]
  benchmark_row <- benchmark_candidates[
    which.min(benchmark_candidates$prevalence_option_specific),
    , drop = FALSE
  ]
  benchmark_prevalence <- benchmark_row$prevalence_option_specific[1]
  benchmark_option <- benchmark_row$option_number[1]
  if ("option_label" %in% names(benchmark_row)) {
    benchmark_label <- benchmark_row$option_label[1]
  }
}

novel_summary_rows <- lapply(seq_len(nrow(novel_theme_dictionary)), function(i) {
  theme <- novel_theme_dictionary$final_theme[i]
  variable <- novel_theme_dictionary$variable[i]
  included_topics <- sort(mapping$topic_id[
    mapping$existing == "No" & mapping$final_theme == theme
  ])
  n_theme <- sum(participant_binary[[variable]] == 1)

  data.frame(
    final_theme = theme,
    variable = variable,
    included_topic_ids = paste(included_topics, collapse = ", "),
    n_participants = n_theme,
    valid_free_text_denominator = free_text_denominator,
    prevalence_all_valid_free_text = n_theme / free_text_denominator,
    non_outlier_denominator = non_outlier_denominator,
    prevalence_among_non_outliers = n_theme / non_outlier_denominator,
    minimum_existing_option_prevalence = benchmark_prevalence,
    benchmark_option_number = benchmark_option,
    benchmark_option_label = benchmark_label,
    above_minimum_existing_benchmark = ifelse(
      is.na(benchmark_prevalence), NA, (n_theme / free_text_denominator) >= benchmark_prevalence
    ),
    stringsAsFactors = FALSE
  )
})
novel_theme_summary <- do.call(rbind, novel_summary_rows)
novel_theme_summary <- novel_theme_summary[
  order(-novel_theme_summary$n_participants),
  , drop = FALSE
]

write.csv(
  novel_theme_summary,
  file.path(stage_dir, "novel_theme_summary.csv"),
  row.names = FALSE,
  na = ""
)

# Topic-level audit showing exactly how 19 novel topics became six themes.
novel_topic_grouping <- mapping[mapping$existing == "No", , drop = FALSE]
keep_cols <- intersect(
  c("topic_id", "topic_size", "bertopic_label", "keywords", "novel_theme", "final_theme", "confidence", "notes"),
  names(novel_topic_grouping)
)
novel_topic_grouping <- novel_topic_grouping[, keep_cols, drop = FALSE]
write.csv(
  novel_topic_grouping,
  file.path(stage_dir, "novel_topic_to_final_theme_mapping.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 11. ANALYSIS-READY FREE-TEXT COHORT DATASETS
# ----------------------------
# Primary analysis matrix:
# 23 augmented existing reasons + six novel themes.
primary_cols <- c(
  "u_passcode", "response_id", "topic_id", "topic_outlier",
  paste0("augmented_existing_", 1:23),
  novel_cols
)
primary_freetext <- participant_binary[, primary_cols, drop = FALSE]

# Audit/comparison dataset retains original, text-derived and augmented indicators.
audit_cols <- c(
  "u_passcode", "response_id", "topic_id", "topic_outlier",
  combined_vars,
  paste0("text_existing_", 1:23),
  paste0("augmented_existing_", 1:23),
  novel_cols
)
audit_freetext <- participant_binary[, audit_cols, drop = FALSE]

write.csv(
  primary_freetext,
  file.path(stage_dir, "downstream_primary_augmented_freetext_cohort.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  audit_freetext,
  file.path(stage_dir, "downstream_audit_freetext_cohort.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 12. MAP FLAGS TO THE FULL RDS FOR LATER DOWNSTREAM ANALYSIS
# ----------------------------
# Merge only newly created fields onto the full participant dataset.
new_mapping_cols <- c(
  "u_passcode", "topic_id", "topic_outlier",
  paste0("text_existing_", 1:23),
  novel_cols,
  "any_text_existing", "any_novel_theme",
  paste0("augmented_existing_", 1:23)
)
new_mapping <- participant_binary[, new_mapping_cols, drop = FALSE]

# For participants without valid free text:
#   - topic_id is NA;
#   - topic_outlier is 0;
#   - text/novel indicators are 0 (theme not expressed in valid Other text);
#   - augmented indicators are then reconstructed from original structured data.
df$u_passcode <- as.character(df$u_passcode)
full_mapped <- merge(df, new_mapping, by = "u_passcode", all.x = TRUE, sort = FALSE)

binary_zero_cols <- c(
  "topic_outlier",
  paste0("text_existing_", 1:23),
  novel_cols,
  "any_text_existing", "any_novel_theme"
)
for (v in binary_zero_cols) {
  full_mapped[[v]][is.na(full_mapped[[v]])] <- 0L
  full_mapped[[v]] <- as.integer(full_mapped[[v]])
}

# Rebuild augmented variables for everybody so structured missingness is preserved.
for (k in 1:23) {
  structured_col <- paste0("vaccrufuse3_", k)
  text_col <- paste0("text_existing_", k)
  augmented_col <- paste0("augmented_existing_", k)

  full_mapped[[augmented_col]] <- ifelse(
    full_mapped[[text_col]] == 1,
    1L,
    ifelse(
      is.na(full_mapped[[structured_col]]),
      NA_integer_,
      as.integer(full_mapped[[structured_col]] == 1)
    )
  )
}

saveRDS(
  full_mapped,
  file.path(stage_dir, "df_with_text_and_novel_theme_variables.rds")
)

# A small data dictionary for downstream use.
variable_dictionary <- rbind(
  data.frame(
    variable = paste0("text_existing_", 1:23),
    role = "Free-text-derived existing reason",
    definition = paste0(
      "1 if the participant's BERTopic topic was manually mapped to VACCRUFUSE3_",
      1:23,
      "; 0 otherwise"
    ),
    primary_or_sensitivity = "Overlap/discrepancy analysis",
    stringsAsFactors = FALSE
  ),
  data.frame(
    variable = novel_theme_dictionary$variable,
    role = "Novel free-text theme",
    definition = novel_theme_dictionary$final_theme,
    primary_or_sensitivity = "Primary downstream analysis",
    stringsAsFactors = FALSE
  ),
  data.frame(
    variable = paste0("augmented_existing_", 1:23),
    role = "Augmented existing reason",
    definition = paste0(
      "1 if original vaccrufuse3_", 1:23,
      " = 1 OR corresponding free-text existing indicator = 1"
    ),
    primary_or_sensitivity = "Primary downstream analysis",
    stringsAsFactors = FALSE
  )
)
write.csv(
  variable_dictionary,
  file.path(stage_dir, "downstream_variable_dictionary.csv"),
  row.names = FALSE
)

# ----------------------------
# 13. FINAL RUN SUMMARY
# ----------------------------
run_summary <- data.frame(
  measure = c(
    "valid_free_text_participants",
    "outlier_participants",
    "participants_mapped_to_existing_option",
    "participants_mapped_to_novel_theme",
    "number_existing_topic_clusters",
    "number_novel_topic_clusters",
    "number_final_novel_themes",
    "full_dataset_rows_exported"
  ),
  value = c(
    nrow(participant_binary),
    sum(participant_binary$topic_outlier),
    sum(participant_binary$any_text_existing),
    sum(participant_binary$any_novel_theme),
    sum(mapping$existing == "Yes"),
    sum(mapping$existing == "No"),
    nrow(novel_theme_dictionary),
    nrow(full_mapped)
  ),
  stringsAsFactors = FALSE
)
write.csv(run_summary, file.path(stage_dir, "run_summary.csv"), row.names = FALSE)

cat("\nStep 06 completed successfully.\n")
cat("Outputs saved to: ", stage_dir, "\n", sep = "")
cat("Primary downstream data: downstream_primary_augmented_freetext_cohort.csv\n")
cat("Full enriched RDS: df_with_text_and_novel_theme_variables.rds\n")
cat("Overlap table: existing_overlap_summary.csv\n")
cat("Novel-theme table: novel_theme_summary.csv\n")
