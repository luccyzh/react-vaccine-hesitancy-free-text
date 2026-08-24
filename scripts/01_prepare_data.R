# ============================================================
# 01_prepare_data.R
# Purpose:
#   1) Load already-cleaned dfRes(from previous 2024 project) RDS.
#   2) Reproduce the confirmed sample-flow counts.
#   3) Create the canonical response-level BERTopic input for this project.
#   4) Exploratory Analysis: Export EDA, word frequencies, word clouds, Table 1 and threshold files.
#
# Analysis definitions
#   Overall hesitant cohort:
#     analysis_4_vax_any_hes_or_refused_vs_vaxxed == 1
#   Reasons-checklist cohort:
#     at least one vaccrufuse3_1 ... vaccrufuse3_23 is non-missing
#   Selected Other:
#     vaccrufuse3_19 == 1
#   BERTopic free-text input:
#     use original VACCRUFUSE1/2 Other flags and text columns, because
#     vaccrufuse3_19 does not preserve which question supplied the text.
# ============================================================

# ----------------------------
# 0. PATHS
# ----------------------------
project_root <- "D:/projects/hda_2026/han"

# Raw dataset pathway
raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"

# Option(s) mapping key
data_key_csv <- "D:/projects/vaccine_hesitancy_2024/vaccine_hesitancy_data_key.csv"

interim_dir <- file.path(project_root, "data", "interim")
stage_dir <- file.path(project_root, "outputs", "01_data_preparation")
fig_dir <- file.path(stage_dir, "figures")

dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1. PACKAGES
# ----------------------------
required_packages <- c("ggplot2", "stopwords", "wordcloud", "RColorBrewer")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Ask the enclave administrator to install them."
  )
}

library(ggplot2)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

# ------------------------------
# 2. LOAD DATA  df[1137927,197]
# ------------------------------
if (!file.exists(raw_rds)) stop("RDS not found: ", raw_rds)
df <- readRDS(raw_rds)

if (!file.exists(data_key_csv)) {
  warning("Data key not found. Threshold labels will be unavailable: ", data_key_csv)
  data_key <- NULL
} else {
  data_key <- read.csv(data_key_csv, stringsAsFactors = FALSE, check.names = FALSE)
}

write.csv(
  data.frame(
    measure = c(
      "rows_in_loaded_clean_dfRes",
      "columns_in_loaded_clean_dfRes",
      "published_final_analytic_cohort"
    ),
    value = c(nrow(df), ncol(df), 1137927),
    note = c(
      "Actual RDS row count",
      "Actual RDS column count",
      "Published comparator only; do not hard-filter to this number"
    )
  ),
  file.path(stage_dir, "source_dataset_audit.csv"),
  row.names = FALSE
)

# ----------------------------
# 3. REQUIRED VARIABLES
# ----------------------------
combined_vars <- paste0("vaccrufuse3_", 1:23)
required_vars <- c(
  "u_passcode",
  "analysis_4_vax_any_hes_or_refused_vs_vaxxed",
  combined_vars,
  "vaccrufuse1_19", "vaccrufuse2_19",
  "vaccrufuse1_19_other", "vaccrufuse2_19_other"
)
missing_required <- setdiff(required_vars, names(df))
if (length(missing_required) > 0) {
  stop("Missing required variables: ", paste(missing_required, collapse = ", "))
}

# ----------------------------
# 4. POPULATION DEFINITIONS
# ----------------------------
overall_hesitant <-
  df$analysis_4_vax_any_hes_or_refused_vs_vaxxed == 1

reason_checklist_eligible <-
  rowSums(!is.na(df[combined_vars])) > 0

selected_other_any <-
  df$vaccrufuse3_19 == 1

# Handbook routing retained as QC only.
asked_refuse1_routing <- if ("vaccineaccept" %in% names(df)) {
  df$vaccineaccept %in% c(2, 3)
} else {
  rep(NA, nrow(df))
}

asked_refuse2_routing <- if ("vaccineapp2" %in% names(df)) {
  df$vaccineapp2 == 2
} else {
  rep(NA, nrow(df))
}

routing_union <- asked_refuse1_routing | asked_refuse2_routing
routing_overlap <- asked_refuse1_routing & asked_refuse2_routing

# ----------------------------
# 5. EXTRACT VALID FREE TEXT
# ----------------------------
other_1 <- df$vaccrufuse1_19 == 1
other_2 <- df$vaccrufuse2_19 == 1

original_text_1 <- as.character(df$vaccrufuse1_19_other)
original_text_2 <- as.character(df$vaccrufuse2_19_other)

# Deliberately minimal cleaning: trim only. Preserve original text separately.
model_text_1 <- trimws(original_text_1)
model_text_2 <- trimws(original_text_2)

invalid_codes <- c("-66", "-77", "-91", "-92")
valid_text_1 <- other_1 & !is.na(model_text_1) & model_text_1 != "" & !(model_text_1 %in% invalid_codes)
valid_text_2 <- other_2 & !is.na(model_text_2) & model_text_2 != "" & !(model_text_2 %in% invalid_codes)

make_response_table <- function(participant_id, question, original_text, model_text, valid_flag) {
  out <- data.frame(
    participant_id = as.character(participant_id),
    question = question,
    original_text = original_text,
    model_text = model_text,
    valid_flag = valid_flag,
    stringsAsFactors = FALSE
  )
  out <- out[out$valid_flag %in% TRUE, , drop = FALSE]
  out$valid_flag <- NULL
  out$response_id <- paste(out$participant_id, out$question, sep = "__")
  out$n_char <- nchar(out$model_text)
  out$n_word <- lengths(gregexpr("[[:alpha:]]+", out$model_text))
  out[, c(
    "participant_id", "response_id", "question",
    "original_text", "model_text", "n_char", "n_word"
  )]
}

response_1 <- make_response_table(
  df$u_passcode, "VACREFUSE1", original_text_1, model_text_1, valid_text_1
)
response_2 <- make_response_table(
  df$u_passcode, "VACREFUSE2", original_text_2, model_text_2, valid_text_2
)
response_df <- rbind(response_1, response_2)
row.names(response_df) <- NULL

if (any(is.na(response_df$participant_id)) || any(response_df$participant_id == "")) {
  stop("Missing participant_id in BERTopic input")
}
if (anyDuplicated(response_df$response_id) > 0) {
  stop("Duplicate response_id detected. Check whether the RDS has repeated participant-question rows.")
}
if (any(is.na(response_df$model_text)) || any(response_df$model_text == "")) {
  stop("Blank model_text remains after filtering")
}

write.csv(
  response_df,
  file.path(interim_dir, "response_level_input.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 6. SAMPLE FLOW / QC
# ----------------------------
valid_text_participant <- valid_text_1 | valid_text_2

sample_flow <- data.frame(
  stage = c(
    "Loaded cleaned dfRes",
    "Overall vaccine hesitant cohort",
    "Observed reasons-checklist cohort",
    "Routing-derived VACREFUSE1 eligible",
    "Routing-derived VACREFUSE2 eligible",
    "Routing-derived overlap",
    "Routing-derived union",
    "Selected Other (participant level)",
    "Valid VACREFUSE1 free-text responses",
    "Valid VACREFUSE2 free-text responses",
    "Total valid free-text responses",
    "Unique participants with valid free text"
  ),
  n = c(
    nrow(df),
    sum(overall_hesitant, na.rm = TRUE),
    sum(reason_checklist_eligible, na.rm = TRUE),
    sum(asked_refuse1_routing, na.rm = TRUE),
    sum(asked_refuse2_routing, na.rm = TRUE),
    sum(routing_overlap, na.rm = TRUE),
    sum(routing_union, na.rm = TRUE),
    sum(selected_other_any, na.rm = TRUE),
    sum(valid_text_1, na.rm = TRUE),
    sum(valid_text_2, na.rm = TRUE),
    nrow(response_df),
    sum(valid_text_participant, na.rm = TRUE)
  )
)
write.csv(sample_flow, file.path(stage_dir, "sample_flow_and_counts.csv"), row.names = FALSE)

routing_vs_observed <- table(
  routing_eligible = routing_union,
  observed_vaccrufuse3 = reason_checklist_eligible,
  useNA = "ifany"
)
write.csv(
  as.data.frame(routing_vs_observed),
  file.path(stage_dir, "routing_vs_observed_reason_cohort.csv"),
  row.names = FALSE
)

# ----------------------------
# 7. EXISTING OPTION PREVALENCE + THRESHOLD
# ----------------------------
reason_denominator <- sum(reason_checklist_eligible, na.rm = TRUE)
selected_n <- colSums(df[combined_vars] == 1, na.rm = TRUE)
option_nonmissing_n <- colSums(!is.na(df[combined_vars]))

existing_option_prevalence <- data.frame(
  variable = combined_vars,
  option_number = 1:23,
  selected_n = as.integer(selected_n),
  shared_reason_cohort_n = reason_denominator,
  prevalence_shared_denominator = as.numeric(selected_n / reason_denominator),
  option_nonmissing_n = as.integer(option_nonmissing_n),
  prevalence_option_specific = as.numeric(selected_n / option_nonmissing_n),
  stringsAsFactors = FALSE
)

# Add labels from data key where possible.
# Data-key column names are handled defensively because the schema may vary across versions.
# Candidate code and label columns are checked before use.
if (!is.null(data_key)) {
  code_col <- intersect(c("code_3", "code", "code_new"), names(data_key))
  label_col <- intersect(c("desc_short_old", "desc_short", "desc", "description"), names(data_key))
  if (length(code_col) > 0 && length(label_col) > 0) {
    key_small <- data.frame(
      variable = as.character(data_key[[code_col[1]]]),
      option_label = as.character(data_key[[label_col[1]]]),
      stringsAsFactors = FALSE
    )
    existing_option_prevalence <- merge(
      existing_option_prevalence, key_small,
      by = "variable", all.x = TRUE, sort = FALSE
    )
  } else {
    warning("Data key loaded, but expected code/label columns were not found. Check column names.")
  }
}

write.csv(
  existing_option_prevalence,
  file.path(stage_dir, "existing_option_prevalence.csv"),
  row.names = FALSE
)

# Threshold reference excludes Other (19) and Prefer not to say (20).
threshold_candidates <- existing_option_prevalence[
  !(existing_option_prevalence$option_number %in% c(19, 20)) &
    existing_option_prevalence$selected_n > 0,
  , drop = FALSE
]

threshold_row <- threshold_candidates[
  which.min(threshold_candidates$prevalence_shared_denominator),
  , drop = FALSE
]

threshold_summary <- data.frame(
  threshold_variable = threshold_row$variable,
  threshold_option_number = threshold_row$option_number,
  threshold_selected_n = threshold_row$selected_n,
  denominator = reason_denominator,
  minimum_existing_prevalence = threshold_row$prevalence_shared_denominator,
  rule = "Flag, not automatic exclusion: new topics below this participant prevalence require manual review",
  stringsAsFactors = FALSE
)
if ("option_label" %in% names(threshold_row)) {
  threshold_summary$threshold_option_label <- threshold_row$option_label
}
write.csv(
  threshold_summary,
  file.path(stage_dir, "existing_option_threshold.csv"),
  row.names = FALSE
)

# ----------------------------
# 8. RESPONSE-LENGTH EDA
# ----------------------------
question_counts <- aggregate(
  response_id ~ question,
  data = response_df,
  FUN = length
)
names(question_counts)[2] <- "n_responses"
write.csv(question_counts, file.path(stage_dir, "response_counts_by_question.csv"), row.names = FALSE)

length_summary <- do.call(
  rbind,
  lapply(split(response_df, response_df$question), function(x) {
    data.frame(
      question = unique(x$question),
      n = nrow(x),
      mean_words = mean(x$n_word),
      median_words = median(x$n_word),
      q1_words = as.numeric(quantile(x$n_word, 0.25)),
      q3_words = as.numeric(quantile(x$n_word, 0.75)),
      mean_characters = mean(x$n_char),
      median_characters = median(x$n_char)
    )
  })
)
write.csv(length_summary, file.path(stage_dir, "response_length_summary_by_question.csv"), row.names = FALSE)

p_box <- ggplot(response_df, aes(x = question, y = n_word)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 60)) +
  labs(title = "Free-text response length by question", x = NULL, y = "Number of words") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "response_length_boxplot.png"), p_box, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "response_length_boxplot.pdf"), p_box, width = 7, height = 5)

p_hist <- ggplot(response_df, aes(x = n_word)) +
  geom_histogram(bins = 30) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Distribution of free-text response length", x = "Number of words", y = "Responses") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "response_length_histogram.png"), p_hist, width = 7, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "response_length_histogram.pdf"), p_hist, width = 7, height = 5)

p_hist_q <- ggplot(response_df, aes(x = n_word)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ question) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Response length by question", x = "Number of words", y = "Responses") +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "response_length_by_question.png"), p_hist_q, width = 9, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "response_length_by_question.pdf"), p_hist_q, width = 9, height = 5)

# ----------------------------
# 9. WORD FREQUENCY + WORD CLOUD
# ----------------------------
clean_words <- function(text_vector) {
  all_text <- tolower(paste(text_vector, collapse = " "))
  all_text <- gsub("[^A-Za-z]", " ", all_text)
  words <- unlist(strsplit(all_text, " "))
  words <- words[words != "" & nchar(words) > 2]
  project_stop_words <- c(
    "covid", "coronavirus", "vaccine", "vaccines", "vaccination", "jab", "jabs"
  )
  extra_stop_words <- c(
    "don", "get", "want", "know", "take", "can", "one", "think",
    "like", "enough", "feel", "also"
  )
  words[!(words %in% c(stopwords("en"), project_stop_words, extra_stop_words))]
}

export_word_outputs <- function(text_vector, prefix, title_text) {
  words <- clean_words(text_vector)
  freq <- sort(table(words), decreasing = TRUE)
  top_df <- data.frame(
    word = names(head(freq, 30)),
    frequency = as.numeric(head(freq, 30)),
    stringsAsFactors = FALSE
  )
  write.csv(top_df, file.path(stage_dir, paste0(prefix, "_top_30_words.csv")), row.names = FALSE)

  p <- ggplot(top_df, aes(x = reorder(word, frequency), y = frequency)) +
    geom_col() +
    coord_flip() +
    labs(title = title_text, x = NULL, y = "Frequency") +
    theme_minimal(base_size = 12)
  ggsave(file.path(fig_dir, paste0(prefix, "_top_30_words.png")), p, width = 7, height = 7, dpi = 300)
  ggsave(file.path(fig_dir, paste0(prefix, "_top_30_words.pdf")), p, width = 7, height = 7)

  png(file.path(fig_dir, paste0(prefix, "_wordcloud.png")), width = 1800, height = 1200, res = 200)
  wordcloud(
    words = names(freq),
    freq = as.numeric(freq),
    min.freq = 2,
    max.words = 150,
    random.order = FALSE,
    rot.per = 0.1,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
}

export_word_outputs(response_df$model_text, "all_responses", "Top words in all free-text responses")
export_word_outputs(response_df$model_text[response_df$question == "VACREFUSE1"], "VACREFUSE1", "Top words in VACREFUSE1")
export_word_outputs(response_df$model_text[response_df$question == "VACREFUSE2"], "VACREFUSE2", "Top words in VACREFUSE2")

# ----------------------------
# 10. TABLE 1 (NO PRIVATE OVERREACT FUNCTIONS)
# ----------------------------
# Variables copied from 2024' project as a reference.
# The code skips unavailable columns and writes them to a QC file.
table1_vars_requested <- c(
  "age_group_named", "sex", "bmi_cat", "ethnic_new",
  "imd_quintile_cat", "edu_cat", "empl_cat_new", "region_named",
  "face_covering", "mask_indoors", "mask_outdoors", "shielding",
  "smokenow", "res_antibody", "res_antigen",
  "healtha_15", "healtha_16", "healtha_17"
)

table1_vars_available <- intersect(table1_vars_requested, names(df))
table1_vars_missing <- setdiff(table1_vars_requested, names(df))
write.csv(
  data.frame(variable = table1_vars_missing),
  file.path(stage_dir, "table1_missing_requested_variables.csv"),
  row.names = FALSE
)

# Replace missing categories with 'Not specified' without changing df permanently.
normalise_table1_value <- function(x) {
  y <- as.character(x)
  y[is.na(y) | trimws(y) == "" | tolower(trimws(y)) %in% c("unknown", "pna", "prefer not to say")] <- "Not specified"
  y
}

build_descriptive_table <- function(data, group, variables) {
  output <- list()
  k <- 1
  group <- factor(group)
  group_levels <- levels(group)

  for (v in variables) {
    x <- data[[v]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) > 10) {
      for (g in group_levels) {
        vals <- x[group == g]
        output[[k]] <- data.frame(
          characteristic = v,
          level = "Mean (SD)",
          group = g,
          value = sprintf("%.1f (%.1f)", mean(vals, na.rm = TRUE), sd(vals, na.rm = TRUE)),
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    } else {
      x2 <- normalise_table1_value(x)
      levels_x <- sort(unique(x2))
      for (lev in levels_x) {
        for (g in group_levels) {
          idx <- group == g
          n <- sum(x2[idx] == lev, na.rm = TRUE)
          denom <- sum(idx)
          pct <- if (denom > 0) 100 * n / denom else NA_real_
          output[[k]] <- data.frame(
            characteristic = v,
            level = lev,
            group = g,
            value = sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct),
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
    }
  }
  do.call(rbind, output)
}

reason_df <- df[reason_checklist_eligible, , drop = FALSE]
reason_selected_other <- reason_df$vaccrufuse3_19 == 1
reason_valid_text <- valid_text_participant[reason_checklist_eligible]

table1_other <- build_descriptive_table(
  reason_df,
  ifelse(reason_selected_other, "Selected Other", "Did not select Other"),
  table1_vars_available
)
write.csv(table1_other, file.path(stage_dir, "table1_selected_other_vs_not.csv"), row.names = FALSE)

table1_text <- build_descriptive_table(
  reason_df,
  ifelse(reason_valid_text, "Valid free text", "No valid free text"),
  table1_vars_available
)
write.csv(table1_text, file.path(stage_dir, "table1_valid_freetext_vs_not.csv"), row.names = FALSE)

write.csv(
  data.frame(
    population = c(
      "Reasons-checklist cohort",
      "Selected Other",
      "Did not select Other",
      "Valid free text",
      "No valid free text"
    ),
    n = c(
      nrow(reason_df),
      sum(reason_selected_other, na.rm = TRUE),
      sum(!reason_selected_other, na.rm = TRUE),
      sum(reason_valid_text, na.rm = TRUE),
      sum(!reason_valid_text, na.rm = TRUE)
    )
  ),
  file.path(stage_dir, "table1_population_audit.csv"),
  row.names = FALSE
)

# ----------------------------
# 11. SESSION INFO
# ----------------------------
capture.output(sessionInfo(), file = file.path(stage_dir, "R_session_info.txt"))
message("01_prepare_data.R completed successfully.")
message("Canonical input: ", file.path(interim_dir, "response_level_input.csv"))
message("Stage outputs: ", stage_dir)
