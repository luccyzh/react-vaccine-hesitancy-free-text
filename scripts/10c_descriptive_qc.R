# Round-aware descriptive QC of free-text mappings, structured response behaviour, additional themes and questionnaire availability.

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)
required_pkgs <- c("dplyr", "tidyr", "readr", "stringr", "ggplot2")
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  stop("Missing package(s): ", paste(missing_pkgs, collapse = ", "))
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})
project_root <- "D:/projects/hda_2026/han"
output_root  <- file.path(project_root, "outputs")
questions_csv <- file.path(project_root, "questions.csv")
raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"
enriched_rds <- file.path(
  output_root,
  "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)
participant_topic_candidates <- c(
  file.path(output_root, "06_participant_mapping", "participant_topic_mapping.csv"),
  file.path(output_root, "06_participant_mapping", "final_document_topics.csv"),
  file.path(output_root, "05_topic_review", "final_document_topics.csv")
)
primary_dictionary_candidates <- c(
  file.path(
    output_root,
    "07_augmented_clustering",
    "primary_without_other",
    "clustering_variable_dictionary.csv"
  )
)
out_dir   <- file.path(output_root, "10c_post_meeting_descriptive_qc_v3")
qc_dir    <- file.path(out_dir, "qc")
other_dir <- file.path(out_dir, "other_response_behaviour")
map_dir   <- file.path(out_dir, "mapping_behaviour")
novel_dir <- file.path(out_dir, "novel_theme_descriptives")
cor_dir   <- file.path(out_dir, "correlation")
round_dir <- file.path(out_dir, "round_audit")
fig_dir   <- file.path(out_dir, "figures")
for (d in c(out_dir, qc_dir, other_dir, map_dir, novel_dir, cor_dir, round_dir, fig_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
first_existing <- function(candidates, nms) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}
first_existing_file <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}
as_binary <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  y <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(y))
  out[y %in% c("1","yes","true","y","t")] <- 1L
  out[y %in% c("0","no","false","n","f")] <- 0L
  suppressWarnings({
    num <- as.numeric(y)
    out[is.na(out) & !is.na(num) & num == 1] <- 1L
    out[is.na(out) & !is.na(num) & num == 0] <- 0L
  })
  out
}
pct <- function(n, d) {
  ifelse(d > 0, 100 * n / d, NA_real_)
}
save_plot_both <- function(plot, stem, width, height) {
  ggsave(
    file.path(fig_dir, paste0(stem, ".png")),
    plot = plot, width = width, height = height, dpi = 320, bg = "white"
  )
  ggsave(
    file.path(fig_dir, paste0(stem, ".pdf")),
    plot = plot, width = width, height = height, bg = "white"
  )
}
normalise_survey_key <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z <- gsub("[ -]", "_", z)
  z <- gsub("__+", "_", z)
  out <- rep(NA_character_, length(z))
  idx <- grepl("^REACT1_R[0-9]{2}$", z)
  out[idx] <- z[idx]
  idx <- grepl("^REACT2_S5_R[0-9]{2}$", z)
  out[idx] <- z[idx]
  idx <- is.na(out) & grepl("REACT.?1", z) & grepl("([R_]|ROUND_?)(0?[0-9]{1,2})", z)
  if (any(idx)) {
    nums <- stringr::str_match(z[idx], "(?:R|ROUND_?)(0?[0-9]{1,2})")[,2]
    out[idx] <- sprintf("REACT1_R%02d", as.integer(nums))
  }
  idx <- is.na(out) & grepl("REACT.?2", z) & grepl("([R_]|ROUND_?)(0?[0-9]{1,2})", z)
  if (any(idx)) {
    nums <- stringr::str_match(z[idx], "(?:R|ROUND_?)(0?[0-9]{1,2})")[,2]
    out[idx] <- sprintf("REACT2_S5_R%02d", as.integer(nums))
  }
  out
}
participant_topic_file <- first_existing_file(participant_topic_candidates)
primary_dictionary_file <- first_existing_file(primary_dictionary_candidates)
manifest <- data.frame(
  object = c(
    "questions_csv",
    "raw_rds",
    "enriched_rds",
    "participant_topic_file",
    "primary_dictionary_file"
  ),
  path = c(
    questions_csv,
    raw_rds,
    enriched_rds,
    participant_topic_file,
    primary_dictionary_file
  ),
  exists = c(
    file.exists(questions_csv),
    file.exists(raw_rds),
    file.exists(enriched_rds),
    !is.na(participant_topic_file),
    !is.na(primary_dictionary_file)
  )
)
write_csv(manifest, file.path(qc_dir, "input_manifest.csv"))
print(manifest)
if (!file.exists(questions_csv)) {
  stop(
    "questions.csv not found at the expected path:\n",
    questions_csv
  )
}
if (!file.exists(raw_rds)) stop("Raw RDS not found: ", raw_rds)
if (!file.exists(enriched_rds)) stop("Enriched participant RDS not found: ", enriched_rds)
questions <- read_csv(questions_csv, show_col_types = FALSE)
raw <- readRDS(raw_rds)
enriched <- readRDS(enriched_rds)
required_q_cols <- c("survey", "variable")
if (!all(required_q_cols %in% names(questions))) {
  stop("questions.csv must contain: survey, variable")
}
q_avail <- questions %>%
  mutate(
    survey = as.character(survey),
    variable = toupper(as.character(variable))
  ) %>%
  filter(
    str_detect(variable, "^VACCRUFUSE[12]_[0-9]+$")
  ) %>%
  mutate(
    source_question = str_match(variable, "^VACCRUFUSE([12])_")[,2],
    reason_num = as.integer(str_match(variable, "_([0-9]+)$")[,2])
  ) %>%
  filter(reason_num >= 1, reason_num <= 23) %>%
  distinct(survey, source_question, reason_num) %>%
  group_by(survey, reason_num) %>%
  summarise(
    source1_available = any(source_question == "1"),
    source2_available = any(source_question == "2"),
    option_available = source1_available | source2_available,
    .groups = "drop"
  )
surveys_with_vax <- sort(unique(q_avail$survey))
official_availability <- tidyr::expand_grid(
  survey = surveys_with_vax,
  reason_num = 1:23
) %>%
  left_join(q_avail, by = c("survey", "reason_num")) %>%
  mutate(
    source1_available = replace_na(source1_available, FALSE),
    source2_available = replace_na(source2_available, FALSE),
    option_available = replace_na(option_available, FALSE)
  )
write_csv(
  official_availability,
  file.path(round_dir, "OFFICIAL_vaccrufuse_option_availability_from_questions.csv")
)
official_availability_wide <- official_availability %>%
  select(survey, reason_num, option_available) %>%
  mutate(option_available = ifelse(option_available, "Available", "Unavailable")) %>%
  pivot_wider(names_from = survey, values_from = option_available)
write_csv(
  official_availability_wide,
  file.path(round_dir, "OFFICIAL_vaccrufuse_option_availability_wide.csv")
)
availability_summary <- official_availability %>%
  group_by(reason_num) %>%
  summarise(
    available_surveys = paste(survey[option_available], collapse = "; "),
    unavailable_surveys = paste(survey[!option_available], collapse = "; "),
    n_surveys_available = sum(option_available),
    n_surveys_total = n(),
    .groups = "drop"
  )
write_csv(
  availability_summary,
  file.path(round_dir, "OFFICIAL_reason_availability_summary.csv")
)
id_candidates <- c(
  "u_passcode","U_PASSCODE","participant_id","participantid",
  "person_id","uid","ID","id"
)
id_raw <- first_existing(id_candidates, names(raw))
id_enr <- first_existing(id_candidates, names(enriched))
if (is.na(id_raw) || is.na(id_enr)) {
  stop("Could not identify participant ID.")
}
raw$.__pid__ <- as.character(raw[[id_raw]])
enriched$.__pid__ <- as.character(enriched[[id_enr]])
survey_candidates <- c(
  "survey", "survey_name", "survey_round", "study_round", "round"
)
survey_col <- first_existing(survey_candidates, names(raw))
if (is.na(survey_col)) {
  stop(
    "Could not identify survey/round variable in raw data.\n",
    "Candidates checked: ", paste(survey_candidates, collapse = ", ")
  )
}
raw$.__survey_raw__ <- as.character(raw[[survey_col]])
raw$.__survey_key__ <- normalise_survey_key(raw$.__survey_raw__)
exact_match <- raw$.__survey_raw__ %in% official_availability$survey
raw$.__survey_key__[exact_match] <- raw$.__survey_raw__[exact_match]
survey_mapping_qc <- data.frame(
  raw_survey_value = sort(unique(raw$.__survey_raw__)),
  normalised_survey_key = normalise_survey_key(sort(unique(raw$.__survey_raw__)))
)
survey_mapping_qc$normalised_survey_key[
  survey_mapping_qc$raw_survey_value %in% official_availability$survey
] <- survey_mapping_qc$raw_survey_value[
  survey_mapping_qc$raw_survey_value %in% official_availability$survey
]
survey_mapping_qc$found_in_questions_dictionary <-
  survey_mapping_qc$normalised_survey_key %in% official_availability$survey
write_csv(
  survey_mapping_qc,
  file.path(qc_dir, "survey_key_mapping_qc.csv")
)
if (any(!survey_mapping_qc$found_in_questions_dictionary)) {
  warning(
    "Some participant-data round labels could not be mapped to questions.csv. ",
    "Inspect qc/survey_key_mapping_qc.csv before interpreting round-aware outputs."
  )
}
if ("vaccrufuse3_19" %in% names(raw)) {
  raw$.__other__ <- as_binary(raw$vaccrufuse3_19) == 1
} else if (all(c("vaccrufuse1_19", "vaccrufuse2_19") %in% names(raw))) {
  a <- as_binary(raw$vaccrufuse1_19) == 1
  b <- as_binary(raw$vaccrufuse2_19) == 1
  a[is.na(a)] <- FALSE
  b[is.na(b)] <- FALSE
  raw$.__other__ <- a | b
} else {
  stop("Could not define Selected Other.")
}
raw$.__other__[is.na(raw$.__other__)] <- FALSE
invalid_codes <- c("-66", "-77", "-91", "-92")
raw$.__valid_text__ <- FALSE
if (all(c("vaccrufuse1_19", "vaccrufuse1_19_other") %in% names(raw))) {
  t1 <- trimws(as.character(raw$vaccrufuse1_19_other))
  f1 <- as_binary(raw$vaccrufuse1_19) == 1 &
    !is.na(t1) & t1 != "" & !(t1 %in% invalid_codes)
  f1[is.na(f1)] <- FALSE
  raw$.__valid_text__ <- raw$.__valid_text__ | f1
}
if (all(c("vaccrufuse2_19", "vaccrufuse2_19_other") %in% names(raw))) {
  t2 <- trimws(as.character(raw$vaccrufuse2_19_other))
  f2 <- as_binary(raw$vaccrufuse2_19) == 1 &
    !is.na(t2) & t2 != "" & !(t2 %in% invalid_codes)
  f2[is.na(f2)] <- FALSE
  raw$.__valid_text__ <- raw$.__valid_text__ | f2
}
reason_labels <- c(
  "1"  = "General side-effect concerns",
  "2"  = "Effectiveness / more evidence",
  "3"  = "Long-term health effects",
  "4"  = "Travel barriers",
  "5"  = "Difficulty reaching vaccination centre",
  "6"  = "Low perceived personal COVID-19 risk",
  "7"  = "Existing health condition",
  "8"  = "Against vaccines in general",
  "9"  = "Doubts vaccine will work personally",
  "10" = "Concern vaccine may cause COVID-19",
  "11" = "Fear of pain or needles",
  "12" = "Concern about feeling ill",
  "13" = "Prior COVID-19 / vaccination not needed",
  "14" = "Pregnancy or breastfeeding",
  "15" = "COVID-19 impact perceived as exaggerated",
  "16" = "Distrust in vaccine developers / development",
  "17" = "Others vaccinated, so own vaccination unnecessary",
  "18" = "Others need limited doses more",
  "19" = "Other",
  "20" = "Prefer not to say",
  "21" = "Fertility / trying to conceive",
  "22" = "Allergic reaction concerns",
  "23" = "Previous bad vaccine reaction"
)
availability_labeled <- official_availability %>%
  mutate(reason_label = unname(reason_labels[as.character(reason_num)])) %>%
  select(survey, reason_num, reason_label, everything())
write_csv(
  availability_labeled,
  file.path(round_dir, "OFFICIAL_vaccrufuse_option_availability_LABELED.csv")
)
orig_cols <- paste0("vaccrufuse3_", 1:23)
orig_cols <- orig_cols[orig_cols %in% names(raw)]
orig_nonother_cols <- setdiff(orig_cols, "vaccrufuse3_19")
orig_bin <- as.data.frame(lapply(raw[orig_nonother_cols], as_binary))
for (nm in names(orig_bin)) {
  orig_bin[[nm]][is.na(orig_bin[[nm]])] <- 0L
}
raw$.__n_structured_nonother__ <- rowSums(orig_bin)
raw$.__any_structured_nonother__ <- raw$.__n_structured_nonother__ > 0
other_summary <- data.frame(
  measure = c(
    "All participants",
    "Selected Other",
    "Did not select Other",
    "Selected Other with valid free text",
    "Selected Other without valid free text",
    "Selected Other only (no other structured reason selected)",
    "Selected Other + >=1 other structured reason selected"
  ),
  n = c(
    nrow(raw),
    sum(raw$.__other__),
    sum(!raw$.__other__),
    sum(raw$.__other__ & raw$.__valid_text__),
    sum(raw$.__other__ & !raw$.__valid_text__),
    sum(raw$.__other__ & !raw$.__any_structured_nonother__, na.rm = TRUE),
    sum(raw$.__other__ & raw$.__any_structured_nonother__, na.rm = TRUE)
  )
)
other_summary$percent_of_other <- c(
  NA, 100, NA,
  pct(other_summary$n[4], other_summary$n[2]),
  pct(other_summary$n[5], other_summary$n[2]),
  pct(other_summary$n[6], other_summary$n[2]),
  pct(other_summary$n[7], other_summary$n[2])
)
write_csv(
  other_summary,
  file.path(other_dir, "other_response_behaviour_summary.csv")
)
structured_keep <- data.frame(
  .__pid__ = raw$.__pid__,
  survey_raw = raw$.__survey_raw__,
  survey_key = raw$.__survey_key__,
  selected_other = raw$.__other__,
  valid_free_text = raw$.__valid_text__,
  n_other_structured_reasons = raw$.__n_structured_nonother__,
  stringsAsFactors = FALSE
)
for (j in 1:23) {
  nm <- paste0("vaccrufuse3_", j)
  if (nm %in% names(raw)) {
    structured_keep[[paste0("original_", j)]] <- as_binary(raw[[nm]])
  }
}
enr <- merge(
  enriched,
  structured_keep,
  by = ".__pid__",
  all.x = TRUE,
  sort = FALSE
)
text_var_candidates <- lapply(
  1:23,
  function(j) {
    c(
      paste0("text_existing_", j),
      paste0("text_derived_existing_", j),
      paste0("mapped_existing_", j),
      paste0("text_vaccrufuse3_", j)
    )
  }
)
text_existing_vars <- rep(NA_character_, 23)
for (j in 1:23) {
  text_existing_vars[j] <- first_existing(
    text_var_candidates[[j]],
    names(enr)
  )
}
write_csv(
  data.frame(
    reason_num = 1:23,
    reason_label = unname(reason_labels[as.character(1:23)]),
    text_variable = text_existing_vars
  ),
  file.path(map_dir, "text_existing_variable_manifest.csv")
)
if (sum(!is.na(text_existing_vars)) == 0) {
  stop(
    "No text-derived existing-reason columns were found in enriched data.\n",
    "Inspect mapping_behaviour/text_existing_variable_manifest.csv and ",
    "the names of df_with_text_and_novel_theme_variables.rds."
  )
}
mapping_rows <- list()
event_rows <- list()
kk <- 1
ee <- 1
for (j in 1:23) {
  if (j == 19) next  # broad Other itself is not a mapped substantive reason
  text_nm <- text_existing_vars[j]
  orig_nm <- paste0("original_", j)
  if (is.na(text_nm) || !(orig_nm %in% names(enr))) next
  txt <- as_binary(enr[[text_nm]])
  org <- as_binary(enr[[orig_nm]])
  txt[is.na(txt)] <- 0L
  use <- !is.na(enr$selected_other) &
    enr$selected_other &
    !is.na(enr$valid_free_text) &
    enr$valid_free_text
  avail_lookup_j <- official_availability %>%
    filter(reason_num == j) %>%
    select(survey, option_available)
  avail_map <- setNames(
    avail_lookup_j$option_available,
    avail_lookup_j$survey
  )
  option_available <- unname(avail_map[enr$survey_key])
  mention <- use & txt == 1
  category <- rep(NA_character_, nrow(enr))
  category[
    mention &
      option_available %in% TRUE &
      org == 1
  ] <- "Already ticked (elaboration)"
  category[
    mention &
      option_available %in% TRUE &
      org == 0
  ] <- "Available but not ticked"
  category[
    mention &
      option_available %in% FALSE
  ] <- "Option unavailable (backfilled by free text)"
  category[
    mention &
      is.na(category)
  ] <- "Unresolved / missing structured response"
  n_mentions <- sum(mention)
  event_j <- data.frame(
    participant_id = enr$.__pid__[mention],
    survey = enr$survey_key[mention],
    reason_num = rep(j,n_mentions),
    reason_label = rep(unname(reason_labels[as.character(j)]),n_mentions),
    original_value = org[mention],
    text_derived_value = txt[mention],
    option_available = option_available[mention],
    mapping_category = category[mention],
    stringsAsFactors = FALSE
  )
  event_rows[[ee]] <- event_j
  ee <- ee + 1
  counts <- table(
    factor(
      category[mention],
      levels = c(
        "Already ticked (elaboration)",
        "Available but not ticked",
        "Option unavailable (backfilled by free text)",
        "Unresolved / missing structured response"
      )
    )
  )
  text_derived_n <- sum(mention)
  mapping_rows[[kk]] <- data.frame(
    reason_num = j,
    reason_label = unname(reason_labels[as.character(j)]),
    free_text_mentions_n = text_derived_n,
    already_ticked_n =
      unname(counts["Already ticked (elaboration)"]),
    already_ticked_percent =
      pct(
        unname(counts["Already ticked (elaboration)"]),
        text_derived_n
      ),
    available_not_ticked_n =
      unname(counts["Available but not ticked"]),
    available_not_ticked_percent =
      pct(
        unname(counts["Available but not ticked"]),
        text_derived_n
      ),
    option_unavailable_n =
      unname(counts["Option unavailable (backfilled by free text)"]),
    option_unavailable_percent =
      pct(
        unname(counts["Option unavailable (backfilled by free text)"]),
        text_derived_n
      ),
    unresolved_n =
      unname(counts["Unresolved / missing structured response"]),
    unresolved_percent =
      pct(
        unname(counts["Unresolved / missing structured response"]),
        text_derived_n
      ),
    stringsAsFactors = FALSE
  )
  kk <- kk + 1
}
mapping_roundaware <- do.call(rbind, mapping_rows)
mapping_events <- do.call(rbind, event_rows)
mapping_roundaware <- mapping_roundaware %>%
  arrange(desc(free_text_mentions_n))
write_csv(
  mapping_roundaware,
  file.path(
    map_dir,
    "MAIN_existing_reason_freetext_source_roundaware.csv"
  )
)
write_csv(
  mapping_events,
  file.path(
    map_dir,
    "participant_level_mapping_source_events_for_QC.csv"
  )
)
mapping_plot_long <- mapping_roundaware %>%
  select(
    reason_num,
    reason_label,
    free_text_mentions_n,
    already_ticked_n,
    available_not_ticked_n,
    option_unavailable_n,
    unresolved_n
  ) %>%
  pivot_longer(
    cols = c(
      already_ticked_n,
      available_not_ticked_n,
      option_unavailable_n,
      unresolved_n
    ),
    names_to = "mapping_source",
    values_to = "n"
  ) %>%
  mutate(
    mapping_source = recode(
      mapping_source,
      already_ticked_n = "Already ticked (elaboration)",
      available_not_ticked_n = "Available but not ticked",
      option_unavailable_n = "Option unavailable (backfilled)",
      unresolved_n = "Unresolved / missing"
    ),
    percent = pct(n, free_text_mentions_n)
  )
write_csv(
  mapping_plot_long,
  file.path(
    map_dir,
    "MAIN_existing_reason_freetext_source_roundaware_LONG_for_plot.csv"
  )
)
plot_labels <- mapping_roundaware %>%
  filter(free_text_mentions_n > 0) %>%
  arrange(option_unavailable_percent + available_not_ticked_percent) %>%
  pull(reason_label)
plot_dat <- mapping_plot_long %>%
  filter(
    reason_label %in% plot_labels,
    mapping_source != "Unresolved / missing"
  ) %>%
  mutate(
    reason_label = factor(reason_label, levels = plot_labels)
  )
p_mapping <- ggplot(
  plot_dat,
  aes(x = percent, y = reason_label, fill = mapping_source)
) +
  geom_col(width = 0.72) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    title = "How free text added information beyond structured response options",
    subtitle = paste0(
      "Among free-text mentions mapped to an existing reason, ",
      "classified using official round-specific questionnaire availability"
    ),
    x = "Share of mapped free-text mentions (%)",
    y = NULL,
    fill = NULL
  ) +
  theme_classic(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 8.3, colour = "black"),
    legend.position = "bottom"
  )
save_plot_both(
  p_mapping,
  "QC_existing_reason_freetext_source_roundaware",
  width = 10,
  height = 7.4
)
round_backfill <- mapping_events %>%
  count(
    survey,
    reason_num,
    reason_label,
    mapping_category,
    name = "n"
  ) %>%
  group_by(
    survey,
    reason_num,
    reason_label
  ) %>%
  mutate(
    percent_within_reason_round =
      100 * n / sum(n)
  ) %>%
  ungroup()
write_csv(
  round_backfill,
  file.path(
    round_dir,
    "existing_reason_mapping_source_by_round.csv"
  )
)
backfill_ranking <- mapping_roundaware %>%
  arrange(desc(option_unavailable_n)) %>%
  select(
    reason_num,
    reason_label,
    free_text_mentions_n,
    option_unavailable_n,
    option_unavailable_percent,
    available_not_ticked_n,
    already_ticked_n
  )
write_csv(
  backfill_ranking,
  file.path(
    map_dir,
    "reasons_ranked_by_unavailable_option_backfill.csv"
  )
)
novel_manifest <- data.frame(
  variable = c(
    "novel_brand_preference",
    "novel_access_availability_dosing",
    "novel_ethical_religious_animal",
    "novel_government_distrust_autonomy",
    "novel_mrna_gene_technology",
    "novel_uncertainty_decision_pending"
  ),
  label = c(
    "Vaccine brand preference and choice",
    "Access, availability and dosing arrangements",
    "Ethical, religious and animal-related concerns",
    "Government distrust, coercion and loss of autonomy",
    "Concerns about mRNA and gene-therapy technology",
    "Uncertainty or decision not yet made"
  ),
  stringsAsFactors = FALSE
)
novel_manifest <- novel_manifest[
  novel_manifest$variable %in% names(enr),
]
use_ft <- !is.na(enr$selected_other) &
  enr$selected_other &
  !is.na(enr$valid_free_text) &
  enr$valid_free_text
if (nrow(novel_manifest) > 0) {
  novel_prev <- do.call(
    rbind,
    lapply(
      seq_len(nrow(novel_manifest)),
      function(i) {
        nm <- novel_manifest$variable[i]
        z <- as_binary(enr[[nm]])
        z[is.na(z)] <- 0L
        n <- sum(use_ft & z == 1)
        den <- sum(use_ft)
        data.frame(
          variable = nm,
          novel_theme = novel_manifest$label[i],
          n = n,
          denominator_valid_freetext = den,
          percent_valid_freetext = pct(n, den)
        )
      }
    )
  ) %>%
    arrange(desc(n))
  write_csv(
    novel_prev,
    file.path(
      novel_dir,
      "novel_theme_prevalence_valid_freetext.csv"
    )
  )
}
text_existing_found <- text_existing_vars[!is.na(text_existing_vars)]
x_text <- as.data.frame(lapply(enr[text_existing_found], as_binary))
for (nm in names(x_text)) x_text[[nm]][is.na(x_text[[nm]])] <- 0L
enr$.__any_text_existing__ <- rowSums(x_text) > 0
if (nrow(novel_manifest) > 0) {
  x_novel <- as.data.frame(lapply(enr[novel_manifest$variable], as_binary))
  for (nm in names(x_novel)) x_novel[[nm]][is.na(x_novel[[nm]])] <- 0L
  enr$.__any_novel__ <- rowSums(x_novel) > 0
} else {
  enr$.__any_novel__ <- FALSE
}
info_type <- rep(NA_character_, nrow(enr))
info_type[
  use_ft & enr$.__any_text_existing__ & !enr$.__any_novel__
] <- "Mapped existing reason(s) only"
info_type[
  use_ft & !enr$.__any_text_existing__ & enr$.__any_novel__
] <- "Novel theme(s) only"
info_type[
  use_ft & enr$.__any_text_existing__ & enr$.__any_novel__
] <- "Both existing mapping and novel theme(s)"
info_type[
  use_ft & !enr$.__any_text_existing__ & !enr$.__any_novel__
] <- "Neither retained existing mapping nor novel theme"
info_summary <- as.data.frame(
  table(
    information_type = info_type[use_ft],
    useNA = "ifany"
  )
)
names(info_summary)[2] <- "n"
info_summary$percent <- 100 * info_summary$n / sum(info_summary$n)
write_csv(
  info_summary,
  file.path(
    map_dir,
    "valid_freetext_information_type_summary.csv"
  )
)
if (nrow(novel_manifest) > 0) {
  novel_structured_behaviour <- do.call(
    rbind,
    lapply(
      seq_len(nrow(novel_manifest)),
      function(i) {
        nm <- novel_manifest$variable[i]
        z <- as_binary(enr[[nm]])
        z[is.na(z)] <- 0L
        idx <- use_ft & z == 1
        n_theme <- sum(idx)
        data.frame(
          novel_theme = novel_manifest$label[i],
          n_theme = n_theme,
          no_other_structured_reason_n =
            sum(idx & enr$n_other_structured_reasons == 0, na.rm = TRUE),
          has_other_structured_reason_n =
            sum(idx & enr$n_other_structured_reasons > 0, na.rm = TRUE),
          percent_no_other_structured_reason =
            pct(
              sum(idx & enr$n_other_structured_reasons == 0, na.rm = TRUE),
              n_theme
            ),
          percent_with_other_structured_reason =
            pct(
              sum(idx & enr$n_other_structured_reasons > 0, na.rm = TRUE),
              n_theme
            )
        )
      }
    )
  )
  write_csv(
    novel_structured_behaviour,
    file.path(
      novel_dir,
      "novel_theme_structured_response_behaviour.csv"
    )
  )
}
primary_vars <- character(0)
if (!is.na(primary_dictionary_file)) {
  dd <- read_csv(primary_dictionary_file, show_col_types = FALSE)
  var_col <- first_existing(
    c("variable","Variable","var"),
    names(dd)
  )
  if (!is.na(var_col)) {
    primary_vars <- unique(as.character(dd[[var_col]]))
  }
}
primary_vars <- primary_vars[primary_vars %in% names(enr)]
if (length(primary_vars) >= 2) {
  x <- as.data.frame(lapply(enr[primary_vars], as_binary))
  for (nm in names(x)) x[[nm]][is.na(x[[nm]])] <- 0L
  write.csv(
    cor(x, use = "pairwise.complete.obs"),
    file.path(
      cor_dir,
      "primary_clustering_binary_correlation_matrix.csv"
    )
  )
  write.csv(
    t(as.matrix(x)) %*% as.matrix(x),
    file.path(
      cor_dir,
      "primary_clustering_cooccurrence_counts.csv"
    )
  )
}
if (nrow(novel_manifest) >= 2) {
  xn <- as.data.frame(lapply(enr[novel_manifest$variable], as_binary))
  for (nm in names(xn)) xn[[nm]][is.na(xn[[nm]])] <- 0L
  xn <- xn[use_ft, , drop = FALSE]
  novel_cor <- cor(xn, use = "pairwise.complete.obs")
  novel_cooccur <- t(as.matrix(xn)) %*% as.matrix(xn)
  write.csv(
    novel_cor,
    file.path(
      cor_dir,
      "novel_theme_binary_correlation_matrix_valid_freetext.csv"
    )
  )
  write.csv(
    novel_cooccur,
    file.path(
      cor_dir,
      "novel_theme_cooccurrence_counts_valid_freetext.csv"
    )
  )
  denom <- colSums(xn)
  conditional <- matrix(
    NA_real_,
    nrow = ncol(xn),
    ncol = ncol(xn),
    dimnames = list(names(xn), names(xn))
  )
  for (i in seq_len(ncol(xn))) {
    if (denom[i] > 0) {
      conditional[i, ] <- 100 * novel_cooccur[i, ] / denom[i]
    }
  }
  write.csv(
    conditional,
    file.path(
      cor_dir,
      "novel_theme_conditional_cooccurrence_percent_valid_freetext.csv"
    )
  )
}
if (!is.na(participant_topic_file)) {
  tp <- read.csv(participant_topic_file, check.names = FALSE)
  id_tp <- first_existing(id_candidates, names(tp))
  topic_col <- first_existing(
    c("topic","Topic","topic_id","bertopic_topic","response_topic"),
    names(tp)
  )
  if (!is.na(id_tp) && !is.na(topic_col)) {
    tp$.__pid__ <- as.character(tp[[id_tp]])
    tp$.__outlier__ <- suppressWarnings(as.numeric(tp[[topic_col]])) == -1
    tp$.__outlier__[is.na(tp$.__outlier__)] <- FALSE
    other_ids <- raw$.__pid__[raw$.__other__]
    tp_other <- tp[tp$.__pid__ %in% other_ids, , drop = FALSE]
    outlier_summary <- data.frame(
      measure = c(
        "Other responders represented in participant-topic mapping",
        "Other responders assigned BERTopic outlier (-1)",
        "Other responders assigned non-outlier topic"
      ),
      n = c(
        length(unique(tp_other$.__pid__)),
        length(unique(tp_other$.__pid__[tp_other$.__outlier__])),
        length(unique(tp_other$.__pid__[!tp_other$.__outlier__]))
      )
    )
    outlier_summary$percent <- pct(
      outlier_summary$n,
      outlier_summary$n[1]
    )
    write_csv(
      outlier_summary,
      file.path(
        other_dir,
        "other_bertopic_outlier_summary.csv"
      )
    )
  }
}
other_by_round <- raw %>%
  group_by(
    survey_raw = .__survey_raw__,
    survey = .__survey_key__
  ) %>%
  summarise(
    n_all_respondents = n(),
    selected_other_n = sum(.__other__),
    valid_freetext_n = sum(.__valid_text__),
    selected_other_percent_all =
      100 * mean(.__other__),
    valid_freetext_percent_all =
      100 * mean(.__valid_text__),
    .groups = "drop"
  )
write_csv(
  other_by_round,
  file.path(
    round_dir,
    "other_and_valid_freetext_by_round_all_respondents.csv"
  )
)
mapping_roundaware <- mapping_roundaware %>%
  mutate(
    classified_sum =
      already_ticked_n +
      available_not_ticked_n +
      option_unavailable_n +
      unresolved_n,
    classification_matches_total =
      classified_sum == free_text_mentions_n
  )
write_csv(
  mapping_roundaware,
  file.path(
    qc_dir,
    "mapping_category_sum_check.csv"
  )
)
if (!all(mapping_roundaware$classification_matches_total)) {
  warning(
    "At least one reason does not have mapping categories summing to total mentions. ",
    "Inspect qc/mapping_category_sum_check.csv."
  )
}
cat("\n============================================================\n")
cat("10c DESCRIPTIVE QC V3 COMPLETED\n")
cat("============================================================\n")
cat("Official questionnaire metadata source:\n", questions_csv, "\n\n")
cat("KEY OUTPUT FILES:\n")
cat(
  "1) ",
  file.path(map_dir, "MAIN_existing_reason_freetext_source_roundaware.csv"),
  "\n",
  sep = ""
)
cat(
  "2) ",
  file.path(
    map_dir,
    "MAIN_existing_reason_freetext_source_roundaware_LONG_for_plot.csv"
  ),
  "\n",
  sep = ""
)
cat(
  "3) ",
  file.path(round_dir, "OFFICIAL_vaccrufuse_option_availability_LABELED.csv"),
  "\n",
  sep = ""
)
cat(
  "4) ",
  file.path(round_dir, "existing_reason_mapping_source_by_round.csv"),
  "\n",
  sep = ""
)
cat(
  "5) ",
  file.path(map_dir, "valid_freetext_information_type_summary.csv"),
  "\n",
  sep = ""
)
cat(
  "6) ",
  file.path(novel_dir, "novel_theme_prevalence_valid_freetext.csv"),
  "\n",
  sep = ""
)
cat(
  "7) correlation/*.csv\n"
)
cat(
  "8) ",
  file.path(other_dir, "other_response_behaviour_summary.csv"),
  "\n",
  sep = ""
)
cat("\nSelected Other: ",
    format(sum(raw$.__other__), big.mark = ","), "\n", sep = "")
cat(
  "Valid free text among Other: ",
  format(sum(raw$.__other__ & raw$.__valid_text__), big.mark = ","),
  "\n",
  sep = ""
)
cat("============================================================\n")
