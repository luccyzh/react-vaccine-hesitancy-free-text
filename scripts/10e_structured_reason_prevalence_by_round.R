# ==============================================================================
# 10e_structured_reason_prevalence_by_round.R
# ==============================================================================
#
# Purpose:
#   Export round-specific structured hesitancy-reason counts and prevalence,
#   and combine them with the round-aware free-text recovery outputs from Step 10c.
#
# For each reason and round, outputs include:
#   - structured-option availability;
#   - structured selection counts;
#   - all-respondent and hesitant-cohort denominators;
#   - reason-specific non-missing denominators; and
#   - free-text recovery counts by mapping category.
#
# Notes:
#   - Questionnaire response options changed across REACT rounds.
#   - This script is descriptive and does not rerun BERTopic, clustering, or
#     logistic regression.
# ==============================================================================

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "readr", "stringr")
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
})

# ------------------------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------------------------
project_root <- "D:/projects/hda_2026/han"

raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"

questions_csv <- file.path(
  project_root,
  "questions.csv"
)

mapping_by_round_csv <- file.path(
  project_root,
  "outputs",
  "10c_post_meeting_descriptive_qc_v3",
  "round_audit",
  "existing_reason_mapping_source_by_round.csv"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "10e_structured_reason_prevalence_by_round"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. HELPERS
# ------------------------------------------------------------------------------
first_existing <- function(candidates, nms) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

as_binary <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(x_chr))
  
  out[x_chr %in% c("1","true","t","yes","y")] <- 1L
  out[x_chr %in% c("0","false","f","no","n")] <- 0L
  
  suppressWarnings({
    x_num <- as.numeric(x_chr)
    out[is.na(out) & !is.na(x_num) & x_num == 1] <- 1L
    out[is.na(out) & !is.na(x_num) & x_num == 0] <- 0L
  })
  
  out
}

normalise_survey_key <- function(x) {
  
  z <- toupper(trimws(as.character(x)))
  z <- gsub("[ -]", "_", z)
  z <- gsub("__+", "_", z)
  
  out <- rep(NA_character_, length(z))
  
  # Already official
  idx <- grepl("^REACT1_R[0-9]{2}$", z)
  out[idx] <- z[idx]
  
  idx <- grepl("^REACT2_S5_R[0-9]{2}$", z)
  out[idx] <- z[idx]
  
  # REACT1 round labels
  idx <- is.na(out) &
    grepl("REACT.?1", z) &
    grepl("([R_]|ROUND_?)(0?[0-9]{1,2})", z)
  
  if (any(idx)) {
    nums <- stringr::str_match(
      z[idx],
      "(?:R|ROUND_?)(0?[0-9]{1,2})"
    )[,2]
    
    out[idx] <- sprintf(
      "REACT1_R%02d",
      as.integer(nums)
    )
  }
  
  # REACT2 round labels
  idx <- is.na(out) &
    grepl("REACT.?2", z) &
    grepl("([R_]|ROUND_?)(0?[0-9]{1,2})", z)
  
  if (any(idx)) {
    nums <- stringr::str_match(
      z[idx],
      "(?:R|ROUND_?)(0?[0-9]{1,2})"
    )[,2]
    
    out[idx] <- sprintf(
      "REACT2_S5_R%02d",
      as.integer(nums)
    )
  }
  
  out
}

safe_pct <- function(n, d) {
  ifelse(d > 0, 100 * n / d, NA_real_)
}

# ------------------------------------------------------------------------------
# 3. LABELS
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# 4. LOAD INPUTS
# ------------------------------------------------------------------------------
if (!file.exists(raw_rds)) {
  stop("Missing raw RDS: ", raw_rds)
}

if (!file.exists(questions_csv)) {
  stop("Missing questions.csv: ", questions_csv)
}

df <- readRDS(raw_rds)
questions <- read_csv(
  questions_csv,
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# 5. IDENTIFY ROUND + DATE
# ------------------------------------------------------------------------------
survey_candidates <- c(
  "survey",
  "survey_name",
  "survey_round",
  "study_round",
  "round"
)

survey_col <- first_existing(
  survey_candidates,
  names(df)
)

if (is.na(survey_col)) {
  stop(
    "Could not identify survey/round variable. Checked: ",
    paste(survey_candidates, collapse = ", ")
  )
}

date_candidates <- c(
  "date",
  "survey_date",
  "completion_date",
  "start_date"
)

date_col <- first_existing(
  date_candidates,
  names(df)
)

df$.__survey_raw__ <- as.character(
  df[[survey_col]]
)

df$.__survey_key__ <- normalise_survey_key(
  df$.__survey_raw__
)

# If raw value already exactly matches official dictionary, preserve it.
official_surveys <- unique(
  as.character(questions$survey)
)

exact_match <- df$.__survey_raw__ %in% official_surveys
df$.__survey_key__[exact_match] <- df$.__survey_raw__[exact_match]

if (!is.na(date_col)) {
  df$.__date__ <- as.Date(
    df[[date_col]]
  )
} else {
  df$.__date__ <- as.Date(NA)
  warning(
    "No date variable found. Output will still contain survey labels, ",
    "but round_midpoint will be missing."
  )
}

# ------------------------------------------------------------------------------
# 6. DEFINE HESITANT COHORT
# ------------------------------------------------------------------------------
hes_var <- "analysis_4_vax_any_hes_or_refused_vs_vaxxed"

if (!(hes_var %in% names(df))) {
  stop(
    "Missing hesitancy definition variable: ",
    hes_var
  )
}

df$.__hesitant__ <- as_binary(
  df[[hes_var]]
) == 1

df$.__hesitant__[
  is.na(df$.__hesitant__)
] <- FALSE

# ------------------------------------------------------------------------------
# 7. OFFICIAL OPTION AVAILABILITY FROM questions.csv
# ------------------------------------------------------------------------------
if (!all(c("survey", "variable") %in% names(questions))) {
  stop(
    "questions.csv must contain columns: survey, variable"
  )
}

availability <- questions %>%
  mutate(
    survey = as.character(survey),
    variable = toupper(
      as.character(variable)
    )
  ) %>%
  filter(
    str_detect(
      variable,
      "^VACCRUFUSE[12]_[0-9]+$"
    )
  ) %>%
  mutate(
    source_question = str_match(
      variable,
      "^VACCRUFUSE([12])_"
    )[,2],
    reason_num = as.integer(
      str_match(
        variable,
        "_([0-9]+)$"
      )[,2]
    )
  ) %>%
  filter(
    reason_num >= 1,
    reason_num <= 23
  ) %>%
  distinct(
    survey,
    source_question,
    reason_num
  ) %>%
  group_by(
    survey,
    reason_num
  ) %>%
  summarise(
    option_available = TRUE,
    .groups = "drop"
  )

survey_reason_grid <- tidyr::expand_grid(
  survey = sort(
    unique(availability$survey)
  ),
  reason_num = 1:23
) %>%
  left_join(
    availability,
    by = c(
      "survey",
      "reason_num"
    )
  ) %>%
  mutate(
    option_available = replace_na(
      option_available,
      FALSE
    ),
    reason_label = unname(
      reason_labels[
        as.character(reason_num)
      ]
    )
  )

# ------------------------------------------------------------------------------
# 8. ROUND MIDPOINT + ROUND DENOMINATORS
# ------------------------------------------------------------------------------
round_summary <- df %>%
  group_by(
    survey = .__survey_key__
  ) %>%
  summarise(
    round_midpoint = if (
      all(is.na(.__date__))
    ) {
      as.Date(NA)
    } else {
      as.Date(
        stats::median(
          .__date__,
          na.rm = TRUE
        ),
        origin = "1970-01-01"
      )
    },
    n_all_respondents = n(),
    n_hesitant = sum(
      .__hesitant__,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  round_summary,
  file.path(
    output_dir,
    "round_denominators_and_midpoints.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. STRUCTURED REASON COUNTS BY ROUND
# ------------------------------------------------------------------------------
structured_rows <- list()
kk <- 1

for (j in 1:23) {
  
  var_j <- paste0(
    "vaccrufuse3_",
    j
  )
  
  if (!(var_j %in% names(df))) {
    warning(
      "Missing structured variable: ",
      var_j
    )
    next
  }
  
  z <- as_binary(
    df[[var_j]]
  )
  
  tmp <- data.frame(
    survey = df$.__survey_key__,
    hesitant = df$.__hesitant__,
    structured_value = z,
    stringsAsFactors = FALSE
  )
  
  out_j <- tmp %>%
    group_by(
      survey
    ) %>%
    summarise(
      reason_num = j,
      reason_label = unname(
        reason_labels[
          as.character(j)
        ]
      ),
      
      # Full-round participant denominator
      n_all_respondents = n(),
      
      # Definition-4 hesitant denominator
      n_hesitant = sum(
        hesitant,
        na.rm = TRUE
      ),
      
      # Reason-specific observed denominator
      structured_nonmissing_n = sum(
        !is.na(structured_value)
      ),
      
      # Reason-specific observed denominator among hesitant participants
      structured_nonmissing_hesitant_n = sum(
        hesitant &
          !is.na(structured_value),
        na.rm = TRUE
      ),
      
      # Structured positive selections
      structured_selected_n = sum(
        structured_value == 1,
        na.rm = TRUE
      ),
      
      structured_selected_hesitant_n = sum(
        hesitant &
          structured_value == 1,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Whole-round denominator for temporal trend:
      structured_selected_percent_all_respondents =
        safe_pct(
          structured_selected_n,
          n_all_respondents
        ),
      
      # Among hesitant participants in that round:
      structured_selected_percent_hesitant =
        safe_pct(
          structured_selected_hesitant_n,
          n_hesitant
        ),
      
      # Among participants with an observed response for that reason:
      structured_selected_percent_nonmissing =
        safe_pct(
          structured_selected_n,
          structured_nonmissing_n
        ),
      
      # Among hesitant participants with observed response for that reason:
      structured_selected_percent_nonmissing_hesitant =
        safe_pct(
          structured_selected_hesitant_n,
          structured_nonmissing_hesitant_n
        )
    )
  
  structured_rows[[kk]] <- out_j
  kk <- kk + 1
}

structured_by_round <- do.call(
  rbind,
  structured_rows
)

# ------------------------------------------------------------------------------
# 10. MERGE OFFICIAL AVAILABILITY + MIDPOINT
# ------------------------------------------------------------------------------
structured_by_round <- structured_by_round %>%
  left_join(
    survey_reason_grid %>%
      select(
        survey,
        reason_num,
        option_available
      ),
    by = c(
      "survey",
      "reason_num"
    )
  ) %>%
  left_join(
    round_summary %>%
      select(
        survey,
        round_midpoint
      ),
    by = "survey"
  ) %>%
  mutate(
    option_available = replace_na(
      option_available,
      FALSE
    ),

    matt_rule_available = case_when(

      survey %in% c('REACT2_S5_R05','REACT1_R09') &
        reason_num %in% c(21,22,23) ~ FALSE,

      !is.na(round_midpoint) &
        round_midpoint > as.Date('2021-09-17') &
        reason_num %in% c(5,18) ~ FALSE,

      TRUE ~ option_available
    ),

    observed_available = 
      structured_nonmissing_n >0,

    effective_available =
      matt_rule_available & observed_available 
  ) %>%
  arrange(
    round_midpoint,
    survey,
    reason_num
  )

write_csv(
  structured_by_round,
  file.path(
    output_dir,
    "MAIN_structured_reason_prevalence_by_round.csv"
  )
)

# ------------------------------------------------------------------------------
# 11. MERGE FREE-TEXT MAPPING SOURCE FROM 10c
# ------------------------------------------------------------------------------
if (file.exists(mapping_by_round_csv)) {
  
  mapping <- read_csv(
    mapping_by_round_csv,
    show_col_types = FALSE
  )
  
  needed_mapping_cols <- c(
    "survey",
    "reason_num",
    "reason_label",
    "mapping_category",
    "n"
  )
  
  missing_mapping_cols <- setdiff(
    needed_mapping_cols,
    names(mapping)
  )
  
  if (length(missing_mapping_cols) > 0) {
    stop(
      "10c mapping file missing columns: ",
      paste(
        missing_mapping_cols,
        collapse = ", "
      )
    )
  }
  
  mapping_wide <- mapping %>%
    select(
      survey,
      reason_num,
      mapping_category,
      n
    ) %>%
    group_by(
      survey,
      reason_num,
      mapping_category
    ) %>%
    summarise(
      n = sum(n),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = mapping_category,
      values_from = n,
      values_fill = 0
    )
  
  # Standardise names regardless of exact punctuation from 10c.
  old_names <- names(mapping_wide)
  
  clean_names <- old_names
  clean_names <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    clean_names
  )
  clean_names <- gsub(
    "_+$",
    "",
    clean_names
  )
  clean_names <- tolower(
    clean_names
  )
  
  names(mapping_wide) <- clean_names
  
  # Find the category columns by patterns.
  already_col <- grep(
    "^already_ticked",
    names(mapping_wide),
    value = TRUE
  )
  
  available_not_ticked_col <- grep(
    "^available_but_not_ticked",
    names(mapping_wide),
    value = TRUE
  )
  
  unavailable_col <- grep(
    "^option_unavailable",
    names(mapping_wide),
    value = TRUE
  )
  
  unresolved_col <- grep(
    "^unresolved",
    names(mapping_wide),
    value = TRUE
  )
  
  mapping_clean <- mapping_wide %>%
    transmute(
      survey,
      reason_num,
      
      free_text_already_ticked_n =
        if (length(already_col) > 0) {
          .data[[already_col[1]]]
        } else {
          0
        },
      
      free_text_available_not_ticked_n =
        if (length(available_not_ticked_col) > 0) {
          .data[[available_not_ticked_col[1]]]
        } else {
          0
        },
      
      free_text_option_unavailable_backfill_n =
        if (length(unavailable_col) > 0) {
          .data[[unavailable_col[1]]]
        } else {
          0
        },
      
      free_text_unresolved_n =
        if (length(unresolved_col) > 0) {
          .data[[unresolved_col[1]]]
        } else {
          0
        }
    ) %>%
    mutate(
      free_text_mentions_n =
        free_text_already_ticked_n +
        free_text_available_not_ticked_n +
        free_text_option_unavailable_backfill_n +
        free_text_unresolved_n
    )
  
  combined <- structured_by_round %>%
    left_join(
      mapping_clean,
      by = c(
        "survey",
        "reason_num"
      )
    ) %>%
    mutate(
      across(
        starts_with("free_text_"),
        ~replace_na(.x, 0)
      ),
      
      # Free-text mention rate using all respondents in the round.
      free_text_mentions_percent_all_respondents =
        safe_pct(
          free_text_mentions_n,
          n_all_respondents
        ),
      
      # Newly recovered information excluding elaboration.
      free_text_new_information_n =
        free_text_available_not_ticked_n +
        free_text_option_unavailable_backfill_n +
        free_text_unresolved_n,
      
      free_text_new_information_percent_all_respondents =
        safe_pct(
          free_text_new_information_n,
          n_all_respondents
        )
    ) %>%
    arrange(
      round_midpoint,
      survey,
      reason_num
    )
  
  write_csv(
    combined,
    file.path(
      output_dir,
      "MAIN_structured_plus_freetext_by_round.csv"
    )
  )
  
  # Export the three reasons most affected by option availability.
  focus <- combined %>%
    filter(
      reason_num %in% c(21, 22, 23)
    ) %>%
    arrange(
      round_midpoint,
      reason_num
    )
  
  write_csv(
    focus,
    file.path(
      output_dir,
      "FOCUS_fertility_allergy_previous_reaction_by_round.csv"
    )
  )
  
} else {
  
  warning(
    "10c mapping-by-round file not found:\n",
    mapping_by_round_csv,
    "\nStructured prevalence outputs were still created."
  )
}

# ------------------------------------------------------------------------------
# 12. AVAILABILITY QC
# ------------------------------------------------------------------------------
availability_qc <- structured_by_round %>%
  group_by(
    reason_num,
    reason_label
  ) %>%
  summarise(
    surveys_available = paste(
      survey[effective_available],
      collapse = "; "
    ),
    surveys_unavailable = paste(
      survey[!effective_available],
      collapse = "; "
    ),
    .groups = "drop"
  )

write_csv(
  availability_qc,
  file.path(
    output_dir,
    "reason_availability_qc.csv"
  )
)

# ------------------------------------------------------------------------------
# 13. COMPLETION
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("10e STRUCTURED REASON PREVALENCE BY ROUND — COMPLETED\n")
cat("============================================================\n")
cat("Key outputs:\n")
cat("1) MAIN_structured_reason_prevalence_by_round.csv\n")
cat("2) MAIN_structured_plus_freetext_by_round.csv\n")
cat("3) FOCUS_fertility_allergy_previous_reaction_by_round.csv\n")
cat("4) round_denominators_and_midpoints.csv\n")
cat("5) reason_availability_qc.csv\n\n")

cat("For the final time plots, sort by round_midpoint, NOT survey label.\n")
cat("============================================================\n")
