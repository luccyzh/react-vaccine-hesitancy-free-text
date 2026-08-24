# ============================================================
# 01b_generate_table1.R
# Purpose:
#   Generate a reproducible descriptive Table 1 for participants
#   with valid VACCRUFUSE1 / VACCRUFUSE2 "Other" free text.
#
# Project structure:
#   D:/projects/hda_2026/han/
#     outputs/01_data_preparation/
#     data/interim/
#
# Main Table 1 population:
#   Unique participants with at least one valid free-text response.
#   Expected cohort size: approximately 4,102 participants.
#
# Notes:
#   - One row per u_passcode.
#   - No p-values are produced because this is a descriptive table
#     for one target cohort.
#   - Missing / unknown / PNA / prefer not to say are shown as
#     "Not specified".
#   - Ethnicity is collapsed defensively into broad categories.
# ============================================================

# ----------------------------
# 0. Paths
# ----------------------------

project_root <- "D:/projects/hda_2026/han"

raw_rds <- file.path(
  "D:/saved_objects",
  "react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"
)

stage_dir <- file.path(project_root, "outputs", "01_data_preparation")
interim_dir <- file.path(project_root, "data", "interim")
table1_dir <- file.path(stage_dir, "table1")

dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(interim_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table1_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1. Load data
# ----------------------------

if (!file.exists(raw_rds)) {
  stop("Raw RDS not found: ", raw_rds)
}

df <- readRDS(raw_rds)

required_vars <- c(
  "u_passcode",
  "vaccrufuse1_19",
  "vaccrufuse2_19",
  "vaccrufuse1_19_other",
  "vaccrufuse2_19_other"
)

missing_required <- setdiff(required_vars, names(df))

if (length(missing_required) > 0) {
  stop(
    "Missing required variables: ",
    paste(missing_required, collapse = ", ")
  )
}

# ----------------------------
# 2. Recreate valid free-text cohort
# ----------------------------

invalid_codes <- c("-66", "-77", "-91", "-92")

text_1 <- trimws(as.character(df$vaccrufuse1_19_other))
text_2 <- trimws(as.character(df$vaccrufuse2_19_other))

valid_text_1 <- (
  df$vaccrufuse1_19 == 1 &
    !is.na(text_1) &
    text_1 != "" &
    !(text_1 %in% invalid_codes)
)

valid_text_2 <- (
  df$vaccrufuse2_19 == 1 &
    !is.na(text_2) &
    text_2 != "" &
    !(text_2 %in% invalid_codes)
)

valid_text_participant <- valid_text_1 | valid_text_2

table1_source <- df[valid_text_participant, , drop = FALSE]

if (nrow(table1_source) == 0) {
  stop("No valid free-text participants were identified.")
}

# Keep one row per participant.
# If repeated rows exist, use the first occurrence and record the audit.
duplicate_rows <- duplicated(as.character(table1_source$u_passcode))
table1_df <- table1_source[!duplicate_rows, , drop = FALSE]

# ----------------------------
# 3. Sample audit
# ----------------------------

sample_audit <- data.frame(
  measure = c(
    "Rows in loaded clean dfRes",
    "Rows with valid VACCRUFUSE1 free text",
    "Rows with valid VACCRUFUSE2 free text",
    "Rows with any valid free text",
    "Unique participants with any valid free text",
    "Duplicate participant rows removed for Table 1"
  ),
  n = c(
    nrow(df),
    sum(valid_text_1, na.rm = TRUE),
    sum(valid_text_2, na.rm = TRUE),
    sum(valid_text_participant, na.rm = TRUE),
    length(unique(as.character(df$u_passcode[valid_text_participant]))),
    sum(duplicate_rows)
  ),
  note = c(
    "Loaded source dataset",
    "VACCRUFUSE1 Other selected and valid text present",
    "VACCRUFUSE2 Other selected and valid text present",
    "Participant row has valid text in question 1 and/or question 2",
    "Primary Table 1 denominator; expected approximately 4,102",
    "Table 1 uses one row per u_passcode"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  sample_audit,
  file.path(table1_dir, "table1_population_audit.csv"),
  row.names = FALSE,
  na = ""
)

# Save participant IDs only for reproducibility.
write.csv(
  data.frame(participant_id = as.character(table1_df$u_passcode)),
  file.path(interim_dir, "table1_valid_freetext_participant_ids.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 4. Candidate Table 1 variables
# ----------------------------

table1_vars_requested <- c(
  "age_group_named",
  "sex",
  "bmi_cat",
  "ethnic_new",
  "imd_quintile_cat",
  "edu_cat",
  "empl_cat_new",
  "region_named",
  "face_covering",
  "mask_indoors",
  "mask_outdoors",
  "shielding",
  "smokenow",
  "res_antibody",
  "res_antigen",
  "healtha_15",
  "healtha_16",
  "healtha_17"
)

table1_vars_available <- intersect(table1_vars_requested, names(table1_df))
table1_vars_missing <- setdiff(table1_vars_requested, names(table1_df))

write.csv(
  data.frame(variable = table1_vars_missing),
  file.path(table1_dir, "table1_missing_requested_variables.csv"),
  row.names = FALSE,
  na = ""
)

if (length(table1_vars_available) == 0) {
  stop("None of the requested Table 1 variables are available.")
}

# ----------------------------
# 5. Cleaning helpers
# ----------------------------

normalise_missing <- function(x) {
  y <- trimws(as.character(x))
  low <- tolower(y)

  missing_labels <- c(
    "", "na", "n/a", "nan", "unknown", "pna",
    "prefer not to say", "prefer not to answer",
    "-66", "-77", "-91", "-92"
  )

  y[is.na(x) | low %in% missing_labels] <- "Not specified"
  y
}

collapse_ethnicity <- function(x) {
  y <- normalise_missing(x)
  z <- tolower(y)

  out <- rep("Other / mixed ethnicity", length(y))

  out[grepl("white", z)] <- "White"
  out[grepl("black|african|caribbean", z)] <- "Black"
  out[grepl("asian|indian|pakistani|bangladeshi|chinese", z)] <- "Asian"
  out[grepl("mixed|multiple", z)] <- "Mixed"
  out[grepl("not specified", z)] <- "Not specified"

  out
}

pretty_variable_name <- function(x) {
  labels <- c(
    age_group_named = "Age group",
    sex = "Sex",
    bmi_cat = "BMI category",
    ethnic_new = "Ethnicity",
    imd_quintile_cat = "Index of Multiple Deprivation quintile",
    edu_cat = "Education",
    empl_cat_new = "Employment status",
    region_named = "Region",
    face_covering = "Face covering",
    mask_indoors = "Mask use indoors",
    mask_outdoors = "Mask use outdoors",
    shielding = "Shielding status",
    smokenow = "Current smoking",
    res_antibody = "Antibody result",
    res_antigen = "Antigen result",
    healtha_15 = "Health condition 15",
    healtha_16 = "Health condition 16",
    healtha_17 = "Health condition 17"
  )

  if (x %in% names(labels)) unname(labels[[x]]) else x
}

# ----------------------------
# 6. Build descriptive Table 1
# ----------------------------

build_table1 <- function(data, variables) {
  output <- list()
  k <- 1

  for (v in variables) {
    x <- data[[v]]
    label <- pretty_variable_name(v)

    # Treat variables with many unique numeric values as continuous.
    numeric_continuous <- (
      is.numeric(x) &&
        length(unique(x[!is.na(x)])) > 10
    )

    if (numeric_continuous) {
      n_nonmissing <- sum(!is.na(x))

      output[[k]] <- data.frame(
        characteristic = label,
        level = "Mean (SD)",
        value = if (n_nonmissing > 0) {
          sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
        } else {
          "Not available"
        },
        n_nonmissing = n_nonmissing,
        denominator = nrow(data),
        stringsAsFactors = FALSE
      )
      k <- k + 1

      output[[k]] <- data.frame(
        characteristic = label,
        level = "Median (IQR)",
        value = if (n_nonmissing > 0) {
          q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
          sprintf("%.1f (%.1f–%.1f)", q[[2]], q[[1]], q[[3]])
        } else {
          "Not available"
        },
        n_nonmissing = n_nonmissing,
        denominator = nrow(data),
        stringsAsFactors = FALSE
      )
      k <- k + 1

    } else {
      if (v == "ethnic_new") {
        x_clean <- collapse_ethnicity(x)
      } else {
        x_clean <- normalise_missing(x)
      }

      counts <- table(x_clean, useNA = "no")
      levels_sorted <- names(sort(counts, decreasing = TRUE))

      for (lev in levels_sorted) {
        n <- unname(counts[[lev]])
        pct <- 100 * n / nrow(data)

        output[[k]] <- data.frame(
          characteristic = label,
          level = lev,
          value = sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct),
          n_nonmissing = if (lev == "Not specified") NA_integer_ else n,
          denominator = nrow(data),
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
  }

  do.call(rbind, output)
}

table1 <- build_table1(table1_df, table1_vars_available)

# Add cohort N as first row.
table1_header <- data.frame(
  characteristic = "Participants",
  level = "Total",
  value = format(nrow(table1_df), big.mark = ","),
  n_nonmissing = nrow(table1_df),
  denominator = nrow(table1_df),
  stringsAsFactors = FALSE
)

table1 <- rbind(table1_header, table1)

write.csv(
  table1,
  file.path(table1_dir, "table1_valid_freetext_overall.csv"),
  row.names = FALSE,
  na = ""
)

# A compact display version for direct use in Word / Excel.
table1_display <- table1[, c("characteristic", "level", "value")]

write.csv(
  table1_display,
  file.path(table1_dir, "table1_valid_freetext_display.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 7. Optional cohort-source audit
# ----------------------------

source_group <- ifelse(
  valid_text_1[valid_text_participant] & valid_text_2[valid_text_participant],
  "Both questions",
  ifelse(
    valid_text_1[valid_text_participant],
    "VACCRUFUSE1 only",
    "VACCRUFUSE2 only"
  )
)

source_audit_raw <- data.frame(
  participant_id = as.character(table1_source$u_passcode),
  source_group = source_group,
  stringsAsFactors = FALSE
)

# One participant may have repeated source rows; collapse transparently.
source_by_participant <- aggregate(
  source_group ~ participant_id,
  data = source_audit_raw,
  FUN = function(z) {
    z <- unique(z)
    if (length(z) == 1) z else "Both questions"
  }
)

source_counts <- as.data.frame(table(source_by_participant$source_group))
names(source_counts) <- c("source_group", "n")
source_counts$percentage <- 100 * source_counts$n / sum(source_counts$n)

write.csv(
  source_counts,
  file.path(table1_dir, "table1_question_source_audit.csv"),
  row.names = FALSE,
  na = ""
)

# ----------------------------
# 8. Session information
# ----------------------------

capture.output(
  sessionInfo(),
  file = file.path(table1_dir, "table1_R_session_info.txt")
)

message("Table 1 completed successfully.")
message("Target cohort N = ", nrow(table1_df))
message("Outputs saved to: ", table1_dir)
