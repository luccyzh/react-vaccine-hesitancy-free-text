# ============================================================
# 10_final_qc_and_table1_FINAL.R
# Purpose:
#   Non-destructive QC checks and a journal-style Table 1.
#
# This script DOES NOT change any analysis variables or rerun models.
# It only reads existing outputs and writes QC / descriptive outputs.
#
# Main questions answered:
#   1) Why were participants excluded by the primary-clustering all-zero rule?
#   2) How much did BERTopic outlier status (-1) contribute to that exclusion?
#   3) Were BERTopic outliers automatically lost downstream, or were some retained
#      through structured questionnaire reasons?
#   4) Does the primary clustering variable set / cohort size reproduce Step 07 QC?
#   5) Produce a journal-style Table 1 for the valid free-text cohort.
#
# Outputs:
#   outputs/10_final_qc_and_table1/
#       qc/
#       table1/
#
# Implementation notes:
#   - base R only (no new packages required)
#   - defensive column-name detection
#   - stops only when a genuinely essential input is missing
# ============================================================

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)

# ============================================================
# 0. PATHS
# ============================================================

project_root <- "D:/projects/hda_2026/han"
output_root  <- file.path(project_root, "outputs")

raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"

enriched_rds <- file.path(
  output_root,
  "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)

participant_topic_file <- file.path(
  output_root,
  "06_participant_mapping",
  "participant_topic_mapping.csv"
)

all_zero_file <- file.path(
  output_root,
  "06d_mapping_qc_and_comparison",
  "participants_all_zero_for_primary_clustering.csv"
)

primary_dictionary_file <- file.path(
  output_root,
  "07_augmented_clustering",
  "primary_without_other",
  "clustering_variable_dictionary.csv"
)

primary_qc_file <- file.path(
  output_root,
  "07_augmented_clustering",
  "primary_without_other",
  "qc_summary.csv"
)

out_dir    <- file.path(output_root, "10_final_qc_and_table1")
qc_dir     <- file.path(out_dir, "qc")
table1_dir <- file.path(out_dir, "table1")

dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table1_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. HELPERS
# ============================================================

first_existing <- function(candidates, nms) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

as_binary <- function(x) {
  # Defensive conversion of common 0/1 encodings.
  if (is.logical(x)) return(as.integer(x))
  y <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(y))
  out[y %in% c("1", "yes", "true", "y")] <- 1L
  out[y %in% c("0", "no", "false", "n")] <- 0L
  suppressWarnings({
    num <- as.numeric(y)
    out[is.na(out) & !is.na(num)] <- as.integer(num[is.na(out) & !is.na(num)] > 0)
  })
  out
}

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
  out <- rep("Other", length(y))
  out[grepl("white", z)] <- "White"
  out[grepl("black|african|caribbean", z)] <- "Black"
  out[grepl("asian|indian|pakistani|bangladeshi|chinese", z)] <- "Asian"
  out[grepl("mixed|multiple", z)] <- "Mixed"
  out[grepl("not specified", z)] <- "Not specified"
  out
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("\\$", "\\\\$", x)
  x
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# ============================================================
# 2. INPUT MANIFEST
# ============================================================

manifest <- data.frame(
  object = c(
    "raw_rds",
    "enriched_rds",
    "participant_topic_mapping",
    "all_zero_primary_clustering",
    "primary_clustering_dictionary",
    "primary_clustering_qc"
  ),
  path = c(
    raw_rds,
    enriched_rds,
    participant_topic_file,
    all_zero_file,
    primary_dictionary_file,
    primary_qc_file
  )
)
manifest$exists <- file.exists(manifest$path)

write.csv(
  manifest,
  file.path(qc_dir, "10_input_manifest.csv"),
  row.names = FALSE
)

cat("\n================ INPUT MANIFEST ================\n")
print(manifest[, c("object", "exists")], row.names = FALSE)
cat("================================================\n\n")

if (!file.exists(raw_rds)) stop("Essential input missing: raw_rds")
if (!file.exists(enriched_rds)) stop("Essential input missing: enriched_rds")

# ============================================================
# 3. LOAD CORE DATA
# ============================================================

raw <- readRDS(raw_rds)
enriched <- readRDS(enriched_rds)

# Prefer u_passcode, which is the project participant identifier.
id_candidates <- c(
  "u_passcode", "participant_id", "participantid",
  "person_id", "uid", "ID", "id"
)

id_raw <- first_existing(id_candidates, names(raw))
id_enriched <- first_existing(id_candidates, names(enriched))

if (is.na(id_raw)) stop("Could not identify participant ID in raw dataset.")
if (is.na(id_enriched)) stop("Could not identify participant ID in enriched dataset.")

cat("Raw ID column:      ", id_raw, "\n")
cat("Enriched ID column: ", id_enriched, "\n")

# Standardised helper ID for joins.
raw$.__pid__ <- as.character(raw[[id_raw]])
enriched$.__pid__ <- as.character(enriched[[id_enriched]])

# ============================================================
# 4. RECREATE VALID FREE-TEXT COHORT FOR TABLE 1
# ============================================================

required_ft <- c(
  "vaccrufuse1_19", "vaccrufuse2_19",
  "vaccrufuse1_19_other", "vaccrufuse2_19_other"
)
missing_ft <- setdiff(required_ft, names(raw))
if (length(missing_ft) > 0) {
  stop("Missing free-text cohort variables: ", paste(missing_ft, collapse = ", "))
}

invalid_codes <- c("-66", "-77", "-91", "-92")
text_1 <- trimws(as.character(raw$vaccrufuse1_19_other))
text_2 <- trimws(as.character(raw$vaccrufuse2_19_other))

valid_text_1 <- (
  as_binary(raw$vaccrufuse1_19) == 1 &
    !is.na(text_1) & text_1 != "" & !(text_1 %in% invalid_codes)
)
valid_text_2 <- (
  as_binary(raw$vaccrufuse2_19) == 1 &
    !is.na(text_2) & text_2 != "" & !(text_2 %in% invalid_codes)
)
valid_any <- valid_text_1 | valid_text_2

ft_source <- raw[valid_any, , drop = FALSE]
ft_source$.__pid__ <- as.character(ft_source[[id_raw]])
ft_df <- ft_source[!duplicated(ft_source$.__pid__), , drop = FALSE]

# Free-text question source at participant level.
source_raw <- data.frame(
  .__pid__ = raw$.__pid__[valid_any],
  source_group = ifelse(
    valid_text_1[valid_any] & valid_text_2[valid_any],
    "Both questions",
    ifelse(valid_text_1[valid_any], "VACCRUFUSE1 only", "VACCRUFUSE2 only")
  )
)
source_by_pid <- aggregate(
  source_group ~ .__pid__,
  data = source_raw,
  FUN = function(z) {
    z <- unique(z)
    if (length(z) == 1) z else "Both questions"
  }
)
ft_df <- merge(ft_df, source_by_pid, by = ".__pid__", all.x = TRUE, sort = FALSE)

cohort_audit <- data.frame(
  measure = c(
    "Rows in raw dataset",
    "Rows with valid VACCRUFUSE1 free text",
    "Rows with valid VACCRUFUSE2 free text",
    "Rows with any valid free text",
    "Unique participants with valid free text",
    "Duplicate rows removed for Table 1"
  ),
  n = c(
    nrow(raw),
    sum(valid_text_1, na.rm = TRUE),
    sum(valid_text_2, na.rm = TRUE),
    sum(valid_any, na.rm = TRUE),
    nrow(ft_df),
    nrow(ft_source) - nrow(ft_df)
  )
)
write.csv(cohort_audit, file.path(table1_dir, "table1_cohort_audit.csv"), row.names = FALSE)

cat("\nValid free-text Table 1 cohort N = ", nrow(ft_df), "\n", sep = "")

# ============================================================
# 5. JOURNAL-STYLE TABLE 1
# ============================================================
# Core descriptive characteristics only. This is intentionally concise.
# If a variable is unavailable, it is simply omitted and recorded.

core_vars <- c(
  age_group_named = "Age group",
  sex = "Sex",
  ethnic_new = "Ethnicity",
  imd_quintile_cat = "Index of Multiple Deprivation quintile",
  edu_cat = "Education",
  empl_cat_new = "Employment status",
  region_named = "Region",
  source_group = "Free-text question source"
)

available_vars <- names(core_vars)[names(core_vars) %in% names(ft_df)]
missing_vars <- setdiff(names(core_vars), names(ft_df))

write.csv(
  data.frame(variable = missing_vars),
  file.path(table1_dir, "table1_missing_core_variables.csv"),
  row.names = FALSE
)

# Long analytical Table 1.
t1_rows <- list()
k <- 1

for (v in available_vars) {
  label <- unname(core_vars[[v]])
  x <- ft_df[[v]]

  if (v == "ethnic_new") {
    xc <- collapse_ethnicity(x)
  } else {
    xc <- normalise_missing(x)
  }

  counts <- table(xc, useNA = "no")
  levs <- names(sort(counts, decreasing = TRUE))

  # Section header row.
  t1_rows[[k]] <- data.frame(
    characteristic = label,
    level = "",
    overall = "",
    row_type = "header"
  )
  k <- k + 1

  for (lev in levs) {
    n <- as.integer(counts[[lev]])
    pct <- 100 * n / nrow(ft_df)
    t1_rows[[k]] <- data.frame(
      characteristic = "",
      level = lev,
      overall = sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct),
      row_type = "level"
    )
    k <- k + 1
  }
}

table1_final <- do.call(rbind, t1_rows)

# CSV output for report preparation.
write.csv(
  table1_final[, c("characteristic", "level", "overall")],
  file.path(table1_dir, "Table1_FINAL.csv"),
  row.names = FALSE,
  na = ""
)

# Plain-text version for QC.
con <- file(file.path(table1_dir, "Table1_FINAL.txt"), open = "wt")
writeLines(
  c(
    "Table 1. Characteristics of participants with valid free-text responses",
    paste0("Overall (N = ", format(nrow(ft_df), big.mark = ","), ")"),
    ""
  ),
  con
)
for (i in seq_len(nrow(table1_final))) {
  if (table1_final$row_type[i] == "header") {
    writeLines(paste0("\n", table1_final$characteristic[i]), con)
  } else {
    writeLines(sprintf("  %-45s %s", table1_final$level[i], table1_final$overall[i]), con)
  }
}
close(con)

# HTML version: genuinely formatted Table 1, opens in a browser and can be copied.
html_file <- file.path(table1_dir, "Table1_FINAL.html")
con <- file(html_file, open = "wt")
writeLines(c(
  "<!DOCTYPE html>",
  "<html><head><meta charset='utf-8'>",
  "<style>",
  "body{font-family:Arial,Helvetica,sans-serif;margin:35px;color:#111;}",
  "table{border-collapse:collapse;width:760px;max-width:100%;font-size:14px;}",
  "caption{text-align:left;font-weight:bold;font-size:16px;margin-bottom:10px;}",
  "th{border-top:2px solid #222;border-bottom:1px solid #222;padding:7px 8px;text-align:left;}",
  "td{padding:5px 8px;vertical-align:top;}",
  "tr.section td{font-weight:bold;padding-top:10px;border-top:1px solid #bbb;}",
  "td.level{padding-left:24px;}",
  "tr:last-child td{border-bottom:2px solid #222;}",
  "</style></head><body>",
  paste0("<table><caption>Table 1. Characteristics of participants with valid free-text responses</caption>"),
  paste0("<thead><tr><th>Characteristic</th><th>Overall (N = ", format(nrow(ft_df), big.mark = ","), ")</th></tr></thead><tbody>")
), con)

for (i in seq_len(nrow(table1_final))) {
  if (table1_final$row_type[i] == "header") {
    writeLines(
      paste0("<tr class='section'><td colspan='2'>", html_escape(table1_final$characteristic[i]), "</td></tr>"),
      con
    )
  } else {
    writeLines(
      paste0(
        "<tr><td class='level'>", html_escape(table1_final$level[i]), "</td><td>",
        html_escape(table1_final$overall[i]), "</td></tr>"
      ),
      con
    )
  }
}
writeLines("</tbody></table></body></html>", con)
close(con)

# LaTeX fragment for Overleaf. Requires \usepackage{booktabs}.
tex_file <- file.path(table1_dir, "Table1_FINAL.tex")
con <- file(tex_file, open = "wt")
writeLines(c(
  "% Requires \\usepackage{booktabs}",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Characteristics of participants with valid free-text responses}",
  "\\label{tab:table1}",
  "\\begin{tabular}{p{0.62\\textwidth}r}",
  "\\toprule",
  paste0("Characteristic & Overall (N = ", format(nrow(ft_df), big.mark = ","), ") \\\\"),
  "\\midrule"
), con)

for (i in seq_len(nrow(table1_final))) {
  if (table1_final$row_type[i] == "header") {
    writeLines(
      paste0("\\textbf{", latex_escape(table1_final$characteristic[i]), "} & \\\\"),
      con
    )
  } else {
    writeLines(
      paste0("\\quad ", latex_escape(table1_final$level[i]), " & ", latex_escape(table1_final$overall[i]), " \\\\"),
      con
    )
  }
}
writeLines(c(
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
), con)
close(con)

# ============================================================
# 6. PRIMARY CLUSTERING VARIABLE SET
# ============================================================

primary_vars <- character(0)

if (file.exists(primary_dictionary_file)) {
  dict <- read.csv(primary_dictionary_file, check.names = FALSE)
  var_candidates <- c("variable", "Variable", "variable_name", "name", "raw_variable")
  dict_var_col <- first_existing(var_candidates, names(dict))
  if (!is.na(dict_var_col)) {
    primary_vars <- unique(as.character(dict[[dict_var_col]]))
    primary_vars <- primary_vars[!is.na(primary_vars) & primary_vars != ""]
  }
}

# Fallback only if dictionary could not be interpreted.
if (length(primary_vars) == 0) {
  primary_vars <- c(
    paste0("augmented_existing_", setdiff(1:23, 19)),
    "novel_brand_preference",
    "novel_access_availability_dosing",
    "novel_ethical_religious_animal",
    "novel_government_distrust_autonomy",
    "novel_mrna_gene_technology"
  )
}

primary_vars_present <- intersect(primary_vars, names(enriched))
primary_vars_missing <- setdiff(primary_vars, names(enriched))

write.csv(
  data.frame(variable = primary_vars_missing),
  file.path(qc_dir, "primary_clustering_variables_missing.csv"),
  row.names = FALSE
)

if (length(primary_vars_present) == 0) {
  warning("Could not identify primary clustering variables in enriched dataset; participant-level all-zero reconstruction skipped.")
}

# ============================================================
# 7. LOAD ALL-ZERO EXCLUSION LIST
# ============================================================

excluded <- NULL
excluded_ids <- character(0)

if (file.exists(all_zero_file)) {
  excluded <- read.csv(all_zero_file, check.names = FALSE)
  id_excluded <- first_existing(id_candidates, names(excluded))
  if (!is.na(id_excluded)) {
    excluded_ids <- unique(as.character(excluded[[id_excluded]]))
  } else {
    warning("Could not detect participant ID in all-zero exclusion file.")
  }
} else {
  warning("All-zero exclusion file not found; exclusion-composition QC will be partial.")
}

# ============================================================
# 8. BERTopic OUTLIER PARTICIPANTS
# ============================================================

outlier_ids <- character(0)
topic_mapping_available <- FALSE

if (file.exists(participant_topic_file)) {
  topic_map <- read.csv(participant_topic_file, check.names = FALSE)
  id_topic <- first_existing(id_candidates, names(topic_map))
  topic_candidates <- c("topic", "Topic", "bertopic_topic", "response_topic", "topic_id")
  topic_col <- first_existing(topic_candidates, names(topic_map))

  if (!is.na(id_topic) && !is.na(topic_col)) {
    tnum <- suppressWarnings(as.numeric(as.character(topic_map[[topic_col]])))
    outlier_ids <- unique(as.character(topic_map[[id_topic]][!is.na(tnum) & tnum == -1]))
    topic_mapping_available <- TRUE
    cat("BERTopic outlier participants detected: ", length(outlier_ids), "\n", sep = "")
  } else {
    warning("participant_topic_mapping.csv found, but ID/topic columns could not be detected.")
  }
} else {
  warning("participant_topic_mapping.csv not found; BERTopic outlier QC skipped.")
}

# ============================================================
# 9. RECONSTRUCT ALL-ZERO STATUS IN ENRICHED DATA
# ============================================================

if (length(primary_vars_present) > 0) {
  mat <- sapply(primary_vars_present, function(v) {
    z <- as_binary(enriched[[v]])
    z[is.na(z)] <- 0L
    z
  })
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1)
  primary_positive_n <- rowSums(mat, na.rm = TRUE)
  enriched$.__primary_all_zero__ <- primary_positive_n == 0
} else {
  enriched$.__primary_all_zero__ <- NA
}

# Special indicators outside the primary 27-variable matrix.
other_candidates <- c("augmented_existing_19", "vaccrufuse3_19")
other_col <- first_existing(other_candidates, names(enriched))

uncertainty_candidates <- grep(
  "uncert|decision.*pending|decision.*not.*made",
  names(enriched),
  ignore.case = TRUE,
  value = TRUE
)
uncertainty_col <- if (length(uncertainty_candidates) > 0) uncertainty_candidates[[1]] else NA_character_

if (!is.na(other_col)) {
  enriched$.__other__ <- as_binary(enriched[[other_col]]) == 1
  enriched$.__other__[is.na(enriched$.__other__)] <- FALSE
} else {
  enriched$.__other__ <- FALSE
}

if (!is.na(uncertainty_col)) {
  enriched$.__uncertainty__ <- as_binary(enriched[[uncertainty_col]]) == 1
  enriched$.__uncertainty__[is.na(enriched$.__uncertainty__)] <- FALSE
} else {
  enriched$.__uncertainty__ <- FALSE
}

enriched$.__bertopic_outlier__ <- enriched$.__pid__ %in% outlier_ids
enriched$.__listed_all_zero_excluded__ <- enriched$.__pid__ %in% excluded_ids

# ============================================================
# 10. EXCLUSION QC: OVERLAPPING COUNTS
# ============================================================

excluded_df <- enriched[enriched$.__listed_all_zero_excluded__, , drop = FALSE]

if (nrow(excluded_df) > 0) {
  exclusion_overlap <- data.frame(
    measure = c(
      "Total participants listed as all-zero excluded from primary clustering",
      "Reconstructed primary matrix all-zero",
      "BERTopic outlier (-1)",
      "BERTopic outlier AND primary matrix all-zero",
      "Other selected",
      "Uncertainty / decision pending",
      "Other selected AND primary matrix all-zero",
      "Uncertainty AND primary matrix all-zero",
      "Other + uncertainty AND primary matrix all-zero"
    ),
    n = c(
      nrow(excluded_df),
      sum(excluded_df$.__primary_all_zero__ %in% TRUE, na.rm = TRUE),
      sum(excluded_df$.__bertopic_outlier__, na.rm = TRUE),
      sum(excluded_df$.__bertopic_outlier__ & excluded_df$.__primary_all_zero__, na.rm = TRUE),
      sum(excluded_df$.__other__, na.rm = TRUE),
      sum(excluded_df$.__uncertainty__, na.rm = TRUE),
      sum(excluded_df$.__other__ & excluded_df$.__primary_all_zero__, na.rm = TRUE),
      sum(excluded_df$.__uncertainty__ & excluded_df$.__primary_all_zero__, na.rm = TRUE),
      sum(excluded_df$.__other__ & excluded_df$.__uncertainty__ & excluded_df$.__primary_all_zero__, na.rm = TRUE)
    )
  )
  exclusion_overlap$percent_of_excluded <- round(100 * exclusion_overlap$n / nrow(excluded_df), 1)

  write.csv(
    exclusion_overlap,
    file.path(qc_dir, "primary_clustering_exclusion_overlap_summary.csv"),
    row.names = FALSE
  )

  # Mutually exclusive explanatory categories among listed all-zero exclusions.
  # These categories are descriptive, not causal proof.
  exclusive_category <- rep("No Other/uncertainty flag in detected variables", nrow(excluded_df))
  exclusive_category[excluded_df$.__other__ & !excluded_df$.__uncertainty__] <- "Other only (outside primary matrix)"
  exclusive_category[!excluded_df$.__other__ & excluded_df$.__uncertainty__] <- "Uncertainty only (outside primary matrix)"
  exclusive_category[excluded_df$.__other__ & excluded_df$.__uncertainty__] <- "Other + uncertainty"

  exclusion_exclusive <- as.data.frame(table(exclusive_category), stringsAsFactors = FALSE)
  names(exclusion_exclusive) <- c("category", "n")
  exclusion_exclusive$percent <- round(100 * exclusion_exclusive$n / sum(exclusion_exclusive$n), 1)
  exclusion_exclusive <- exclusion_exclusive[order(-exclusion_exclusive$n), ]

  write.csv(
    exclusion_exclusive,
    file.path(qc_dir, "primary_clustering_exclusion_exclusive_categories.csv"),
    row.names = FALSE
  )
}

# ============================================================
# 11. OUTLIER -> DOWNSTREAM INCLUSION QC
# ============================================================

if (topic_mapping_available && length(outlier_ids) > 0) {
  outlier_status <- data.frame(
    .__pid__ = outlier_ids,
    excluded_by_primary_all_zero_rule = outlier_ids %in% excluded_ids
  )

  outlier_summary <- data.frame(
    measure = c(
      "Total BERTopic outlier participants (-1)",
      "Outliers excluded by primary all-zero rule",
      "Outliers not excluded by primary all-zero rule"
    ),
    n = c(
      length(outlier_ids),
      sum(outlier_status$excluded_by_primary_all_zero_rule),
      sum(!outlier_status$excluded_by_primary_all_zero_rule)
    )
  )
  outlier_summary$percent_of_outliers <- round(100 * outlier_summary$n / length(outlier_ids), 1)

  write.csv(
    outlier_summary,
    file.path(qc_dir, "bertopic_outlier_downstream_inclusion_summary.csv"),
    row.names = FALSE
  )

  # Within valid free-text cohort only: outlier vs non-outlier and exclusion status.
  ft_ids <- unique(as.character(ft_df$.__pid__))
  ft_qc <- data.frame(
    .__pid__ = ft_ids,
    bertopic_outlier = ft_ids %in% outlier_ids,
    excluded_primary_all_zero = ft_ids %in% excluded_ids
  )
  ft_cross <- as.data.frame(table(
    BERTopic_outlier = ft_qc$bertopic_outlier,
    Primary_all_zero_excluded = ft_qc$excluded_primary_all_zero
  ))
  write.csv(
    ft_cross,
    file.path(qc_dir, "valid_freetext_outlier_by_primary_exclusion_crosstab.csv"),
    row.names = FALSE
  )
}

# ============================================================
# 12. STEP 07 REPRODUCTION CHECK
# ============================================================

step07_check <- data.frame(
  measure = character(0),
  value = character(0)
)

step07_check <- rbind(
  step07_check,
  data.frame(measure = "Primary clustering variables detected", value = as.character(length(primary_vars_present))),
  data.frame(measure = "Participants listed as all-zero excluded", value = as.character(length(excluded_ids))),
  data.frame(measure = "Valid free-text cohort", value = as.character(nrow(ft_df))),
  data.frame(measure = "BERTopic outlier participants detected", value = as.character(length(outlier_ids)))
)

if (file.exists(primary_qc_file)) {
  qc07 <- read.csv(primary_qc_file, check.names = FALSE)
  write.csv(qc07, file.path(qc_dir, "step07_primary_qc_copy.csv"), row.names = FALSE)
}

write.csv(
  step07_check,
  file.path(qc_dir, "final_key_count_check.csv"),
  row.names = FALSE
)

# ============================================================
# 13. SESSION INFO + FINAL CONSOLE SUMMARY
# ============================================================

capture.output(sessionInfo(), file = file.path(out_dir, "10_R_session_info.txt"))

cat("\n============================================================\n")
cat("STEP 10 FINAL QC + TABLE 1 COMPLETED\n")
cat("============================================================\n")
cat("Valid free-text cohort N: ", nrow(ft_df), "\n", sep = "")
cat("Primary clustering variables detected: ", length(primary_vars_present), "\n", sep = "")
cat("All-zero excluded IDs detected: ", length(excluded_ids), "\n", sep = "")
cat("BERTopic outlier IDs detected: ", length(outlier_ids), "\n", sep = "")
cat("\nTable 1 outputs:\n")
cat("  ", file.path(table1_dir, "Table1_FINAL.csv"), "\n", sep = "")
cat("  ", file.path(table1_dir, "Table1_FINAL.html"), "\n", sep = "")
cat("  ", file.path(table1_dir, "Table1_FINAL.tex"), "\n", sep = "")
cat("\nQC outputs:\n")
cat("  ", qc_dir, "\n", sep = "")
cat("============================================================\n")
