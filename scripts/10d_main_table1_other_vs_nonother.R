# ==============================================================================
# 10d_main_table1_other_vs_nonother.R
# ==============================================================================
#
# PURPOSE
#   Create the final main Table 1:
#
#     Characteristics of vaccine-hesitant participants
#     by selection of "Other"
#
#   Columns:
#     Overall hesitant cohort
#     Selected Other
#     Did not select Other
#
#   This is descriptive only. It does not rerun NLP, clustering, or regression.
#
# EXPECTED COUNTS
#   Hesitant cohort: 37,982
#   Selected Other:  4,237
#   Did not select Other: 33,745
#
# OUTPUT
#   D:/projects/hda_2026/han/outputs/10d_main_table1_other_vs_nonother/
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

input_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"

output_dir <- file.path(
  project_root,
  "outputs",
  "10d_main_table1_other_vs_nonother"
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
  
  out[x_chr %in% c("1", "true", "t", "yes", "y")] <- 1L
  out[x_chr %in% c("0", "false", "f", "no", "n")] <- 0L
  
  suppressWarnings({
    x_num <- as.numeric(x_chr)
    out[is.na(out) & !is.na(x_num) & x_num == 1] <- 1L
    out[is.na(out) & !is.na(x_num) & x_num == 0] <- 0L
  })
  
  out
}

fmt_n_pct <- function(n, d) {
  if (is.na(d) || d == 0) return(NA_character_)
  sprintf(
    "%s (%.1f%%)",
    format(n, big.mark = ",", scientific = FALSE),
    100 * n / d
  )
}

clean_chr <- function(x) {
  y <- trimws(as.character(x))
  y[y %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  y
}

# ------------------------------------------------------------------------------
# 3. LOAD DATA
# ------------------------------------------------------------------------------
if (!file.exists(input_rds)) {
  stop("Input RDS not found: ", input_rds)
}

df <- readRDS(input_rds)

if (!is.data.frame(df)) {
  stop("Input RDS is not a data.frame.")
}

# ------------------------------------------------------------------------------
# 4. DEFINE HESITANT COHORT
# ------------------------------------------------------------------------------
hes_flag_var <- "analysis_4_vax_any_hes_or_refused_vs_vaxxed"

if (!(hes_flag_var %in% names(df))) {
  stop("Missing hesitancy definition variable: ", hes_flag_var)
}

df$.__hesitant__ <- as_binary(df[[hes_flag_var]]) == 1
df$.__hesitant__[is.na(df$.__hesitant__)] <- FALSE

df_hes <- df %>%
  filter(.__hesitant__)

# ------------------------------------------------------------------------------
# 5. DEFINE SELECTED OTHER
# ------------------------------------------------------------------------------
if ("vaccrufuse3_19" %in% names(df_hes)) {
  
  df_hes$.__selected_other__ <- as_binary(df_hes$vaccrufuse3_19) == 1
  
} else if (all(c("vaccrufuse1_19", "vaccrufuse2_19") %in% names(df_hes))) {
  
  a <- as_binary(df_hes$vaccrufuse1_19) == 1
  b <- as_binary(df_hes$vaccrufuse2_19) == 1
  a[is.na(a)] <- FALSE
  b[is.na(b)] <- FALSE
  
  df_hes$.__selected_other__ <- a | b
  
} else {
  
  stop("Could not define Selected Other.")
}

df_hes$.__selected_other__[is.na(df_hes$.__selected_other__)] <- FALSE

# ------------------------------------------------------------------------------
# 6. COHORT QC
# ------------------------------------------------------------------------------
cohort_qc <- data.frame(
  measure = c(
    "Hesitant cohort",
    "Selected Other",
    "Did not select Other"
  ),
  n = c(
    nrow(df_hes),
    sum(df_hes$.__selected_other__),
    sum(!df_hes$.__selected_other__)
  )
)

write_csv(
  cohort_qc,
  file.path(output_dir, "Table1_cohort_qc.csv")
)

print(cohort_qc)

if (nrow(df_hes) != 37982) {
  warning(
    "Hesitant cohort is ",
    format(nrow(df_hes), big.mark = ","),
    ", not 37,982."
  )
}

if (sum(df_hes$.__selected_other__) != 4237) {
  warning(
    "Selected Other is ",
    format(sum(df_hes$.__selected_other__), big.mark = ","),
    ", not 4,237."
  )
}

# ------------------------------------------------------------------------------
# 7. VARIABLE DETECTION
# ------------------------------------------------------------------------------
# Variable names used in the analysis dataset.
age_var <- "age_group_named"
sex_var <- "sex"
ethnicity_var <- "ethnic_new"
imd_var <- "imd_quintile_cat"
education_var <- "edu_cat"
employment_var <- "empl_cat_new"
region_var <- "region_named"
round_var <- "study_round"

variable_manifest <- data.frame(
  characteristic = c(
    "Age group",
    "Sex",
    "Ethnicity",
    "Index of Multiple Deprivation quintile",
    "Education",
    "Employment status",
    "Region",
    "Survey round"
  ),
  variable = c(
    age_var,
    sex_var,
    ethnicity_var,
    imd_var,
    education_var,
    employment_var,
    region_var,
    round_var
  )
)

write_csv(
  variable_manifest,
  file.path(output_dir, "Table1_variable_manifest.csv")
)

print(variable_manifest)

# ------------------------------------------------------------------------------
# 8. DESCRIPTIVE TABLE BUILDER
# ------------------------------------------------------------------------------
make_section <- function(dat, var, section_label, custom_order = NULL) {
  
  if (is.na(var) || !(var %in% names(dat))) {
    return(
      data.frame(
        Characteristic = section_label,
        Level = "Variable unavailable",
        Overall = NA_character_,
        Selected_Other = NA_character_,
        Did_not_select_Other = NA_character_,
        Overall_n = NA_integer_,
        Selected_Other_n = NA_integer_,
        Did_not_select_Other_n = NA_integer_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  x <- clean_chr(dat[[var]])
  
  # Keep explicit missing as "Missing" so denominator remains the full cohort.
  x_display <- ifelse(is.na(x), "Missing", x)
  
  if (!is.null(custom_order)) {
    extra <- setdiff(unique(x_display), custom_order)
    levs <- c(custom_order, sort(extra))
    x_display <- factor(x_display, levels = levs)
  }
  
  tmp <- data.frame(
    level = as.character(x_display),
    selected_other = dat$.__selected_other__,
    stringsAsFactors = FALSE
  )
  
  all_levels <- unique(tmp$level)
  
  # Respect factor order if supplied.
  if (!is.null(custom_order)) {
    all_levels <- levels(x_display)
    all_levels <- all_levels[all_levels %in% unique(tmp$level)]
  } else {
    all_levels <- sort(all_levels)
  }
  
  n_overall <- nrow(dat)
  n_other <- sum(dat$.__selected_other__)
  n_nonother <- sum(!dat$.__selected_other__)
  
  rows <- lapply(
    all_levels,
    function(lvl) {
      
      n1 <- sum(tmp$level == lvl, na.rm = TRUE)
      n2 <- sum(tmp$level == lvl & tmp$selected_other, na.rm = TRUE)
      n3 <- sum(tmp$level == lvl & !tmp$selected_other, na.rm = TRUE)
      
      data.frame(
        Characteristic = section_label,
        Level = lvl,
        Overall = fmt_n_pct(n1, n_overall),
        Selected_Other = fmt_n_pct(n2, n_other),
        Did_not_select_Other = fmt_n_pct(n3, n_nonother),
        Overall_n = n1,
        Selected_Other_n = n2,
        Did_not_select_Other_n = n3,
        stringsAsFactors = FALSE
      )
    }
  )
  
  do.call(rbind, rows)
}

# ------------------------------------------------------------------------------
# 9. ORDERING
# ------------------------------------------------------------------------------
age_order <- c(
  "18-24",
  "25-34",
  "35-44",
  "45-54",
  "55-64",
  "65-74",
  "74+",
  "75+",
  "Missing"
)

sex_order <- c(
  "Female",
  "Male",
  "Other",
  "Not specified",
  "Missing"
)

imd_order <- c(
  "1 - most deprived",
  "1",
  "2",
  "3",
  "4",
  "5",
  "5 - least deprived",
  "Missing"
)

# ------------------------------------------------------------------------------
# 10. BUILD FINAL TABLE
# ------------------------------------------------------------------------------
sections <- list(
  make_section(df_hes, age_var, "Age group", age_order),
  make_section(df_hes, sex_var, "Sex", sex_order),
  make_section(df_hes, ethnicity_var, "Ethnicity"),
  make_section(
    df_hes,
    imd_var,
    "Index of Multiple Deprivation quintile",
    imd_order
  ),
  make_section(df_hes, education_var, "Education"),
  make_section(df_hes, employment_var, "Employment status"),
  make_section(df_hes, region_var, "Region"),
  make_section(df_hes, round_var, "Survey round")
)

table1 <- do.call(rbind, sections)

# Add N row at top.
n_row <- data.frame(
  Characteristic = "N",
  Level = "",
  Overall = format(nrow(df_hes), big.mark = ","),
  Selected_Other = format(sum(df_hes$.__selected_other__), big.mark = ","),
  Did_not_select_Other = format(sum(!df_hes$.__selected_other__), big.mark = ","),
  Overall_n = nrow(df_hes),
  Selected_Other_n = sum(df_hes$.__selected_other__),
  Did_not_select_Other_n = sum(!df_hes$.__selected_other__),
  stringsAsFactors = FALSE
)

table1 <- rbind(n_row, table1)

# ------------------------------------------------------------------------------
# 11. EXPORT
# ------------------------------------------------------------------------------
write_csv(
  table1,
  file.path(output_dir, "MAIN_Table1_other_vs_nonother.csv")
)

# Clean display version only.
display_table <- table1 %>%
  select(
    Characteristic,
    Level,
    Overall,
    Selected_Other,
    Did_not_select_Other
  )

write_csv(
  display_table,
  file.path(output_dir, "MAIN_Table1_other_vs_nonother_DISPLAY.csv")
)

# Long raw counts for local reformatting / plotting if needed.
long_counts <- table1 %>%
  select(
    Characteristic,
    Level,
    Overall_n,
    Selected_Other_n,
    Did_not_select_Other_n
  ) %>%
  pivot_longer(
    cols = c(
      Overall_n,
      Selected_Other_n,
      Did_not_select_Other_n
    ),
    names_to = "group",
    values_to = "n"
  )

write_csv(
  long_counts,
  file.path(output_dir, "MAIN_Table1_other_vs_nonother_LONG_counts.csv")
)

# ------------------------------------------------------------------------------
# 12. COMPLETION
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("MAIN TABLE 1 — OTHER VS NON-OTHER COMPLETED\n")
cat("============================================================\n")
cat(
  "Hesitant cohort: ",
  format(nrow(df_hes), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Selected Other: ",
  format(sum(df_hes$.__selected_other__), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Did not select Other: ",
  format(sum(!df_hes$.__selected_other__), big.mark = ","),
  "\n\n",
  sep = ""
)

cat("KEY OUTPUT FILES:\n")
cat("1) MAIN_Table1_other_vs_nonother_DISPLAY.csv\n")
cat("2) MAIN_Table1_other_vs_nonother.csv\n")
cat("3) MAIN_Table1_other_vs_nonother_LONG_counts.csv\n")
cat("4) Table1_variable_manifest.csv\n")
cat("5) Table1_cohort_qc.csv\n")
cat("============================================================\n")
