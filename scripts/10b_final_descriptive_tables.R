# Format existing analysis outputs into journal-style descriptive tables and supplementary summaries.

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE, scipen = 999)
project_root <- "D:/projects/hda_2026/han"
output_root  <- file.path(project_root, "outputs")
raw_rds <- "D:/saved_objects/react_1_react_2_vax_hesitancy_data_with_NHS_first_Vax_data.rds"
old_t1_dir <- file.path(output_root, "01_data_preparation", "table1")
old_t1_display <- file.path(old_t1_dir, "table1_valid_freetext_display.csv")
old_t1_overall <- file.path(old_t1_dir, "table1_valid_freetext_overall.csv")
old_t1_audit   <- file.path(old_t1_dir, "table1_population_audit.csv")
novel_prev_file <- file.path(
  output_root, "06f_theme_level_descriptive",
  "novel_theme_prevalence_freetext_cohort.csv"
)
novel_outcome_file <- file.path(
  output_root, "06f_theme_level_descriptive",
  "subsequent_vaccination_by_theme.csv"
)
novel_char_file <- file.path(
  output_root, "06f_theme_level_descriptive",
  "participant_characteristics_overall_and_by_theme_long.csv"
)
novel_dict_file <- file.path(
  output_root, "06f_theme_level_descriptive",
  "novel_theme_dictionary.csv"
)
cluster_prev_file <- file.path(
  output_root, "07b_cluster_level_descriptive",
  "cluster_prevalence_primary_cohort.csv"
)
cluster_outcome_file <- file.path(
  output_root, "07b_cluster_level_descriptive",
  "subsequent_vaccination_by_cluster.csv"
)
cluster_char_file <- file.path(
  output_root, "07b_cluster_level_descriptive",
  "participant_characteristics_by_cluster_long.csv"
)
cluster_dict_file <- file.path(
  output_root, "07b_cluster_level_descriptive",
  "primary_cluster_dictionary_final.csv"
)
out_dir <- file.path(output_root, "10b_final_descriptive_tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
first_existing <- function(candidates, nms) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}
first_regex <- function(patterns, nms, exclude = character(0)) {
  pool <- setdiff(nms, exclude)
  for (p in patterns) {
    hit <- grep(p, pool, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[[1]])
  }
  NA_character_
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
as_binary <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  y <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(y))
  out[y %in% c("1", "yes", "true", "y")] <- 1L
  out[y %in% c("0", "no", "false", "n")] <- 0L
  suppressWarnings({
    num <- as.numeric(y)
    use <- is.na(out) & !is.na(num)
    out[use] <- as.integer(num[use] > 0)
  })
  out
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
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}
write_simple_table <- function(df, title, stem, note = NULL) {
  csv_file  <- file.path(out_dir, paste0(stem, ".csv"))
  html_file <- file.path(out_dir, paste0(stem, ".html"))
  tex_file  <- file.path(out_dir, paste0(stem, ".tex"))
  write.csv(df, csv_file, row.names = FALSE, na = "")
  con <- file(html_file, "wt")
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>",
    "body{font-family:Arial,Helvetica,sans-serif;margin:35px;color:#111;}",
    "table{border-collapse:collapse;width:900px;max-width:100%;font-size:14px;}",
    "caption{text-align:left;font-weight:bold;font-size:16px;margin-bottom:10px;}",
    "th{border-top:2px solid #222;border-bottom:1px solid #222;padding:7px 8px;text-align:left;}",
    "td{padding:5px 8px;vertical-align:top;}",
    "tr:last-child td{border-bottom:2px solid #222;}",
    ".note{font-size:12px;margin-top:8px;max-width:900px;}",
    "</style></head><body>",
    paste0("<table><caption>", html_escape(title), "</caption><thead><tr>")
  ), con)
  for (nm in names(df)) writeLines(paste0("<th>", html_escape(nm), "</th>"), con)
  writeLines("</tr></thead><tbody>", con)
  for (i in seq_len(nrow(df))) {
    writeLines("<tr>", con)
    for (j in seq_along(df)) {
      val <- ifelse(is.na(df[i, j]), "", as.character(df[i, j]))
      writeLines(paste0("<td>", html_escape(val), "</td>"), con)
    }
    writeLines("</tr>", con)
  }
  writeLines("</tbody></table>", con)
  if (!is.null(note)) writeLines(paste0("<div class='note'><b>Note:</b> ", html_escape(note), "</div>"), con)
  writeLines("</body></html>", con)
  close(con)
  con <- file(tex_file, "wt")
  ncol_df <- ncol(df)
  colspec <- paste0("p{", round(0.92 / ncol_df, 2), "\\\\textwidth}", collapse = "")
  writeLines(c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(title), "}"),
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    paste0(paste(latex_escape(names(df)), collapse = " & "), " \\\\"),
    "\\midrule"
  ), con)
  for (i in seq_len(nrow(df))) {
    vals <- vapply(df[i, , drop = FALSE], function(z) {
      z <- ifelse(is.na(z), "", as.character(z))
      latex_escape(z)
    }, character(1))
    writeLines(paste0(paste(vals, collapse = " & "), " \\\\"), con)
  }
  writeLines("\\bottomrule", con)
  writeLines("\\end{tabular}", con)
  if (!is.null(note)) writeLines(paste0("\\\\\\footnotesize{Note: ", latex_escape(note), "}"), con)
  writeLines("\\end{table}", con)
  close(con)
  invisible(list(csv = csv_file, html = html_file, tex = tex_file))
}
manifest <- data.frame(
  object = c(
    "01b Table1 display", "01b Table1 overall", "01b Table1 audit",
    "06f novel prevalence", "06f novel outcome", "06f novel characteristics",
    "07b cluster prevalence", "07b cluster outcome", "07b cluster characteristics",
    "raw RDS (optional representativeness table)"
  ),
  path = c(
    old_t1_display, old_t1_overall, old_t1_audit,
    novel_prev_file, novel_outcome_file, novel_char_file,
    cluster_prev_file, cluster_outcome_file, cluster_char_file,
    raw_rds
  )
)
manifest$exists <- file.exists(manifest$path)
write.csv(manifest, file.path(out_dir, "10b_input_manifest.csv"), row.names = FALSE)
cat("\n================ INPUT CHECK ================\n")
print(manifest[, c("object", "exists")], row.names = FALSE)
cat("=============================================\n\n")
if (file.exists(old_t1_display)) {
  t1 <- read.csv(old_t1_display, check.names = FALSE)
  names(t1) <- tolower(names(t1))
  char_col <- first_existing(c("characteristic", "variable"), names(t1))
  level_col <- first_existing(c("level", "category"), names(t1))
  value_col <- first_existing(c("value", "overall"), names(t1))
  if (any(is.na(c(char_col, level_col, value_col)))) {
    stop("01b Table 1 display file exists but expected columns could not be detected.")
  }
  t1_final <- data.frame(
    Characteristic = t1[[char_col]],
    Category = t1[[level_col]],
    `Overall, n (%)` = t1[[value_col]],
    check.names = FALSE
  )
  total_row <- tolower(t1_final$Characteristic) == "participants" &
    tolower(t1_final$Category) == "total"
  cohort_n <- NA_character_
  if (any(total_row)) {
    cohort_n <- t1_final[[3]][which(total_row)[1]]
    t1_final <- t1_final[!total_row, , drop = FALSE]
  }
  if (is.na(cohort_n) && file.exists(old_t1_audit)) {
    aud <- read.csv(old_t1_audit, check.names = FALSE)
    hit <- grep("Unique participants with any valid free text|Primary Table 1 denominator", aud$measure, ignore.case = TRUE)
    if (length(hit) > 0) cohort_n <- as.character(aud$n[hit[1]])
  }
  if (is.na(cohort_n)) cohort_n <- "4,102"
  write_simple_table(
    t1_final,
    title = paste0("Table 1. Characteristics of participants with valid free-text responses (N = ", cohort_n, ")"),
    stem = "Table1_valid_freetext_characteristics",
    note = "Values are n (%) unless otherwise stated. Missing, unknown and prefer-not-to-say responses are shown as Not specified."
  )
  cat("Created formal Table 1 from existing 01b aggregate output.\n")
} else {
  warning("01b Table 1 display file not found. Run 01b_generate_table1.R first, then rerun this script.")
}
if (file.exists(raw_rds)) {
  raw <- readRDS(raw_rds)
  req <- c("vaccrufuse1_19", "vaccrufuse2_19", "vaccrufuse1_19_other", "vaccrufuse2_19_other")
  if (all(req %in% names(raw))) {
    text1 <- trimws(as.character(raw$vaccrufuse1_19_other))
    text2 <- trimws(as.character(raw$vaccrufuse2_19_other))
    invalid <- c("-66", "-77", "-91", "-92")
    other1 <- as_binary(raw$vaccrufuse1_19) == 1
    other2 <- as_binary(raw$vaccrufuse2_19) == 1
    other1[is.na(other1)] <- FALSE
    other2[is.na(other2)] <- FALSE
    valid1 <- other1 & !is.na(text1) & text1 != "" & !(text1 %in% invalid)
    valid2 <- other2 & !is.na(text2) & text2 != "" & !(text2 %in% invalid)
    selected_other <- other1 | other2
    valid_any <- valid1 | valid2
    id_col <- first_existing(c("u_passcode", "participant_id", "participantid", "ID", "id"), names(raw))
    if (!is.na(id_col)) {
      z <- raw[selected_other, , drop = FALSE]
      z$.__pid__ <- as.character(z[[id_col]])
      z$.__group__ <- ifelse(valid_any[selected_other], "Valid free text", "No valid free text")
      z <- z[!duplicated(z$.__pid__), , drop = FALSE]
      compare_vars <- c(
        age_group_named = "Age group",
        sex = "Sex",
        ethnic_new = "Ethnicity",
        imd_quintile_cat = "IMD quintile",
        edu_cat = "Education",
        empl_cat_new = "Employment status",
        region_named = "Region"
      )
      compare_vars <- compare_vars[names(compare_vars) %in% names(z)]
      comp_rows <- list(); kk <- 1
      group_ns <- table(z$.__group__)
      for (v in names(compare_vars)) {
        label <- unname(compare_vars[[v]])
        x <- if (v == "ethnic_new") collapse_ethnicity(z[[v]]) else normalise_missing(z[[v]])
        for (lev in unique(x)) {
          row <- data.frame(Characteristic = label, Category = lev, check.names = FALSE)
          for (g in c("Valid free text", "No valid free text")) {
            den <- sum(z$.__group__ == g)
            num <- sum(z$.__group__ == g & x == lev)
            row[[g]] <- sprintf("%s (%.1f%%)", format(num, big.mark = ","), ifelse(den > 0, 100*num/den, NA_real_))
          }
          comp_rows[[kk]] <- row; kk <- kk + 1
        }
      }
      comp <- do.call(rbind, comp_rows)
      names(comp)[names(comp) == "Valid free text"] <- paste0("Valid free text (N = ", format(group_ns[["Valid free text"]], big.mark=","), ")")
      names(comp)[names(comp) == "No valid free text"] <- paste0("No valid free text (N = ", format(group_ns[["No valid free text"]], big.mark=","), ")")
      write_simple_table(
        comp,
        title = "Supplementary Table S1. Characteristics of participants selecting Other, by availability of valid free text",
        stem = "TableS1_selected_other_valid_vs_no_valid_text",
        note = "Descriptive comparison only; no hypothesis-test p-values are reported."
      )
      cat("Created optional representativeness / selection table.\n")
    }
  }
}
clean_label_key <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y <- gsub("cluster[[:space:]]*[0-9]+[: -]*", "", y)
  y <- gsub("[^a-z0-9]+", "", y)
  y
}
build_descriptive_summary <- function(prev_file, outcome_file, dict_file = NA_character_, kind = c("theme", "cluster")) {
  kind <- match.arg(kind)
  if (!file.exists(prev_file)) return(NULL)
  prev <- read.csv(prev_file, check.names = FALSE)
  nms <- names(prev)
  label_patterns <- if (kind == "theme") {
    c("theme.*label", "theme.*name", "final.*theme", "label", "theme")
  } else {
    c("cluster.*display", "cluster.*name", "cluster.*label", "label", "cluster")
  }
  label_col <- first_regex(label_patterns, nms)
  if (is.na(label_col)) label_col <- nms[[1]]
  n_col <- first_regex(c("^n$", "participant.*count", "count$", "^count$", "participants$"), nms, exclude = label_col)
  pct_col <- first_regex(c("percent", "percentage", "prevalence"), nms, exclude = c(label_col, n_col))
  den_col <- first_regex(c("denominator", "cohort.*n", "total.*n", "^N$"), nms, exclude = c(label_col, n_col, pct_col))
  out <- data.frame(Label = as.character(prev[[label_col]]), stringsAsFactors = FALSE)
  if (!is.na(n_col)) out$`Participants, n` <- prev[[n_col]]
  if (!is.na(pct_col)) {
    p <- suppressWarnings(as.numeric(as.character(prev[[pct_col]])))
    if (sum(!is.na(p)) > 0 && max(p, na.rm = TRUE) <= 1.01) p <- 100*p
    out$`Prevalence, %` <- ifelse(is.na(p), as.character(prev[[pct_col]]), sprintf("%.1f", p))
  } else if (!is.na(n_col) && !is.na(den_col)) {
    nn <- suppressWarnings(as.numeric(prev[[n_col]])); dd <- suppressWarnings(as.numeric(prev[[den_col]]))
    out$`Prevalence, %` <- sprintf("%.1f", 100*nn/dd)
  }
  if (file.exists(outcome_file)) {
    oc <- read.csv(outcome_file, check.names = FALSE)
    onms <- names(oc)
    olabel <- first_regex(label_patterns, onms)
    if (!is.na(olabel)) {
      out$.__key__ <- clean_label_key(out$Label)
      oc$.__key__ <- clean_label_key(oc[[olabel]])
      follow_n <- first_regex(c("follow.*n", "linked.*n", "outcome.*n", "eligible.*n", "denominator"), onms, exclude = olabel)
      vax_n <- first_regex(c("subsequent.*vacc.*n", "vaccinated.*n", "n.*vacc"), onms, exclude = c(olabel, follow_n))
      vax_pct <- first_regex(c("subsequent.*vacc.*percent", "vaccinated.*percent", "vaccinated.*prop", "vaccinated.*rate"), onms, exclude = c(olabel, follow_n, vax_n))
      persistent_n <- first_regex(c("persistent.*n", "unvacc.*n", "hesitan.*n"), onms, exclude = c(olabel, follow_n, vax_n, vax_pct))
      persistent_pct <- first_regex(c("persistent.*percent", "unvacc.*percent", "hesitan.*percent", "persistent.*prop"), onms, exclude = c(olabel, follow_n, vax_n, vax_pct, persistent_n))
      keep <- c(".__key__", follow_n, vax_n, vax_pct, persistent_n, persistent_pct)
      keep <- unique(keep[!is.na(keep)])
      oc2 <- oc[, keep, drop = FALSE]
      oc2 <- oc2[!duplicated(oc2$.__key__), , drop = FALSE]
      out <- merge(out, oc2, by = ".__key__", all.x = TRUE, sort = FALSE)
      if (!is.na(follow_n) && follow_n %in% names(out)) names(out)[names(out) == follow_n] <- "N with follow-up"
      if (!is.na(vax_n) && vax_n %in% names(out)) names(out)[names(out) == vax_n] <- "Subsequently vaccinated, n"
      if (!is.na(vax_pct) && vax_pct %in% names(out)) {
        p <- suppressWarnings(as.numeric(as.character(out[[vax_pct]])))
        if (sum(!is.na(p)) > 0 && max(p, na.rm = TRUE) <= 1.01) p <- 100*p
        out[[vax_pct]] <- ifelse(is.na(p), as.character(out[[vax_pct]]), sprintf("%.1f", p))
        names(out)[names(out) == vax_pct] <- "Subsequently vaccinated, %"
      }
      if (!is.na(persistent_n) && persistent_n %in% names(out)) names(out)[names(out) == persistent_n] <- "Persistent hesitancy, n"
      if (!is.na(persistent_pct) && persistent_pct %in% names(out)) {
        p <- suppressWarnings(as.numeric(as.character(out[[persistent_pct]])))
        if (sum(!is.na(p)) > 0 && max(p, na.rm = TRUE) <= 1.01) p <- 100*p
        out[[persistent_pct]] <- ifelse(is.na(p), as.character(out[[persistent_pct]]), sprintf("%.1f", p))
        names(out)[names(out) == persistent_pct] <- "Persistent hesitancy, %"
      }
      out$.__key__ <- NULL
    }
  }
  out
}
novel_summary <- build_descriptive_summary(
  novel_prev_file,
  novel_outcome_file,
  novel_dict_file,
  kind = "theme"
)
if (!is.null(novel_summary)) {
  write_simple_table(
    novel_summary,
    title = "Table 2. Descriptive summary of novel free-text hesitancy themes",
    stem = "Table2_novel_theme_descriptive_summary",
    note = "Theme prevalence is based on the valid free-text cohort. Vaccination outcome summaries use participants with eligible NHS-linked follow-up where available."
  )
  cat("Created novel-theme descriptive summary.\n")
} else {
  warning("Novel theme prevalence file not found; Table 2 was not created.")
}
cluster_summary <- build_descriptive_summary(
  cluster_prev_file,
  cluster_outcome_file,
  cluster_dict_file,
  kind = "cluster"
)
if (!is.null(cluster_summary)) {
  write_simple_table(
    cluster_summary,
    title = "Table 3. Descriptive summary of primary augmented vaccine hesitancy clusters",
    stem = "Table3_cluster_descriptive_summary",
    note = "Cluster indicators are not mutually exclusive at participant level. Prevalence is based on the primary clustering cohort. Vaccination outcome summaries use participants with eligible NHS-linked follow-up where available."
  )
  cat("Created cluster descriptive summary.\n")
} else {
  warning("Cluster prevalence file not found; Table 3 was not created.")
}
if (file.exists(novel_char_file)) {
  x <- read.csv(novel_char_file, check.names = FALSE)
  write.csv(x, file.path(out_dir, "TableS2_novel_theme_participant_characteristics_long.csv"), row.names = FALSE)
}
if (file.exists(cluster_char_file)) {
  x <- read.csv(cluster_char_file, check.names = FALSE)
  write.csv(x, file.path(out_dir, "TableS3_cluster_participant_characteristics_long.csv"), row.names = FALSE)
}
capture.output(sessionInfo(), file = file.path(out_dir, "10b_R_session_info.txt"))
cat("\n============================================================\n")
cat("STEP 10b FINAL DESCRIPTIVE TABLES COMPLETED\n")
cat("============================================================\n")
cat("Output folder:\n", out_dir, "\n\n", sep = "")
cat("Expected key outputs:\n")
cat("  Table1_valid_freetext_characteristics.html / .tex / .csv\n")
cat("  TableS1_selected_other_valid_vs_no_valid_text.html / .tex / .csv\n")
cat("  Table2_novel_theme_descriptive_summary.html / .tex / .csv\n")
cat("  Table3_cluster_descriptive_summary.html / .tex / .csv\n")
cat("============================================================\n")
