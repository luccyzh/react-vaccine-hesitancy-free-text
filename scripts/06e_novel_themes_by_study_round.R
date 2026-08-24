# ============================================================
# 06e_novel_themes_by_study_round.R
#
# Descriptive analysis only:
#   distribution of six novel themes across study rounds among
#   participants with valid response-level free text.
# ============================================================

project_root <- "D:/projects/hda_2026/han"
input_rds <- file.path(
  project_root, "outputs", "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)
output_dir <- file.path(
  project_root, "outputs", "06e_novel_themes_by_round"
)
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_rds)) stop("Required input not found: ", input_rds)
df <- readRDS(input_rds)

round_candidates <- c("study_round", "round", "react_round")
round_col <- round_candidates[round_candidates %in% names(df)][1]
if (is.na(round_col)) {
  stop(
    "No study-round variable found. Checked: ",
    paste(round_candidates, collapse = ", ")
  )
}

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

missing_cols <- setdiff(c("topic_id", novel_cols, round_col), names(df))
if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
}

free_text <- df[!is.na(df$topic_id), c("u_passcode", round_col, novel_cols), drop = FALSE]
free_text <- free_text[!is.na(free_text[[round_col]]), , drop = FALSE]

round_denominator <- as.data.frame(table(free_text[[round_col]]), stringsAsFactors = FALSE)
names(round_denominator) <- c("study_round", "n_free_text")

rows <- list()
idx <- 1
for (i in seq_along(novel_cols)) {
  v <- novel_cols[i]
  tab <- aggregate(
    as.integer(free_text[[v]] == 1),
    by = list(study_round = free_text[[round_col]]),
    FUN = sum,
    na.rm = TRUE
  )
  names(tab)[2] <- "n_theme"
  tab$variable <- v
  tab$theme <- novel_labels[i]
  rows[[idx]] <- tab
  idx <- idx + 1
}

theme_by_round <- do.call(rbind, rows)
theme_by_round$study_round <- as.character(theme_by_round$study_round)
round_denominator$study_round <- as.character(round_denominator$study_round)
theme_by_round <- merge(theme_by_round, round_denominator, by = "study_round", all.x = TRUE)
theme_by_round$percent_of_free_text_round <- 100 * theme_by_round$n_theme /
  theme_by_round$n_free_text

round_levels <- c("REACT-1, round 9", "REACT-1, round 10", "REACT-1, round 11", "REACT-1, round 12", "REACT-1, round 13",
  "REACT-1, round 17","REACT-1, round 18" ,"REACT-1, round 19" , "REACT-2, round 5" , "REACT-2, round 6" )
theme_by_round$study_round <- factor(
  theme_by_round$study_round,
  levels = round_levels
)

write.csv(
  theme_by_round,
  file.path(output_dir, "novel_themes_by_study_round.csv"),
  row.names = FALSE
)
write.csv(
  round_denominator,
  file.path(output_dir, "free_text_denominator_by_study_round.csv"),
  row.names = FALSE
)

theme_levels <- novel_labels
plot_matrix <- matrix(
  0,
  nrow = length(theme_levels),
  ncol = length(round_levels),
  dimnames = list(theme_levels, round_levels)
)

for (r in seq_len(nrow(theme_by_round))) {
  plot_matrix[
    theme_by_round$theme[r],
    theme_by_round$study_round[r]
  ] <- theme_by_round$percent_of_free_text_round[r]
}

png(
  file.path(figure_dir, "novel_theme_percent_by_study_round.png"),
  width = 2200, height = 1400, res = 180
)
par(mar = c(7, 5, 4, 2))
matplot(
  x = seq_along(round_levels),
  y = t(plot_matrix),
  type = "b",
  pch = seq_len(nrow(plot_matrix)),
  lty = 1,
  xaxt = "n",
  xlab = "Study round",
  ylab = "Theme prevalence among free-text respondents (%)",
  main = "Novel themes across study rounds"
)
axis(1, at = seq_along(round_levels), labels = round_levels, las = 2)
legend(
  "topright",
  legend = rownames(plot_matrix),
  pch = seq_len(nrow(plot_matrix)),
  lty = 1,
  cex = 0.75,
  bty = "n"
)
dev.off()

cat("\nStep 06e completed successfully.\n")
cat("Outputs saved to: ", output_dir, "\n", sep = "")
