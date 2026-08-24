
# ==============================================================================
# 09_final_report_figures_and_tables_FINAL.R
#
# PURPOSE
#   Create report figures and aggregate result tables from
#   outputs already produced in Steps 01-08.
#
# Notes
#   * Does NOT rerun BERTopic.
#   * Does NOT rerun clustering.
#   * Does NOT rerun logistic regression.
#   * Does NOT read participant-level RDS files.
#   * Uses only simple packages already used in this project:
#       dplyr, ggplot2, readr, stringr, scales
#   * Forest plots are drawn from ModelMakerMultiRD aggregate output CSVs.
#     Therefore ORs / CIs / p-values remain model outputs; only the
#     presentation layer is redrawn with ggplot2.
# ==============================================================================

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "ggplot2", "readr", "stringr", "scales")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nThese are the only packages required by Step 09."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------------------------
project_root <- "D:/projects/hda_2026/han"
output_root  <- file.path(project_root, "outputs")

out09           <- file.path(output_root, "09_final_results")
main_fig_dir    <- file.path(out09, "main_figures")
meeting_fig_dir <- file.path(out09, "meeting_figures")
supp_fig_dir    <- file.path(out09, "supplementary_figures")
table_dir       <- file.path(out09, "tables")
qc_dir          <- file.path(out09, "qc")

# This folder contains candidate outputs for disclosure review.
# Candidate outputs still require disclosure review before export.
review_dir      <- file.path(out09, "disclosure_review_candidate")

for (p in c(
  out09, main_fig_dir, meeting_fig_dir, supp_fig_dir,
  table_dir, qc_dir, review_dir
)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------------------------
# 2. INPUT FILES
# ------------------------------------------------------------------------------
f <- list(
  # Step 01
  sample_flow = file.path(
    output_root, "01_data_preparation", "sample_flow_and_counts.csv"
  ),

  # Step 04: model comparison (supplementary)
  coherence_png = file.path(
    output_root, "04_model_comparison", "figures", "coherence_comparison.png"
  ),
  outlier_png = file.path(
    output_root, "04_model_comparison", "figures", "outlier_rate_comparison.png"
  ),

  # Step 06
  novel_theme_summary = file.path(
    output_root, "06_participant_mapping", "novel_theme_summary.csv"
  ),
  novel_topic_to_theme = file.path(
    output_root, "06_participant_mapping",
    "novel_topic_to_final_theme_mapping.csv"
  ),
  original_vs_augmented = file.path(
    output_root, "06d_mapping_qc_and_comparison",
    "existing_reason_original_vs_augmented_definition4.csv"
  ),
  novel_theme_counts = file.path(
    output_root, "06d_mapping_qc_and_comparison",
    "novel_theme_counts.csv"
  ),
  umap_novel_png = file.path(
    output_root, "06c_interpretable_umap", "figures",
    "response_umap_final_novel_themes.png"
  ),
  manual_dendrogram_png = file.path(
    output_root, "06b_dendrogram_comparison", "figures",
    "response_full_dendrogram_manual_overlay.png"
  ),

  # Step 07
  cluster_assignments = file.path(
    output_root, "07_augmented_clustering", "primary_without_other",
    "cluster_assignments_unlabelled.csv"
  ),
  cluster_primary_qc = file.path(
    output_root, "07_augmented_clustering", "primary_without_other",
    "qc_summary.csv"
  ),
  primary_vs_sensitivity = file.path(
    output_root, "07_augmented_clustering",
    "primary_vs_sensitivity_summary.csv"
  ),
  cluster_prevalence = file.path(
    output_root, "07b_cluster_level_descriptive",
    "cluster_prevalence_primary_cohort.csv"
  ),

  # Step 08 aggregate model outputs
  followup_qc = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "followup_outcome_qc.csv"
  ),
  method_record = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "analysis_method_record.csv"
  ),
  reason_primary = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "reason_theme_primary_plot_data.csv"
  ),
  reason_round = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "reason_theme_round_plot_data.csv"
  ),
  cluster_primary = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "cluster_primary_plot_data.csv"
  ),
  cluster_round = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "cluster_round_plot_data.csv"
  ),
  original_other_primary = file.path(
    output_root, "08_follow_up_vaccination_models", "tables",
    "original_other_primary_age_sex_df_output.csv"
  )
)

# ------------------------------------------------------------------------------
# 3. HELPERS
# ------------------------------------------------------------------------------
read_csv_checked <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    msg <- paste0("Missing input: ", path)
    if (required) stop(msg) else {
      warning(msg)
      return(NULL)
    }
  }
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

require_cols <- function(dat, cols, object_name) {
  miss <- setdiff(cols, names(dat))
  if (length(miss) > 0) {
    stop(
      object_name, " is missing column(s): ",
      paste(miss, collapse = ", ")
    )
  }
}

save_plot <- function(plot_object, folder, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(folder, paste0(filename, ".png")),
    plot = plot_object,
    width = width, height = height, units = "in",
    dpi = 320, bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(folder, paste0(filename, ".pdf")),
    plot = plot_object,
    width = width, height = height, units = "in",
    bg = "white"
  )
}

copy_if_exists <- function(source, destination, new_name = NULL) {
  if (!file.exists(source)) return(invisible(FALSE))
  if (is.null(new_name)) new_name <- basename(source)
  file.copy(source, file.path(destination, new_name), overwrite = TRUE)
}

pick_col <- function(dat, candidates) {
  hit <- candidates[candidates %in% names(dat)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Plot theme.
theme_report <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size),
      axis.title = element_text(face = "plain"),
      strip.background = element_rect(fill = "white", colour = "grey40"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# ------------------------------------------------------------------------------
# 4. INPUT MANIFEST
# ------------------------------------------------------------------------------
input_manifest <- data.frame(
  object = names(f),
  path = unname(unlist(f)),
  exists = file.exists(unname(unlist(f))),
  stringsAsFactors = FALSE
)

write.csv(
  input_manifest,
  file.path(qc_dir, "09_input_manifest.csv"),
  row.names = FALSE
)

cat("\n================ STEP 09 INPUT CHECK ================\n")
print(input_manifest[, c("object", "exists")], row.names = FALSE)
cat("=====================================================\n\n")

# ==============================================================================
# FIGURE 1. ANALYTICAL COHORT FLOW
# ==============================================================================
# Deliberately shown as two analytical branches because the free-text and
# downstream vaccination analyses have different denominators.
#
# These counts have already been validated in Steps 01 / 08:
#   Full participant dataset                       1,137,927
#   Selected Other                                  4,237
#   Valid free-text responses                       4,102
#   Definition 4 hesitant cohort                   37,982
#   NHS-linked follow-up cohort                    24,229
#   Persistent hesitancy                           8,485
#   Subsequently vaccinated                       15,744

flow_boxes <- data.frame(
  branch = c(
    "Free-text analysis", "Free-text analysis", "Free-text analysis",
    "Outcome analysis", "Outcome analysis", "Outcome analysis"
  ),
  x = c(1, 1, 1, 2, 2, 2),
  y = c(3, 2, 1, 3, 2, 1),
  label = c(
    "Full participant dataset\nn = 1,137,927",
    "Selected Other\nn = 4,237",
    "Valid free-text responses\nn = 4,102",
    "Definition 4 hesitant cohort\nn = 37,982",
    "NHS-linked follow-up cohort\nn = 24,229",
    "Persistent hesitancy: 8,485\nSubsequently vaccinated: 15,744"
  )
)

flow_arrows <- data.frame(
  x = c(1, 1, 2, 2),
  xend = c(1, 1, 2, 2),
  y = c(2.80, 1.80, 2.80, 1.80),
  yend = c(2.20, 1.20, 2.20, 1.20)
)

p_flow <- ggplot() +
  geom_segment(
    data = flow_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.16, "inches")),
    linewidth = 0.55
  ) +
  geom_label(
    data = flow_boxes,
    aes(x = x, y = y, label = label),
    size = 4.0,
    label.size = 0.35,
    label.padding = grid::unit(0.20, "lines"),
    lineheight = 1.05
  ) +
  annotate(
    "text", x = 1, y = 3.55,
    label = "Free-text analysis", fontface = "bold", size = 4.5
  ) +
  annotate(
    "text", x = 2, y = 3.55,
    label = "NHS-linked outcome analysis", fontface = "bold", size = 4.5
  ) +
  xlim(0.4, 2.6) +
  ylim(0.6, 3.8) +
  theme_void() +
  labs(
    title = "Analytical cohorts used in the study"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

save_plot(p_flow, main_fig_dir, "Figure1_analytical_cohort_flow", 10, 6)
save_plot(p_flow, meeting_fig_dir, "Meeting_analytical_cohort_flow", 10, 6)

write.csv(
  flow_boxes[, c("branch", "label")],
  file.path(table_dir, "analytical_cohort_flow_counts.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 2. FREE-TEXT TOPIC INTERPRETATION FLOW
# ==============================================================================
# A flow diagram is used instead of a bar chart because these values are
# different units (responses, topics, and final themes).

topic_boxes <- data.frame(
  x = c(1, 2, 3, 4, 4, 5),
  y = c(2, 2, 2, 2.6, 1.4, 1.4),
  label = c(
    "Valid free-text\nresponses\nn = 4,102",
    "Response-level\nBERTopic",
    "88 non-outlier\ntopics",
    "69 topics mapped to\nexisting VACCRUFUSE3\nreasons",
    "19 novel topics",
    "6 broader\nnovel themes"
  )
)

topic_arrows <- data.frame(
  x = c(1.20, 2.20, 3.20, 3.20, 4.20),
  xend = c(1.80, 2.80, 3.78, 3.78, 4.80),
  y = c(2, 2, 2, 2, 1.4),
  yend = c(2, 2, 2.6, 1.4, 1.4)
)

p_topic_flow <- ggplot() +
  geom_segment(
    data = topic_arrows,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.14, "inches")),
    linewidth = 0.5
  ) +
  geom_label(
    data = topic_boxes,
    aes(x = x, y = y, label = label),
    size = 3.7,
    label.size = 0.35,
    lineheight = 1.05
  ) +
  xlim(0.6, 5.5) +
  ylim(0.8, 3.1) +
  theme_void() +
  labs(
    title = "From free-text responses to interpretable hesitancy themes",
    subtitle = "BERTopic outputs were manually reviewed and mapped to existing or novel reasons"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10.5)
  )

save_plot(
  p_topic_flow, main_fig_dir,
  "Figure2_free_text_topic_interpretation_flow", 11, 5
)
save_plot(
  p_topic_flow, meeting_fig_dir,
  "Meeting_free_text_topic_interpretation_flow", 11, 5
)

# Also save the aggregate novel-theme tables for report writing.
novel_summary <- read_csv_checked(f$novel_theme_summary, required = FALSE)
if (!is.null(novel_summary)) {
  write.csv(
    novel_summary,
    file.path(table_dir, "final_novel_theme_summary.csv"),
    row.names = FALSE
  )
}
novel_map <- read_csv_checked(f$novel_topic_to_theme, required = FALSE)
if (!is.null(novel_map)) {
  write.csv(
    novel_map,
    file.path(table_dir, "novel_topic_to_final_theme_mapping.csv"),
    row.names = FALSE
  )
}

# ==============================================================================
# FIGURE 3A. ORIGINAL STRUCTURED VS AUGMENTED EXISTING REASONS
# ==============================================================================
orig_aug <- read_csv_checked(f$original_vs_augmented)

label_col <- pick_col(
  orig_aug,
  c("label", "reason_label", "reason", "Variable", "variable_label")
)
original_col <- pick_col(
  orig_aug,
  c("original_n", "original_count", "n_original", "structured_n", "original")
)
augmented_col <- pick_col(
  orig_aug,
  c("augmented_n", "augmented_count", "n_augmented", "augmented")
)

# Conservative fallback if exact names differ.
if (is.na(label_col)) {
  chr_cols <- names(orig_aug)[vapply(orig_aug, is.character, logical(1))]
  if (length(chr_cols) > 0) label_col <- chr_cols[1]
}
numeric_cols <- names(orig_aug)[vapply(orig_aug, is.numeric, logical(1))]
if (is.na(original_col) && length(numeric_cols) >= 1) original_col <- numeric_cols[1]
if (is.na(augmented_col) && length(numeric_cols) >= 2) augmented_col <- numeric_cols[2]

if (any(is.na(c(label_col, original_col, augmented_col)))) {
  stop(
    "Could not identify label/original/augmented columns in:\n",
    f$original_vs_augmented,
    "\nColumn names are: ", paste(names(orig_aug), collapse = ", ")
  )
}

aug_df <- orig_aug %>%
  transmute(
    reason = as.character(.data[[label_col]]),
    original = as.numeric(.data[[original_col]]),
    augmented = as.numeric(.data[[augmented_col]]),
    additional_from_text = augmented - original
  ) %>%
  filter(
    !is.na(reason), !is.na(original), !is.na(augmented),
    augmented >= original
  ) %>%
  arrange(augmented) %>%
  mutate(reason = factor(reason, levels = reason))

p_aug <- ggplot(aug_df, aes(y = reason)) +
  geom_segment(
    aes(x = original, xend = augmented, yend = reason),
    linewidth = 0.7
  ) +
  geom_point(
    aes(x = original, shape = "Original structured"),
    size = 2.8
  ) +
  geom_point(
    aes(x = augmented, shape = "Augmented"),
    size = 2.8
  ) +
  scale_shape_manual(
    values = c("Original structured" = 1, "Augmented" = 16)
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_report(10.5) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    x = "Participants",
    y = NULL,
    shape = NULL,
    title = "Existing hesitancy reasons before and after free-text augmentation",
    subtitle = "Augmented counts combine structured responses with matching reasons recovered from free text"
  )

save_plot(
  p_aug, main_fig_dir,
  "Figure3A_existing_reasons_original_vs_augmented", 10.5, 8.5
)
save_plot(
  p_aug, meeting_fig_dir,
  "Meeting_existing_reasons_original_vs_augmented", 10.5, 8.5
)

write.csv(
  aug_df,
  file.path(table_dir, "existing_reasons_original_vs_augmented_final.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 3B. NOVEL THEMES IDENTIFIED FROM FREE TEXT
# ==============================================================================
novel_counts <- read_csv_checked(f$novel_theme_counts)

theme_col <- pick_col(
  novel_counts,
  c("theme", "theme_label", "novel_theme", "label", "Variable")
)
count_col <- pick_col(
  novel_counts,
  c("n", "count", "N", "theme_n", "participants")
)

if (is.na(theme_col)) {
  chr_cols <- names(novel_counts)[vapply(novel_counts, is.character, logical(1))]
  if (length(chr_cols) > 0) theme_col <- chr_cols[1]
}
if (is.na(count_col)) {
  numeric_cols <- names(novel_counts)[vapply(novel_counts, is.numeric, logical(1))]
  if (length(numeric_cols) > 0) count_col <- numeric_cols[1]
}

if (any(is.na(c(theme_col, count_col)))) {
  stop(
    "Could not identify theme/count columns in:\n",
    f$novel_theme_counts,
    "\nColumn names are: ", paste(names(novel_counts), collapse = ", ")
  )
}

novel_df <- novel_counts %>%
  transmute(
    theme = as.character(.data[[theme_col]]),
    n = as.numeric(.data[[count_col]])
  ) %>%
  filter(!is.na(theme), !is.na(n)) %>%
  arrange(n) %>%
  mutate(theme = factor(theme, levels = theme))

p_novel <- ggplot(novel_df, aes(x = n, y = theme)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.15, size = 3.6
  ) +
  expand_limits(x = max(novel_df$n, na.rm = TRUE) * 1.15) +
  theme_report(10.5) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Participants",
    y = NULL,
    title = "Novel hesitancy themes identified from free text"
  )

save_plot(
  p_novel, main_fig_dir,
  "Figure3B_novel_theme_counts", 8.5, 5.5
)
save_plot(
  p_novel, meeting_fig_dir,
  "Meeting_novel_theme_counts", 8.5, 5.5
)

write.csv(
  novel_df,
  file.path(table_dir, "novel_theme_counts_final.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 4. FINAL PRIMARY AUGMENTED CLUSTERING
# ==============================================================================
# Use the variable-level cluster assignment file already validated in Step 07.
cluster_map <- read_csv_checked(f$cluster_assignments)
require_cols(cluster_map, c("label", "variable", "source", "cluster"),
             "cluster_assignments_unlabelled.csv")

cluster_names <- c(
  "1" = "Effectiveness and side-effect concerns",
  "2" = "Access and travel barriers",
  "3" = "Low perceived need for vaccination and distrust",
  "4" = "Personal health and adverse-reaction concerns",
  "5" = "General vaccine opposition and specific fears",
  "6" = "Context-specific and newly identified concerns",
  "7" = "Fertility and vaccination-need considerations"
)

cl <- cluster_map %>%
  mutate(
    cluster = as.integer(cluster),
    cluster_name = unname(cluster_names[as.character(cluster)]),
    cluster_display = paste0("Cluster ", cluster, ": ", cluster_name)
  ) %>%
  arrange(cluster, label)

# Academic, easy-to-read "cluster membership map":
# one row per reason/theme, faceted by final cluster.
cl <- cl %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  )

p_cluster_map <- ggplot(cl, aes(x = 1, y = label)) +
  geom_point(size = 2.7) +
  facet_grid(
    cluster_display ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  xlim(0.8, 1.2) +
  theme_report(9.5) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0, hjust = 1, size = 8.5),
    legend.position = "none"
  ) +
  labs(
    title = "Primary augmented hesitancy clusters",
    subtitle = "Primary analysis excluded the broad Other indicator; uncertainty was excluded from clustering"
  )

save_plot(
  p_cluster_map, main_fig_dir,
  "Figure4_primary_augmented_cluster_membership", 10, 10
)
save_plot(
  p_cluster_map, meeting_fig_dir,
  "Meeting_primary_augmented_cluster_membership", 10, 10
)

write.csv(
  cl,
  file.path(table_dir, "primary_cluster_membership_final.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 5. REASON/THEME-LEVEL FOREST PLOT
# ==============================================================================
reason_primary <- read_csv_checked(f$reason_primary)
require_cols(
  reason_primary,
  c("Variable", "Category", "OR", "Lower", "Upper", "adjustment"),
  "reason_theme_primary_plot_data.csv"
)

rp <- reason_primary %>%
  mutate(
    OR = as.numeric(OR),
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper),
    adjustment = as.numeric(adjustment)
  ) %>%
  filter(
    adjustment == 2,
    is.finite(OR), is.finite(Lower), is.finite(Upper),
    OR > 0, Lower > 0, Upper > 0
  )

# Preserve the cluster order created in Step 08.
rp <- rp %>%
  mutate(
    Variable = factor(Variable, levels = unique(Variable))
  )

# Within each cluster, preserve current Step-08 order.
rp$Category_plot <- factor(
  paste(rp$Variable, rp$Category, sep = "___"),
  levels = rev(unique(paste(rp$Variable, rp$Category, sep = "___")))
)

label_lookup <- setNames(
  as.character(rp$Category),
  as.character(rp$Category_plot)
)

# Use segments rather than geom_errorbarh to avoid package/version warnings.
p_reason <- ggplot(rp, aes(y = Category_plot)) +
  geom_vline(xintercept = 1, linewidth = 0.45) +
  geom_segment(
    aes(x = Lower, xend = Upper, yend = Category_plot),
    linewidth = 0.65
  ) +
  geom_point(aes(x = OR), size = 2.3) +
  scale_x_log10(
    breaks = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4, 8, 16),
    labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16")
  ) +
  scale_y_discrete(labels = label_lookup) +
  facet_grid(
    Variable ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  theme_report(9.5) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(face = "bold", size = 8.4),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Odds ratio for persistent hesitancy (95% CI)",
    y = NULL,
    title = "Hesitancy reasons and novel themes associated with persistent hesitancy",
    subtitle = "Separate logistic regression models adjusted for age and sex"
  )

save_plot(
  p_reason, main_fig_dir,
  "Figure5_reason_theme_primary_forest", 10.5, 12.5
)
save_plot(
  p_reason, meeting_fig_dir,
  "Meeting_reason_theme_primary_forest", 10.5, 12.5
)

write.csv(
  rp,
  file.path(table_dir, "reason_theme_primary_adjusted_results.csv"),
  row.names = FALSE
)

# ==============================================================================
# FIGURE 6. CLUSTER-LEVEL FOREST PLOT
# ==============================================================================
cluster_primary <- read_csv_checked(f$cluster_primary)
require_cols(
  cluster_primary,
  c("Category", "OR", "Lower", "Upper", "adjustment"),
  "cluster_primary_plot_data.csv"
)

cp <- cluster_primary %>%
  mutate(
    OR = as.numeric(OR),
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper),
    adjustment = as.numeric(adjustment)
  ) %>%
  filter(
    adjustment == 2,
    is.finite(OR), is.finite(Lower), is.finite(Upper),
    OR > 0, Lower > 0, Upper > 0
  ) %>%
  mutate(
    Category = factor(Category, levels = rev(unique(Category)))
  )

p_cluster_forest <- ggplot(cp, aes(y = Category)) +
  geom_vline(xintercept = 1, linewidth = 0.45) +
  geom_segment(
    aes(x = Lower, xend = Upper, yend = Category),
    linewidth = 0.75
  ) +
  geom_point(aes(x = OR), size = 2.6) +
  scale_x_log10(
    breaks = c(1/4, 1/2, 1, 2, 4),
    labels = c("0.25", "0.5", "1", "2", "4")
  ) +
  theme_report(11) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Odds ratio for persistent hesitancy (95% CI)",
    y = NULL,
    title = "Augmented hesitancy clusters associated with persistent hesitancy",
    subtitle = "Separate logistic regression models adjusted for age and sex"
  )

save_plot(
  p_cluster_forest, main_fig_dir,
  "Figure6_cluster_primary_forest", 9.5, 6.2
)
save_plot(
  p_cluster_forest, meeting_fig_dir,
  "Meeting_cluster_primary_forest", 9.5, 6.2
)

write.csv(
  cp,
  file.path(table_dir, "cluster_primary_adjusted_results.csv"),
  row.names = FALSE
)

# ==============================================================================
# SUPPLEMENTARY OUTPUTS
# ==============================================================================
copy_if_exists(
  f$coherence_png, supp_fig_dir,
  "S1_response_vs_sentence_coherence.png"
)
copy_if_exists(
  f$outlier_png, supp_fig_dir,
  "S2_response_vs_sentence_outlier_rate.png"
)
copy_if_exists(
  f$umap_novel_png, supp_fig_dir,
  "S3_novel_theme_umap.png"
)
copy_if_exists(
  f$manual_dendrogram_png, supp_fig_dir,
  "S4_manual_grouping_dendrogram_overlay.png"
)

copy_if_exists(
  f$coherence_png, meeting_fig_dir,
  "Meeting_backup_response_vs_sentence_coherence.png"
)
copy_if_exists(
  f$outlier_png, meeting_fig_dir,
  "Meeting_backup_response_vs_sentence_outlier_rate.png"
)
copy_if_exists(
  f$umap_novel_png, meeting_fig_dir,
  "Meeting_backup_novel_theme_umap.png"
)

# Round-adjusted reason/theme forest plot
reason_round <- read_csv_checked(f$reason_round, required = FALSE)

if (!is.null(reason_round)) {
  require_cols(
    reason_round,
    c("Variable", "Category", "OR", "Lower", "Upper", "adjustment"),
    "reason_theme_round_plot_data.csv"
  )

  rr <- reason_round %>%
    mutate(
      OR = as.numeric(OR),
      Lower = as.numeric(Lower),
      Upper = as.numeric(Upper),
      adjustment = as.numeric(adjustment)
    ) %>%
    filter(
      adjustment == max(adjustment, na.rm = TRUE),
      is.finite(OR), is.finite(Lower), is.finite(Upper),
      OR > 0, Lower > 0, Upper > 0
    ) %>%
    mutate(
      Variable = factor(Variable, levels = unique(Variable))
    )

  rr$Category_plot <- factor(
    paste(rr$Variable, rr$Category, sep = "___"),
    levels = rev(unique(paste(rr$Variable, rr$Category, sep = "___")))
  )

  rr_labels <- setNames(
    as.character(rr$Category),
    as.character(rr$Category_plot)
  )

  p_rr <- ggplot(rr, aes(y = Category_plot)) +
    geom_vline(xintercept = 1, linewidth = 0.45) +
    geom_segment(
      aes(x = Lower, xend = Upper, yend = Category_plot),
      linewidth = 0.65
    ) +
    geom_point(aes(x = OR), size = 2.2) +
    scale_x_log10(
      breaks = c(1/16, 1/8, 1/4, 1/2, 1, 2, 4, 8, 16),
      labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16")
    ) +
    scale_y_discrete(labels = rr_labels) +
    facet_grid(
      Variable ~ .,
      scales = "free_y",
      space = "free_y"
    ) +
    theme_report(9.5) +
    theme(
      panel.grid.major.y = element_blank(),
      strip.text.y = element_text(face = "bold", size = 8.4),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = "Odds ratio for persistent hesitancy (95% CI)",
      y = NULL,
      title = "Reason/theme associations: round-adjusted sensitivity",
      subtitle = "Adjusted for age, sex, and survey-round midpoint"
    )

  save_plot(
    p_rr, supp_fig_dir,
    "S5_reason_theme_round_adjusted_forest", 10.5, 12.5
  )
}

# Round-adjusted cluster forest plot
cluster_round <- read_csv_checked(f$cluster_round, required = FALSE)

if (!is.null(cluster_round)) {
  require_cols(
    cluster_round,
    c("Category", "OR", "Lower", "Upper", "adjustment"),
    "cluster_round_plot_data.csv"
  )

  cr <- cluster_round %>%
    mutate(
      OR = as.numeric(OR),
      Lower = as.numeric(Lower),
      Upper = as.numeric(Upper),
      adjustment = as.numeric(adjustment)
    ) %>%
    filter(
      adjustment == max(adjustment, na.rm = TRUE),
      is.finite(OR), is.finite(Lower), is.finite(Upper),
      OR > 0, Lower > 0, Upper > 0
    ) %>%
    mutate(
      Category = factor(Category, levels = rev(unique(Category)))
    )

  p_cr <- ggplot(cr, aes(y = Category)) +
    geom_vline(xintercept = 1, linewidth = 0.45) +
    geom_segment(
      aes(x = Lower, xend = Upper, yend = Category),
      linewidth = 0.75
    ) +
    geom_point(aes(x = OR), size = 2.5) +
    scale_x_log10(
      breaks = c(1/4, 1/2, 1, 2, 4),
      labels = c("0.25", "0.5", "1", "2", "4")
    ) +
    theme_report(11) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    labs(
      x = "Odds ratio for persistent hesitancy (95% CI)",
      y = NULL,
      title = "Cluster associations: round-adjusted sensitivity",
      subtitle = "Adjusted for age, sex, and survey-round midpoint"
    )

  save_plot(
    p_cr, supp_fig_dir,
    "S6_cluster_round_adjusted_forest", 9.5, 6.2
  )
}

# ==============================================================================
# AGGREGATE TABLES FOR REPORT WRITING
# ==============================================================================
for (nm in c(
  "cluster_primary_qc",
  "primary_vs_sensitivity",
  "cluster_prevalence",
  "followup_qc",
  "method_record"
)) {
  dat <- read_csv_checked(f[[nm]], required = FALSE)
  if (!is.null(dat)) {
    write.csv(
      dat,
      file.path(table_dir, paste0(nm, ".csv")),
      row.names = FALSE
    )
  }
}

# ==============================================================================
# DISCLOSURE-REVIEW CANDIDATE FOLDER
# ==============================================================================
# Notes:
#   This is a small whitelist of candidate outputs for disclosure review.
#   It is NOT a statement that the files are automatically disclosure-safe.
#
#   No participant-level RDS, raw text, embeddings, IDs, or document-level
#   outputs are copied here.
# ==============================================================================

review_main <- file.path(review_dir, "figures")
review_tables <- file.path(review_dir, "aggregate_tables")
dir.create(review_main, recursive = TRUE, showWarnings = FALSE)
dir.create(review_tables, recursive = TRUE, showWarnings = FALSE)

# Final figures
for (src in list.files(main_fig_dir, full.names = TRUE)) {
  file.copy(src, review_main, overwrite = TRUE)
}

# Very small aggregate table whitelist.
aggregate_whitelist <- c(
  "analytical_cohort_flow_counts.csv",
  "existing_reasons_original_vs_augmented_final.csv",
  "novel_theme_counts_final.csv",
  "primary_cluster_membership_final.csv",
  "reason_theme_primary_adjusted_results.csv",
  "cluster_primary_adjusted_results.csv",
  "followup_qc.csv",
  "method_record.csv"
)

for (nm in aggregate_whitelist) {
  src <- file.path(table_dir, nm)
  if (file.exists(src)) {
    file.copy(src, review_tables, overwrite = TRUE)
  }
}

review_manifest <- data.frame(
  file = list.files(review_dir, recursive = TRUE),
  stringsAsFactors = FALSE
)

write.csv(
  review_manifest,
  file.path(review_dir, "DISCLOSURE_REVIEW_MANIFEST.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat("STEP 09 COMPLETED\n")
cat("Main figures: ", main_fig_dir, "\n", sep = "")
cat("Meeting figures: ", meeting_fig_dir, "\n", sep = "")
cat("Supplementary figures: ", supp_fig_dir, "\n", sep = "")
cat("Tables: ", table_dir, "\n", sep = "")
cat("Candidate files for disclosure review (not automatically safe):\n")
cat(review_dir, "\n")
cat("============================================================\n")
