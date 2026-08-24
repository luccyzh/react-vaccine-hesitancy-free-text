# ==============================================================================
# STEP 07B: MAP PRIMARY CLUSTERS TO PARTICIPANTS AND DESCRIBE THEM
# ==============================================================================
# Purpose
#   1. Read the final primary clustering result from Step 07.
#   2. Convert the seven reason/theme clusters into participant-level binary
#      indicators: a participant is positive for a cluster if they selected at
#      least one reason/theme assigned to that cluster.
#   3. Save a new downstream dataset containing the seven cluster indicators.
#   4. Produce descriptive tables and figures.
#
# Important
#   - This script DOES NOT rerun clustering.
#   - Primary clustering excludes original "Other" and excludes the
#     "Uncertainty / decision not yet made" novel theme.
#   - A participant may be positive for more than one cluster.
#   - Participants vaccinated before the REACT survey are excluded only from
#     subsequent-vaccination summaries.
# ==============================================================================

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 0. PATHS
# ------------------------------------------------------------------------------
project_root <- "D:/projects/hda_2026/han"

input_rds <- file.path(
  project_root, "outputs", "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)

primary_cluster_dir <- file.path(
  project_root, "outputs", "07_augmented_clustering",
  "primary_without_other"
)

cluster_assignment_file <- file.path(
  primary_cluster_dir, "cluster_assignments_unlabelled.csv"
)

output_dir <- file.path(
  project_root, "outputs", "07b_cluster_level_descriptive"
)
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. FINAL CLUSTER NAMES
# ------------------------------------------------------------------------------
cluster_names <- c(
  `1` = "Effectiveness and side-effect concerns",
  `2` = "Access and travel barriers",
  `3` = "Low perceived need for vaccination and distrust",
  `4` = "Personal health and adverse-reaction concerns",
  `5` = "General vaccine opposition and specific fears",
  `6` = "Context-specific and newly identified concerns",
  `7` = "Fertility and vaccination-need considerations"
)

cluster_vars <- paste0("cluster_", 1:7)

# ------------------------------------------------------------------------------
# 2. HELPERS
# ------------------------------------------------------------------------------
as_binary_zero <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  as.integer(!is.na(x_num) & x_num == 1)
}

is_positive_flag <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  !is.na(x) & x_chr %in% c("1", "true", "t", "yes", "y")
}

safe_percent <- function(n, denominator) {
  ifelse(denominator > 0, 100 * n / denominator, NA_real_)
}

save_plot_both <- function(plot_object, filename, width, height) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, paste0(filename, ".png")),
    plot = plot_object, width = width, height = height,
    units = "in", dpi = 320, bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(figure_dir, paste0(filename, ".pdf")),
    plot = plot_object, width = width, height = height,
    units = "in", bg = "white"
  )
}

# ------------------------------------------------------------------------------
# 3. PACKAGES
# ------------------------------------------------------------------------------
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# 4. LOAD AND VALIDATE INPUTS
# ------------------------------------------------------------------------------
if (!file.exists(input_rds)) {
  stop("Missing Step 06 enriched dataset: ", input_rds)
}
if (!file.exists(cluster_assignment_file)) {
  stop("Missing Step 07 primary cluster assignment file: ",
       cluster_assignment_file)
}

df <- readRDS(input_rds)
if (!is.data.frame(df)) {
  stop("Step 06 RDS did not contain a data.frame.")
}

cluster_map <- read.csv(
  cluster_assignment_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_map_cols <- c("variable", "cluster")
missing_map_cols <- setdiff(required_map_cols, names(cluster_map))
if (length(missing_map_cols) > 0) {
  stop(
    "Cluster assignment file is missing column(s): ",
    paste(missing_map_cols, collapse = ", ")
  )
}

cluster_map$cluster <- as.integer(cluster_map$cluster)

if (!setequal(sort(unique(cluster_map$cluster)), 1:7)) {
  stop(
    "Expected primary clusters 1-7, but found: ",
    paste(sort(unique(cluster_map$cluster)), collapse = ", ")
  )
}

if (nrow(cluster_map) != 27) {
  warning(
    "Primary cluster assignment file contains ", nrow(cluster_map),
    " variables rather than the expected 27."
  )
}

missing_cluster_variables <- setdiff(cluster_map$variable, names(df))
if (length(missing_cluster_variables) > 0) {
  stop(
    "The enriched participant dataset is missing clustering variable(s):\n",
    paste(missing_cluster_variables, collapse = "\n")
  )
}

# Add final names and save the definitive cluster dictionary.
cluster_map$cluster_name <- unname(cluster_names[as.character(cluster_map$cluster)])
cluster_map <- cluster_map[
  order(cluster_map$cluster, cluster_map$variable),
  c("cluster", "cluster_name", setdiff(names(cluster_map),
                                      c("cluster", "cluster_name")))
]

write.csv(
  cluster_map,
  file.path(output_dir, "primary_cluster_dictionary_final.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 5. CREATE PARTICIPANT-LEVEL CLUSTER INDICATORS
# ------------------------------------------------------------------------------
for (v in unique(cluster_map$variable)) {
  df[[v]] <- as_binary_zero(df[[v]])
}

for (k in 1:7) {
  member_vars <- cluster_map$variable[cluster_map$cluster == k]

  if (length(member_vars) == 0) {
    stop("Cluster ", k, " has no member variables.")
  }

  df[[paste0("cluster_", k)]] <- as.integer(
    rowSums(df[member_vars], na.rm = TRUE) > 0
  )
}

# Human-readable aliases are saved in a separate dictionary rather than used as
# column names, so downstream code remains safe and reproducible.
cluster_indicator_dictionary <- data.frame(
  variable = cluster_vars,
  cluster = 1:7,
  cluster_name = unname(cluster_names),
  stringsAsFactors = FALSE
)

write.csv(
  cluster_indicator_dictionary,
  file.path(output_dir, "cluster_indicator_dictionary.csv"),
  row.names = FALSE
)

# Save the full enriched participant-level dataset.
participant_output_rds <- file.path(
  output_dir,
  "df_with_augmented_reasons_novel_themes_and_clusters.rds"
)
saveRDS(df, participant_output_rds)

# ------------------------------------------------------------------------------
# 6. DEFINE ANALYSIS COHORTS
# ------------------------------------------------------------------------------
primary_hesitancy_var <- "analysis_4_vax_any_hes_or_refused_vs_vaxxed"
nhs_status_var <- "nhs_vaccine_status"

if (!primary_hesitancy_var %in% names(df)) {
  stop("Missing primary Definition 4 variable: ", primary_hesitancy_var)
}

definition4_flag <- is_positive_flag(df[[primary_hesitancy_var]])
df_definition4 <- df[definition4_flag, , drop = FALSE]

primary_reason_vars <- unique(cluster_map$variable)
primary_information_flag <- rowSums(
  df_definition4[primary_reason_vars],
  na.rm = TRUE
) > 0

df_primary <- df_definition4[
  primary_information_flag,
  ,
  drop = FALSE
]

if (nrow(df_definition4) != 37982) {
  warning(
    "Definition 4 cohort is ", nrow(df_definition4),
    ", not the reference value 37,982."
  )
}
if (nrow(df_primary) != 30143) {
  warning(
    "Primary clustering-eligible cohort is ", nrow(df_primary),
    ", not the reference value 30,143."
  )
}

cohort_qc <- data.frame(
  measure = c(
    "rows_in_enriched_dataset",
    "definition4_hesitant_cohort",
    "primary_clustering_variables",
    "primary_rows_with_at_least_one_selected_reason_or_theme",
    "primary_rows_all_zero_excluded",
    "participants_positive_for_more_than_one_cluster"
  ),
  value = c(
    nrow(df),
    nrow(df_definition4),
    length(primary_reason_vars),
    nrow(df_primary),
    sum(!primary_information_flag),
    sum(rowSums(df_primary[cluster_vars]) > 1)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  cohort_qc,
  file.path(output_dir, "cluster_mapping_qc_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 7. CLUSTER PREVALENCE
# ------------------------------------------------------------------------------
cluster_prevalence <- data.frame(
  variable = cluster_vars,
  cluster = 1:7,
  cluster_name = unname(cluster_names),
  n = vapply(df_primary[cluster_vars], sum, numeric(1)),
  denominator = nrow(df_primary),
  stringsAsFactors = FALSE
)
cluster_prevalence$percent <- safe_percent(
  cluster_prevalence$n,
  cluster_prevalence$denominator
)

write.csv(
  cluster_prevalence,
  file.path(output_dir, "cluster_prevalence_primary_cohort.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 8. SUBSEQUENT-VACCINATION DESCRIPTIVE SUMMARY
# ------------------------------------------------------------------------------
vaccination_summary <- NULL

if (nhs_status_var %in% names(df_primary)) {
  # Outcome definition:
  # Exclude participants whose NHS vaccination occurred before the REACT survey.
  df_followup <- df_primary[
    !is.na(df_primary[[nhs_status_var]]) &
      df_primary[[nhs_status_var]] != "Vaccine recorded before REACT survey",
    ,
    drop = FALSE
  ]

  df_followup$subs_vaxxed <- ifelse(
    df_followup[[nhs_status_var]] == "NHS vaccination recorded",
    0L,  # subsequently vaccinated
    1L   # no subsequent NHS vaccination: persistent hesitancy
  )

  vaccination_rows <- lapply(1:7, function(k) {
    v <- paste0("cluster_", k)
    exposed <- df_followup[df_followup[[v]] == 1, , drop = FALSE]

    data.frame(
      variable = v,
      cluster = k,
      cluster_name = unname(cluster_names[as.character(k)]),
      linked_n = nrow(exposed),
      subsequently_vaccinated_n = sum(exposed$subs_vaxxed == 0, na.rm = TRUE),
      persistently_unvaccinated_n = sum(exposed$subs_vaxxed == 1, na.rm = TRUE),
      subsequently_vaccinated_percent = safe_percent(
        sum(exposed$subs_vaxxed == 0, na.rm = TRUE),
        nrow(exposed)
      ),
      persistent_hesitancy_percent = safe_percent(
        sum(exposed$subs_vaxxed == 1, na.rm = TRUE),
        nrow(exposed)
      ),
      stringsAsFactors = FALSE
    )
  })

  vaccination_summary <- do.call(rbind, vaccination_rows)

  write.csv(
    vaccination_summary,
    file.path(output_dir, "subsequent_vaccination_by_cluster.csv"),
    row.names = FALSE
  )

  followup_qc <- data.frame(
    measure = c(
      "primary_clustering_eligible_rows",
      "rows_with_usable_post_survey_NHS_outcome",
      "excluded_vaccinated_before_REACT_survey",
      "subsequently_vaccinated_outcome_0",
      "persistent_hesitancy_outcome_1"
    ),
    value = c(
      nrow(df_primary),
      nrow(df_followup),
      sum(
        !is.na(df_primary[[nhs_status_var]]) &
          df_primary[[nhs_status_var]] ==
          "Vaccine recorded before REACT survey"
      ),
      sum(df_followup$subs_vaxxed == 0),
      sum(df_followup$subs_vaxxed == 1)
    ),
    stringsAsFactors = FALSE
  )

  write.csv(
    followup_qc,
    file.path(output_dir, "subsequent_vaccination_cohort_qc.csv"),
    row.names = FALSE
  )
} else {
  warning(
    "Variable ", nhs_status_var,
    " was not found. Subsequent-vaccination descriptive outputs were skipped."
  )
}

# ------------------------------------------------------------------------------
# 9. CHARACTERISTICS BY CLUSTER
# ------------------------------------------------------------------------------
characteristic_vars <- intersect(
  c("age_group_named", "sex", "study_round"),
  names(df_primary)
)

characteristic_output <- list()
idx <- 1L

for (k in 1:7) {
  cluster_df <- df_primary[
    df_primary[[paste0("cluster_", k)]] == 1,
    ,
    drop = FALSE
  ]

  for (char_var in characteristic_vars) {
    x <- as.character(cluster_df[[char_var]])
    x[is.na(x) | trimws(x) == ""] <- "Missing"

    tab <- as.data.frame(table(x), stringsAsFactors = FALSE)
    names(tab) <- c("level", "n")
    tab$percent <- safe_percent(tab$n, sum(tab$n))
    tab$cluster <- k
    tab$cluster_name <- unname(cluster_names[as.character(k)])
    tab$characteristic <- char_var

    characteristic_output[[idx]] <- tab[
      , c("cluster", "cluster_name", "characteristic",
          "level", "n", "percent")
    ]
    idx <- idx + 1L
  }
}

if (length(characteristic_output) > 0) {
  characteristic_long <- do.call(rbind, characteristic_output)
  write.csv(
    characteristic_long,
    file.path(output_dir, "participant_characteristics_by_cluster_long.csv"),
    row.names = FALSE
  )
}

# ------------------------------------------------------------------------------
# 10. DESCRIPTIVE FIGURES
# ------------------------------------------------------------------------------
plot_prevalence_data <- cluster_prevalence
plot_prevalence_data$cluster_name <- factor(
  plot_prevalence_data$cluster_name,
  levels = rev(unname(cluster_names))
)

p_prevalence <- ggplot2::ggplot(
  plot_prevalence_data,
  ggplot2::aes(x = cluster_name, y = percent)
) +
  ggplot2::geom_col(width = 0.7, fill = "#2C6E91") +
  ggplot2::geom_text(
    ggplot2::aes(
      label = paste0(
        scales::comma(n), " (",
        sprintf("%.1f", percent), "%)"
      )
    ),
    hjust = -0.08,
    size = 3.5
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = ggplot2::expansion(mult = c(0, 0.22))
  ) +
  ggplot2::labs(
    title = "Prevalence of the seven augmented hesitancy clusters",
    subtitle = paste0(
      "Primary clustering cohort (n = ",
      scales::comma(nrow(df_primary)), ")"
    ),
    x = NULL,
    y = "Participants positive for cluster (%)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 15),
    plot.subtitle = ggplot2::element_text(colour = "grey30"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 10),
    plot.margin = ggplot2::margin(10, 75, 10, 10)
  )

save_plot_both(
  p_prevalence,
  "cluster_prevalence_primary_cohort",
  width = 10,
  height = 6.5
)

if (!is.null(vaccination_summary)) {
  plot_vax_data <- vaccination_summary
  plot_vax_data$cluster_name <- factor(
    plot_vax_data$cluster_name,
    levels = rev(unname(cluster_names))
  )

  p_persistence <- ggplot2::ggplot(
    plot_vax_data,
    ggplot2::aes(x = cluster_name, y = persistent_hesitancy_percent)
  ) +
    ggplot2::geom_col(width = 0.7, fill = "#B44B4B") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          persistently_unvaccinated_n, "/",
          linked_n, " (",
          sprintf("%.1f", persistent_hesitancy_percent), "%)"
        )
      ),
      hjust = -0.08,
      size = 3.2
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.25))
    ) +
    ggplot2::labs(
      title = "Persistent hesitancy by augmented cluster",
      subtitle = paste0(
        "Descriptive, unadjusted proportions; ",
        "participants vaccinated before the survey excluded"
      ),
      x = NULL,
      y = "No subsequent NHS vaccination recorded (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(10, 100, 10, 10)
    )

  save_plot_both(
    p_persistence,
    "persistent_hesitancy_by_cluster_unadjusted",
    width = 10,
    height = 6.5
  )
}

# Cluster composition figure.
composition_plot_data <- cluster_map
composition_plot_data$cluster_label <- paste0(
  "Cluster ", composition_plot_data$cluster, ": ",
  composition_plot_data$cluster_name
)
composition_plot_data$label_order <- seq_len(nrow(composition_plot_data))

p_composition <- ggplot2::ggplot(
  composition_plot_data,
  ggplot2::aes(
    x = 1,
    y = reorder(
      if ("label" %in% names(composition_plot_data)) label else variable,
      -label_order
    )
  )
) +
  ggplot2::geom_point(size = 2.5, colour = "#2C6E91") +
  ggplot2::facet_grid(
    cluster_label ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  ggplot2::scale_x_continuous(NULL, breaks = NULL) +
  ggplot2::labs(
    title = "Composition of the seven primary augmented clusters",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 15),
    panel.grid = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(
      angle = 0, face = "bold", hjust = 1
    ),
    axis.text.y = ggplot2::element_text(hjust = 0),
    plot.margin = ggplot2::margin(10, 10, 10, 20)
  )

save_plot_both(
  p_composition,
  "primary_cluster_composition",
  width = 11,
  height = 11
)

# ------------------------------------------------------------------------------
# 11. COMPLETION MESSAGE
# ------------------------------------------------------------------------------
cat("\nStep 07b completed successfully.\n")
cat("Participant-level output:\n", participant_output_rds, "\n", sep = "")
cat("Primary clustering cohort:", nrow(df_primary), "\n")
cat("Outputs saved to:", output_dir, "\n")
