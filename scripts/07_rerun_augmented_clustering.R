# Augmented reason-level consensus clustering.
# Re-runs the primary and sensitivity clustering specifications and exports QC, cluster assignments and figures.

rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)
project_root <- "D:/projects/hda_2026/han"
matt_project_root <- "D:/projects/vaccine_hesitancy_2024"
input_rds <- file.path(
  project_root,
  "outputs",
  "06_participant_mapping",
  "df_with_text_and_novel_theme_variables.rds"
)
existing_label_csv <- file.path(
  project_root,
  "outputs",
  "06d_mapping_qc_and_comparison",
  "existing_reason_original_vs_augmented_definition4.csv"
)
output_root <- file.path(project_root, "outputs", "07_augmented_clustering")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
primary_hesitancy_var <- "analysis_4_vax_any_hes_or_refused_vs_vaxxed"
seed_value <- 123
candidate_k <- 2:11
n_resamples <- 1000
linkage_method <- "ward.D2"
distance_metric <- "Jaccard"
local_package_dir <- file.path(matt_project_root, "packages")
if (dir.exists(local_package_dir)) {
  .libPaths(unique(c(local_package_dir, .libPaths())))
}
required_packages <- c("sharp", "proxy", "dendextend")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    "\nCurrent .libPaths():\n",
    paste(.libPaths(), collapse = "\n"),
    "\n\nProject-local package directory checked: ",
    local_package_dir
  )
}
suppressPackageStartupMessages({
  library(sharp)
  library(proxy)
  library(dendextend)
})
is_positive_flag <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  !is.na(x) & x_chr %in% c("1", "true", "t", "yes", "y")
}
binary_count <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  sum(!is.na(x_num) & x_num == 1)
}
save_png_and_pdf <- function(figure_dir, filename_stub, width, height, plot_fun) {
  png(
    filename = file.path(figure_dir, paste0(filename_stub, ".png")),
    width = width,
    height = height,
    units = "in",
    res = 300
  )
  plot_fun()
  dev.off()
  grDevices::cairo_pdf(
    file = file.path(figure_dir, paste0(filename_stub, ".pdf")),
    width = width,
    height = height
  )
  plot_fun()
  dev.off()
}
run_clustering_version <- function(
    df_definition4,
    cluster_cols,
    variable_dictionary,
    version_name,
    version_description
) {
  output_dir <- file.path(output_root, version_name)
  figure_dir <- file.path(output_dir, "figures")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  cat("\n============================================================\n")
  cat("RUNNING CLUSTERING VERSION:", version_name, "\n")
  cat(version_description, "\n")
  cat("============================================================\n")
  df_for_clust_raw <- df_definition4[, cluster_cols, drop = FALSE]
  df_for_clust_raw[] <- lapply(df_for_clust_raw, function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  })
  non_binary_values <- unique(unlist(lapply(df_for_clust_raw, function(x) {
    setdiff(unique(x[!is.na(x)]), c(0, 1))
  })))
  if (length(non_binary_values) > 0) {
    stop(
      "Non-binary value(s) found in ", version_name, ": ",
      paste(non_binary_values, collapse = ", ")
    )
  }
  all_missing_row <- rowSums(is.na(df_for_clust_raw)) == ncol(df_for_clust_raw)
  include_index <- !all_missing_row
  df_for_clust <- df_for_clust_raw[include_index, , drop = FALSE]
  na_before_zero_fill <- colSums(is.na(df_for_clust))
  df_for_clust[is.na(df_for_clust)] <- 0
  all_zero_after_cleaning <- rowSums(df_for_clust) == 0
  n_all_zero_after_cleaning <- sum(all_zero_after_cleaning)
  df_for_clust <- df_for_clust[
    !all_zero_after_cleaning,
    ,
    drop=FALSE
  ]
  colnames(df_for_clust) <- variable_dictionary$label
  reason_prevalence <- data.frame(
    variable = variable_dictionary$variable,
    label = variable_dictionary$label,
    source = variable_dictionary$source,
    n_positive_definition4 = vapply(df_for_clust_raw, binary_count, integer(1)),
    n_missing_definition4 = colSums(is.na(df_for_clust_raw)),
    n_missing_after_excluding_all_missing_rows = na_before_zero_fill,
    stringsAsFactors = FALSE
  )
  write.csv(
    variable_dictionary,
    file.path(output_dir, "clustering_variable_dictionary.csv"),
    row.names = FALSE
  )
  write.csv(
    reason_prevalence,
    file.path(output_dir, "clustering_reason_prevalence.csv"),
    row.names = FALSE
  )
  saveRDS(
    df_for_clust,
    file.path(output_dir, "clustering_input_matrix_cleaned.rds")
  )
  qc_summary <- data.frame(
    measure = c(
      "version_name",
      "version_description",
      "definition4_hesitant_cohort",
      "number_clustering_variables",
      "number_augmented_existing_variables",
      "number_novel_variables",
      "original_other_included",
      "uncertainty_included",
      "rows_all_variables_missing_excluded",
      "final_clustering_participant_rows",
      "rows_all_zero_after_variable_selection_excluded",
      "remaining_NA_after_cleaning",
      "distance_metric",
      "linkage_method",
      "candidate_cluster_numbers",
      "consensus_resamples",
      "random_seed"
    ),
    value = c(
      version_name,
      version_description,
      nrow(df_definition4),
      length(cluster_cols),
      sum(grepl("^augmented_existing_", cluster_cols)),
      sum(grepl("^novel_", cluster_cols)),
      "augmented_existing_19" %in% cluster_cols,
      "novel_uncertainty_decision_pending" %in% cluster_cols,
      sum(all_missing_row),
      nrow(df_for_clust),
      n_all_zero_after_cleaning,
      sum(is.na(df_for_clust)),
      distance_metric,
      linkage_method,
      paste(candidate_k, collapse = ","),
      n_resamples,
      seed_value
    ),
    stringsAsFactors = FALSE
  )
  write.csv(qc_summary, file.path(output_dir, "qc_summary.csv"), row.names = FALSE)
  cat("Definition 4 cohort:", nrow(df_definition4), "\n")
  cat("Variables supplied:", ncol(df_for_clust), "\n")
  cat("Rows excluded because all variables were NA:", sum(all_missing_row), "\n")
  cat("Final participant rows supplied:", nrow(df_for_clust), "\n")
  cat("Remaining NA after cleaning:", sum(is.na(df_for_clust)), "\n")
  cat('Rows excluded because all selected variables were zero:', n_all_zero_after_cleaning,"\n")
  dist_jaccard <- proxy::dist(
    df_for_clust,
    by_rows = FALSE,
    method = distance_metric
  )
  if (attr(dist_jaccard, "Size") != ncol(df_for_clust)) {
    stop(version_name, ": distance object size does not match variable count.")
  }
  saveRDS(dist_jaccard, file.path(output_dir, "jaccard_distance_object.rds"))
  set.seed(seed_value)
  sharp_clus <- sharp::Clustering(
    xdata = dist_jaccard,
    nc = candidate_k,
    seed = seed_value,
    K = n_resamples,
    method = linkage_method
  )
  saveRDS(sharp_clus, file.path(output_dir, "consensus_clustering_object.rds"))
  optimal_assignments <- Clusters(sharp_clus)
  optimal_k <- length(unique(optimal_assignments))
  if (!optimal_k %in% candidate_k) {
    stop(version_name, ": sharp selected k outside candidate_k.")
  }
  writeLines(
    c(
      paste0("version=", version_name),
      paste0("optimal_k=", optimal_k),
      paste0("candidate_k=", paste(candidate_k, collapse = ",")),
      paste0("distance_metric=", distance_metric),
      paste0("linkage_method=", linkage_method),
      paste0("consensus_resamples=", n_resamples),
      paste0("seed=", seed_value)
    ),
    con = file.path(output_dir, "optimal_cluster_number.txt")
  )
  calibration_plot_fun <- function() {
    par(mar = c(5, 5, 4, 2))
    CalibrationPlot(
      sharp_clus,
      xlab = "Number of clusters",
      ylab = "Consensus score"
    )
    title(
      main = paste0(
        gsub("_", " ", version_name),
        ": consensus calibration (optimal k = ", optimal_k, ")"
      )
    )
  }
  save_png_and_pdf(
    figure_dir,
    "calibration_plot_jaccard",
    width = 7,
    height = 8,
    plot_fun = calibration_plot_fun
  )
  consensus_matrix <- ConsensusMatrix(sharp_clus)
  rownames(consensus_matrix) <- colnames(df_for_clust)
  colnames(consensus_matrix) <- colnames(df_for_clust)
  write.csv(
    consensus_matrix,
    file.path(output_dir, "consensus_matrix.csv"),
    row.names = TRUE
  )
  distance_matrix <- 1 - consensus_matrix
  dist_stability <- as.dist(distance_matrix)
  set.seed(seed_value)
  hc <- hclust(dist_stability, method = sharp_clus$methods$linkage)
  saveRDS(hc, file.path(output_dir, "hierarchical_clustering_object.rds"))
  cluster_assignment_numeric <- cutree(hc, k = optimal_k)
  cluster_assignment_table <- data.frame(
    label = names(cluster_assignment_numeric),
    cluster = as.integer(cluster_assignment_numeric),
    stringsAsFactors = FALSE
  )
  cluster_assignment_table <- merge(
    variable_dictionary,
    cluster_assignment_table,
    by = "label",
    all.x = TRUE,
    sort = FALSE
  )
  cluster_assignment_table <- cluster_assignment_table[
    order(
      cluster_assignment_table$cluster,
      cluster_assignment_table$source,
      cluster_assignment_table$label
    ),
    ,
    drop = FALSE
  ]
  if (anyNA(cluster_assignment_table$cluster)) {
    stop(version_name, ": some variables received no cluster assignment.")
  }
  write.csv(
    cluster_assignment_table,
    file.path(output_dir, "cluster_assignments_unlabelled.csv"),
    row.names = FALSE
  )
  member_lines <- unlist(lapply(seq_len(optimal_k), function(k) {
    members <- cluster_assignment_table$label[
      cluster_assignment_table$cluster == k
    ]
    c(
      paste0("CLUSTER ", k, " (n reasons/themes = ", length(members), ")"),
      paste0("  - ", members),
      ""
    )
  }))
  writeLines(member_lines, file.path(output_dir, "cluster_members_unlabelled.txt"))
  palette_clusters <- grDevices::hcl.colors(optimal_k, palette = "Dark 3")
  dend <- as.dendrogram(hc)
  dend_coloured <- dendextend::color_branches(
    dend,
    k = optimal_k,
    col = palette_clusters
  )
  dend_coloured <- dendextend::color_labels(
    dend_coloured,
    k = optimal_k,
    col = palette_clusters
  )
  cut_height <- hc$height[length(hc$height) - (optimal_k - 1)]
  dendrogram_plot_fun <- function() {
    par(mar = c(5, 4, 4, 22), xpd = NA)
    plot(
      dend_coloured,
      horiz = TRUE,
      xlab = "Consensus distance",
      ylab = "",
      cex = 0.75,
      main = paste0(
        gsub("_", " ", version_name),
        " — unlabelled clusters (k = ", optimal_k, ")"
      )
    )
    abline(v = cut_height, col = "grey45", lty = 2, lwd = 1)
    legend(
      "topleft",
      legend = paste("Cluster", seq_len(optimal_k)),
      fill = palette_clusters,
      border = NA,
      bty = "n",
      cex = 0.8,
      inset = c(0.02, -0.02)
    )
  }
  save_png_and_pdf(
    figure_dir,
    "dendrogram_jaccard_unlabelled",
    width = 14,
    height = 10,
    plot_fun = dendrogram_plot_fun
  )
  consensus_plot_fun <- function() {
    ordered_labels <- hc$labels[hc$order]
    ordered_matrix <- consensus_matrix[ordered_labels, ordered_labels, drop = FALSE]
    par(mar = c(12, 12, 4, 3))
    image(
      x = seq_len(nrow(ordered_matrix)),
      y = seq_len(ncol(ordered_matrix)),
      z = ordered_matrix,
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = paste0(gsub("_", " ", version_name), " — consensus matrix")
    )
    axis(
      1,
      at = seq_len(nrow(ordered_matrix)),
      labels = rownames(ordered_matrix),
      las = 2,
      cex.axis = 0.45
    )
    axis(
      2,
      at = seq_len(ncol(ordered_matrix)),
      labels = colnames(ordered_matrix),
      las = 2,
      cex.axis = 0.45
    )
    box()
  }
  save_png_and_pdf(
    figure_dir,
    "consensus_matrix_jaccard",
    width = 12,
    height = 12,
    plot_fun = consensus_plot_fun
  )
  grDevices::cairo_pdf(
    file = file.path(figure_dir, "dendrogram_plus_optimisation_jaccard.pdf"),
    width = 14,
    height = 8
  )
  layout(matrix(c(1, 0, 2, 2), nrow = 2, ncol = 2), heights = c(3, 1), widths = c(1, 3))
  par(mar = c(5, 5, 4, 2))
  CalibrationPlot(
    sharp_clus,
    xlab = "Number of clusters",
    ylab = "Consensus score"
  )
  title(main = paste0("Optimal k = ", optimal_k))
  par(mar = c(5, 4, 4, 22), xpd = NA)
  plot(
    dend_coloured,
    horiz = TRUE,
    xlab = "Consensus distance",
    ylab = "",
    cex = 0.75,
    main = gsub("_", " ", version_name)
  )
  abline(v = cut_height, col = "grey60", lty = 2, lwd = 1)
  legend(
    "topleft",
    legend = paste("Cluster", seq_len(optimal_k)),
    fill = palette_clusters,
    border = NA,
    bty = "n",
    cex = 0.8,
    inset = c(0.02, -0.02)
  )
  dev.off()
  cat("Optimal cluster number:", optimal_k, "\n")
  cat("Outputs saved to:", output_dir, "\n")
  invisible(list(
    version_name = version_name,
    optimal_k = optimal_k,
    cluster_assignments = cluster_assignment_table,
    qc_summary = qc_summary,
    sharp_object = sharp_clus,
    hierarchical_object = hc
  ))
}
required_files <- c(input_rds, existing_label_csv)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}
df <- readRDS(input_rds)
if (!is.data.frame(df)) {
  stop("The Step 06 enriched RDS did not contain a data.frame.")
}
if (!primary_hesitancy_var %in% names(df)) {
  stop(
    "Primary hesitancy variable not found: ", primary_hesitancy_var,
    "\nAvailable analysis variables include:\n",
    paste(grep("^analysis_", names(df), value = TRUE), collapse = "\n")
  )
}
existing_cols_all <- paste0("augmented_existing_", 1:23)
existing_cols_without_other <- setdiff(existing_cols_all, "augmented_existing_19")
novel_cols_substantive <- c(
  "novel_brand_preference",
  "novel_access_availability_dosing",
  "novel_ethical_religious_animal",
  "novel_government_distrust_autonomy",
  "novel_mrna_gene_technology"
)
uncertainty_col <- "novel_uncertainty_decision_pending"
all_required_analysis_cols <- c(
  existing_cols_all,
  novel_cols_substantive,
  uncertainty_col
)
missing_analysis_cols <- setdiff(all_required_analysis_cols, names(df))
if (length(missing_analysis_cols) > 0) {
  stop(
    "Required variable(s) missing from Step 06 enriched RDS:\n",
    paste(missing_analysis_cols, collapse = "\n")
  )
}
label_table <- read.csv(
  existing_label_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
required_label_cols <- c("reason_number", "reason_label")
if (!all(required_label_cols %in% names(label_table))) {
  stop(
    "Existing-label CSV must contain columns: ",
    paste(required_label_cols, collapse = ", ")
  )
}
label_table <- label_table[order(label_table$reason_number), , drop = FALSE]
if (!identical(as.integer(label_table$reason_number), 1:23)) {
  stop("Existing-label CSV does not contain exactly reason numbers 1:23.")
}
existing_label_lookup <- setNames(
  label_table$reason_label,
  paste0("augmented_existing_", label_table$reason_number)
)
novel_label_lookup <- c(
  novel_brand_preference = "Vaccine brand preference and choice",
  novel_access_availability_dosing = "Access, availability and dosing arrangements",
  novel_ethical_religious_animal = "Ethical, religious and animal-related concerns",
  novel_government_distrust_autonomy = "Government distrust, coercion and loss of autonomy",
  novel_mrna_gene_technology = "Concerns about mRNA and gene-therapy technology"
)
make_dictionary <- function(cluster_cols) {
  labels <- c(existing_label_lookup, novel_label_lookup)[cluster_cols]
  if (anyNA(labels)) {
    stop("Missing readable label for: ", paste(cluster_cols[is.na(labels)], collapse = ", "))
  }
  data.frame(
    variable = cluster_cols,
    label = unname(labels),
    source = ifelse(
      grepl("^augmented_existing_", cluster_cols),
      "Augmented existing VACCRUFUSE3 reason",
      "Novel free-text theme"
    ),
    stringsAsFactors = FALSE
  )
}
definition4_flag <- is_positive_flag(df[[primary_hesitancy_var]])
df_definition4 <- df[definition4_flag, , drop = FALSE]
if (nrow(df_definition4) == 0) {
  stop("Definition 4 filtering returned zero participants.")
}
if (nrow(df_definition4) != 37982) {
  warning(
    "Definition 4 cohort is ", nrow(df_definition4),
    ", not the previously observed 37,982. Check input data and flag coding."
  )
}
primary_cols <- c(existing_cols_without_other, novel_cols_substantive)
sensitivity_cols <- c(existing_cols_all, novel_cols_substantive)
if (length(primary_cols) != 27) {
  stop("Primary clustering should contain exactly 27 variables.")
}
if (length(sensitivity_cols) != 28) {
  stop("Sensitivity clustering should contain exactly 28 variables.")
}
if ("augmented_existing_19" %in% primary_cols) {
  stop("Primary clustering accidentally includes Other.")
}
if (!"augmented_existing_19" %in% sensitivity_cols) {
  stop("Sensitivity clustering does not include Other.")
}
if (uncertainty_col %in% c(primary_cols, sensitivity_cols)) {
  stop("Uncertainty was accidentally included in clustering.")
}
primary_result <- run_clustering_version(
  df_definition4 = df_definition4,
  cluster_cols = primary_cols,
  variable_dictionary = make_dictionary(primary_cols),
  version_name = "primary_without_other",
  version_description = paste(
    "Primary clustering: 22 augmented existing reasons (Other excluded)",
    "+ 5 substantive novel themes; uncertainty excluded."
  )
)
sensitivity_result <- run_clustering_version(
  df_definition4 = df_definition4,
  cluster_cols = sensitivity_cols,
  variable_dictionary = make_dictionary(sensitivity_cols),
  version_name = "sensitivity_with_other",
  version_description = paste(
    "Sensitivity clustering: all 23 augmented existing reasons (Other retained)",
    "+ 5 substantive novel themes; uncertainty excluded."
  )
)
comparison_summary <- data.frame(
  version = c("Primary: without Other", "Sensitivity: with Other"),
  n_variables = c(length(primary_cols), length(sensitivity_cols)),
  other_included = c(FALSE, TRUE),
  uncertainty_included = c(FALSE, FALSE),
  optimal_k = c(primary_result$optimal_k, sensitivity_result$optimal_k),
  stringsAsFactors = FALSE
)
write.csv(
  comparison_summary,
  file.path(output_root, "primary_vs_sensitivity_summary.csv"),
  row.names = FALSE
)
primary_assign <- primary_result$cluster_assignments[, c("variable", "cluster")]
names(primary_assign)[2] <- "cluster_primary_without_other"
sensitivity_assign <- sensitivity_result$cluster_assignments[, c("variable", "cluster")]
names(sensitivity_assign)[2] <- "cluster_sensitivity_with_other"
shared_assignment_comparison <- merge(
  primary_assign,
  sensitivity_assign,
  by = "variable",
  all = TRUE,
  sort = FALSE
)
shared_assignment_comparison <- merge(
  make_dictionary(sensitivity_cols)[, c("variable", "label", "source")],
  shared_assignment_comparison,
  by = "variable",
  all.y = TRUE,
  sort = FALSE
)
write.csv(
  shared_assignment_comparison,
  file.path(output_root, "primary_vs_sensitivity_cluster_assignments.csv"),
  row.names = FALSE
)
saveRDS(
  list(primary = primary_result, sensitivity = sensitivity_result),
  file.path(output_root, "all_clustering_results.rds")
)
cat("\nSTEP 07 COMPLETED SUCCESSFULLY.\n")
cat("Primary optimal k:", primary_result$optimal_k, "\n")
cat("Sensitivity optimal k:", sensitivity_result$optimal_k, "\n")
cat("Outputs saved to:", output_root, "\n")
cat("Next step: review cluster_members_unlabelled.txt in both folders and assign names manually.\n")
