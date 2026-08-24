# Compare manually grouped additional BERTopic topics with the full response-level BERTopic hierarchy.

from __future__ import annotations
from pathlib import Path
from typing import Any
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from bertopic import BERTopic
from scipy.cluster.hierarchy import dendrogram, linkage
from scipy.spatial.distance import pdist, squareform
from pipeline_config import GROUPING_DIR, OUTPUT_ROOT, RESPONSE_DIR
OUTPUT_DIR = OUTPUT_ROOT / "06b_dendrogram_comparison"
FIGURE_DIR = OUTPUT_DIR / "figures"
MANUAL_MAPPING_PATH = GROUPING_DIR / "response_topic_mapping_review.xlsx"
MODEL_PATH = RESPONSE_DIR / "response_bertopic_model"
HIERARCHY_PATH = RESPONSE_DIR / "response_hierarchical_topics.csv"
DOCUMENT_TOPICS_PATH = RESPONSE_DIR / "response_document_topics.csv"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
FIGURE_DIR.mkdir(parents=True, exist_ok=True)
THEME_ABBREVIATIONS = {
    "Vaccine brand preference and choice": "BRAND",
    "Access, availability and dosing arrangements": "ACCESS",
    "Ethical, religious and animal-related concerns": "ETHICS",
    "Government distrust, coercion and loss of autonomy": "GOV/AUTO",
    "Concerns about mRNA and gene-therapy technology": "MRNA",
    "Uncertainty or decision not yet made": "UNCERTAIN",
}
THEME_COLOURS = {
    "Vaccine brand preference and choice": "tab:blue",
    "Access, availability and dosing arrangements": "tab:orange",
    "Ethical, religious and animal-related concerns": "tab:green",
    "Government distrust, coercion and loss of autonomy": "tab:red",
    "Concerns about mRNA and gene-therapy technology": "tab:purple",
    "Uncertainty or decision not yet made": "tab:brown",
}
def find_column(df: pd.DataFrame, candidates: list[str]) -> str:
    """Find a column while allowing small differences in capitalisation."""
    normalised = {str(col).strip().lower(): col for col in df.columns}
    for candidate in candidates:
        key = candidate.strip().lower()
        if key in normalised:
            return normalised[key]
    raise KeyError(
        f"Could not find any of {candidates}. "
        f"Available columns: {list(df.columns)}"
    )
def clean_existing_flag(series: pd.Series) -> pd.Series:
    """Standardise Yes/No values from the manually edited CSV."""
    cleaned = series.astype(str).str.strip().str.lower()
    return cleaned.replace(
        {
            "y": "yes",
            "yes": "yes",
            "true": "yes",
            "1": "yes",
            "n": "no",
            "no": "no",
            "false": "no",
            "0": "no",
        }
    )
def shorten_label(value: Any, max_length: int = 42) -> str:
    """Keep leaf labels readable in the full 88-topic figure."""
    text = str(value).strip()
    if len(text) <= max_length:
        return text
    return text[: max_length - 3] + "..."
def get_topic_embedding_lookup(model: BERTopic) -> dict[int, np.ndarray]:
    """
    Align BERTopic topic IDs with rows of topic_embeddings_.
    The function includes checks so that it stops rather than silently
    applying the wrong embedding row to a topic.
    """
    if model.topic_embeddings_ is None:
        raise ValueError(
            "The loaded BERTopic model does not contain topic_embeddings_."
        )
    embeddings = np.asarray(model.topic_embeddings_)
    topic_labels = getattr(model, "topic_labels_", None)
    if isinstance(topic_labels, dict) and len(topic_labels) == embeddings.shape[0]:
        ordered_topic_ids = [int(topic_id) for topic_id in topic_labels.keys()]
    else:
        ordered_topic_ids = sorted(int(topic_id) for topic_id in model.get_topics())
        if len(ordered_topic_ids) != embeddings.shape[0]:
            raise ValueError(
                "Could not safely align topic embeddings with topic IDs. "
                f"Embedding rows: {embeddings.shape[0]}; "
                f"model topics: {len(ordered_topic_ids)}."
            )
    return {
        topic_id: embeddings[row_number]
        for row_number, topic_id in enumerate(ordered_topic_ids)
    }
def load_or_rebuild_hierarchy(
    model: BERTopic,
) -> pd.DataFrame:
    """
    Prefer the hierarchy exported when Script 02 originally ran.
    If that CSV is missing, rebuild it from exactly the response-level
    documents used by the fitted model.
    """
    if HIERARCHY_PATH.exists():
        hierarchy = pd.read_csv(HIERARCHY_PATH)
        print(f"Loaded original hierarchy: {HIERARCHY_PATH}")
        return hierarchy
    if not DOCUMENT_TOPICS_PATH.exists():
        raise FileNotFoundError(
            "The original hierarchy CSV is missing and the document-topic "
            f"file needed to rebuild it was not found: {DOCUMENT_TOPICS_PATH}"
        )
    document_topics = pd.read_csv(DOCUMENT_TOPICS_PATH)
    text_col = find_column(
        document_topics,
        ["model_text", "Document", "document", "original_text"],
    )
    docs = (
        document_topics[text_col]
        .fillna("")
        .astype(str)
        .str.strip()
        .tolist()
    )
    if any(doc == "" for doc in docs):
        raise ValueError(
            "Blank document text found while attempting to rebuild hierarchy."
        )
    hierarchy = model.hierarchical_topics(docs)
    hierarchy.to_csv(HIERARCHY_PATH, index=False)
    print(
        "Original hierarchy CSV was missing, so it was rebuilt and saved to: "
        f"{HIERARCHY_PATH}"
    )
    return hierarchy
def make_plotly_full_hierarchy(
    model: BERTopic,
    hierarchy: pd.DataFrame,
    custom_labels: dict[int, str],
) -> None:
    """
    Create the primary interactive hierarchy.
    The tree structure comes from the original BERTopic hierarchy.
    Manual final themes are added only as leaf labels.
    """
    original_custom_labels = getattr(model, "custom_labels_", None)
    try:
        model.set_topic_labels(custom_labels)
        figure = model.visualize_hierarchy(
            hierarchical_topics=hierarchy,
            custom_labels=True,
            title=(
                "BERTopic hierarchy with manually grouped "
                "novel-topic highlighted"
            ),
        )
        figure.write_html(
            str(OUTPUT_DIR / "response_full_dendrogram_manual_overlay.html")
        )
    finally:
        model.custom_labels_ = original_custom_labels
def make_static_full_hierarchy(
    model: BERTopic,
    topic_ids: list[int],
    label_lookup: dict[int, str],
    novel_theme_lookup: dict[int, str],
) -> tuple[list[int], dict[int, int]]:
    """
    Create a readable static companion figure containing ALL non-outlier topics.
    This plot is reconstructed from all topic embeddings. It is useful for a
    dissertation figure and visual checking, but the BERTopic HTML hierarchy
    remains the primary original-tree comparison.
    """
    embedding_lookup = get_topic_embedding_lookup(model)
    missing_topics = [
        topic_id for topic_id in topic_ids if topic_id not in embedding_lookup
    ]
    if missing_topics:
        raise ValueError(
            f"Topics missing from BERTopic topic embeddings: {missing_topics}"
        )
    embedding_matrix = np.vstack(
        [embedding_lookup[topic_id] for topic_id in topic_ids]
    )
    condensed_distances = pdist(embedding_matrix, metric="cosine")
    linkage_matrix = linkage(condensed_distances, method="average")
    figure_height = max(18, 0.28 * len(topic_ids))
    fig, ax = plt.subplots(figsize=(15, figure_height))
    dendrogram_result = dendrogram(
        linkage_matrix,
        labels=[label_lookup[topic_id] for topic_id in topic_ids],
        orientation="right",
        leaf_font_size=7,
        link_color_func=lambda _: '0.75',
        above_threshold_color='0.75',
        ax=ax,
    )
    ax.set_title(
        "BERTopic hierarchy with manually grouped novel topics highlighted"
    )
    ax.set_xlabel("Cosine-distance linkage")
    ax.set_ylabel("BERTopic topic")
    for tick_label in ax.get_yticklabels():
        label_text = tick_label.get_text()
        try:
            topic_id = int(label_text.split(" ", 1)[0].replace("T", ""))
        except (ValueError, IndexError):
            continue
        final_theme = novel_theme_lookup.get(topic_id)
        if final_theme is None:
            tick_label.set_color("0.45")
        else:
            tick_label.set_color(THEME_COLOURS[final_theme])
            tick_label.set_fontweight("bold")
    fig.tight_layout()
    fig.savefig(
        FIGURE_DIR / "response_full_dendrogram_manual_overlay.png",
        dpi=300,
        bbox_inches="tight",
    )
    plt.close(fig)
    leaf_topic_ids = [
        topic_ids[position] for position in dendrogram_result["leaves"]
    ]
    leaf_position_lookup = {
        topic_id: position for position, topic_id in enumerate(leaf_topic_ids)
    }
    return leaf_topic_ids, leaf_position_lookup
def main() -> None:
    for path in [MANUAL_MAPPING_PATH, MODEL_PATH]:
        if not path.exists():
            raise FileNotFoundError(f"Required input not found: {path}")
    mapping = pd.read_excel(MANUAL_MAPPING_PATH, engine="openpyxl")
    topic_col = find_column(mapping, ["topic_id", "Topic", "topic"])
    existing_col = find_column(
        mapping,
        ["Existing?", "Existing", "existing"],
    )
    existing_code_col = find_column(
        mapping,
        ["Existing_code", "existing_code", "Existing code"],
    )
    final_theme_col = find_column(
        mapping,
        ["Final_theme", "final_theme", "Final theme"],
    )
    bertopic_label_col = find_column(
        mapping,
        ["bertopic_label", "Name", "topic_label"],
    )
    mapping[topic_col] = pd.to_numeric(
        mapping[topic_col],
        errors="raise",
    ).astype(int)
    mapping["existing_clean"] = clean_existing_flag(mapping[existing_col])
    mapping[existing_code_col] = (
        mapping[existing_code_col]
        .fillna("")
        .astype(str)
        .str.strip()
    )
    mapping[final_theme_col] = (
        mapping[final_theme_col]
        .fillna("")
        .astype(str)
        .str.strip()
    )
    if mapping[topic_col].duplicated().any():
        duplicated = mapping.loc[
            mapping[topic_col].duplicated(keep=False),
            topic_col,
        ].tolist()
        raise ValueError(
            f"Duplicate topic IDs in manual mapping: {duplicated}"
        )
    mapping = mapping.loc[mapping[topic_col] >= 0].copy()
    invalid_existing_flags = sorted(
        set(mapping["existing_clean"]) - {"yes", "no"}
    )
    if invalid_existing_flags:
        raise ValueError(
            "Existing? contains unrecognised values: "
            f"{invalid_existing_flags}"
        )
    novel = mapping.loc[mapping["existing_clean"] == "no"].copy()
    existing = mapping.loc[mapping["existing_clean"] == "yes"].copy()
    blank_novel_themes = novel.loc[
        novel[final_theme_col].eq(""),
        topic_col,
    ].tolist()
    if blank_novel_themes:
        raise ValueError(
            "Novel topics missing Final_theme: "
            f"{blank_novel_themes}"
        )
    observed_themes = set(novel[final_theme_col])
    expected_themes = set(THEME_ABBREVIATIONS)
    unexpected_themes = observed_themes - expected_themes
    missing_themes = expected_themes - observed_themes
    if unexpected_themes:
        raise ValueError(
            "Unexpected Final_theme labels were found. Check spelling: "
            f"{sorted(unexpected_themes)}"
        )
    if missing_themes:
        raise ValueError(
            "Expected final themes are missing from the mapping: "
            f"{sorted(missing_themes)}"
        )
    if len(mapping) != 88:
        print(
            f"Warning: expected 88 non-outlier topics in mapping, found {len(mapping)}."
        )
    if len(existing) != 69:
        print(
            f"Warning: expected 69 existing topics, found {len(existing)}."
        )
    if len(novel) != 19:
        print(
            f"Warning: expected 19 novel topics, found {len(novel)}."
        )
    if novel[final_theme_col].nunique() != 6:
        print(
            "Warning: expected 6 final broader themes, found "
            f"{novel[final_theme_col].nunique()}."
        )
    model = BERTopic.load(str(MODEL_PATH))
    hierarchy = load_or_rebuild_hierarchy(model)
    model_topic_ids = sorted(
        int(topic_id)
        for topic_id in model.get_topics()
        if int(topic_id) >= 0
    )
    mapped_topic_ids = sorted(mapping[topic_col].tolist())
    missing_from_mapping = sorted(
        set(model_topic_ids) - set(mapped_topic_ids)
    )
    extra_in_mapping = sorted(
        set(mapped_topic_ids) - set(model_topic_ids)
    )
    if missing_from_mapping or extra_in_mapping:
        raise ValueError(
            "Manual mapping topic IDs do not match fitted-model topics. "
            f"Missing from mapping: {missing_from_mapping}; "
            f"extra in mapping: {extra_in_mapping}"
        )
    novel_theme_lookup = dict(
        zip(
            novel[topic_col].astype(int),
            novel[final_theme_col],
        )
    )
    mapping_label_lookup = dict(
        zip(
            mapping[topic_col].astype(int),
            mapping[bertopic_label_col].map(shorten_label),
        )
    )
    existing_code_lookup = dict(
        zip(
            existing[topic_col].astype(int),
            existing[existing_code_col],
        )
    )
    interactive_label_lookup: dict[int, str] = {}
    static_label_lookup: dict[int, str] = {}
    for topic_id in model_topic_ids:
        topic_label = mapping_label_lookup[topic_id]
        if topic_id in novel_theme_lookup:
            theme = novel_theme_lookup[topic_id]
            abbreviation = THEME_ABBREVIATIONS[theme]
            interactive_label_lookup[topic_id] = (
                f"[{abbreviation}] T{topic_id} | {topic_label}"
            )
            static_label_lookup[topic_id] = (
                f"T{topic_id} [{abbreviation}] {topic_label}"
            )
        else:
            existing_code = existing_code_lookup.get(topic_id, "")
            interactive_label_lookup[topic_id] = (
                f"[EXISTING {existing_code}] T{topic_id} | {topic_label}"
            )
            static_label_lookup[topic_id] = (
                f"T{topic_id} [EXISTING] {topic_label}"
            )
    make_plotly_full_hierarchy(
        model=model,
        hierarchy=hierarchy,
        custom_labels=interactive_label_lookup,
    )
    leaf_topic_ids, leaf_position_lookup = make_static_full_hierarchy(
        model=model,
        topic_ids=model_topic_ids,
        label_lookup=static_label_lookup,
        novel_theme_lookup=novel_theme_lookup,
    )
    legend = (
        novel[
            [
                topic_col,
                bertopic_label_col,
                final_theme_col,
            ]
        ]
        .copy()
        .rename(
            columns={
                topic_col: "topic_id",
                bertopic_label_col: "bertopic_label",
                final_theme_col: "final_theme",
            }
        )
    )
    legend["theme_abbreviation"] = legend["final_theme"].map(
        THEME_ABBREVIATIONS
    )
    legend["static_leaf_position"] = legend["topic_id"].map(
        leaf_position_lookup
    )
    legend = legend.sort_values(
        ["final_theme", "static_leaf_position", "topic_id"]
    )
    legend.to_csv(
        OUTPUT_DIR / "novel_topic_manual_theme_legend.csv",
        index=False,
    )
    grouping_summary = (
        legend.groupby("final_theme", as_index=False)
        .agg(
            theme_abbreviation=("theme_abbreviation", "first"),
            included_topic_ids=(
                "topic_id",
                lambda values: ", ".join(
                    map(str, sorted(values.astype(int)))
                ),
            ),
            number_topics=("topic_id", "size"),
            minimum_leaf_position=("static_leaf_position", "min"),
            maximum_leaf_position=("static_leaf_position", "max"),
        )
    )
    grouping_summary["leaf_position_span"] = (
        grouping_summary["maximum_leaf_position"]
        - grouping_summary["minimum_leaf_position"]
    )
    embedding_lookup = get_topic_embedding_lookup(model)
    novel_topic_ids = sorted(novel[topic_col].astype(int).tolist())
    novel_embedding_matrix = np.vstack(
        [embedding_lookup[topic_id] for topic_id in novel_topic_ids]
    )
    novel_distance_matrix = squareform(
        pdist(novel_embedding_matrix, metric="cosine")
    )
    pairwise_rows: list[dict[str, Any]] = []
    for row_i, topic_i in enumerate(novel_topic_ids):
        for row_j in range(row_i + 1, len(novel_topic_ids)):
            topic_j = novel_topic_ids[row_j]
            theme_i = novel_theme_lookup[topic_i]
            theme_j = novel_theme_lookup[topic_j]
            pairwise_rows.append(
                {
                    "topic_id_1": topic_i,
                    "topic_id_2": topic_j,
                    "final_theme_1": theme_i,
                    "final_theme_2": theme_j,
                    "same_manual_theme": theme_i == theme_j,
                    "cosine_distance": float(
                        novel_distance_matrix[row_i, row_j]
                    ),
                    "absolute_static_leaf_gap": abs(
                        leaf_position_lookup[topic_i]
                        - leaf_position_lookup[topic_j]
                    ),
                }
            )
    pairwise = pd.DataFrame(pairwise_rows)
    pairwise.to_csv(
        OUTPUT_DIR / "pairwise_novel_topic_distances.csv",
        index=False,
    )
    same_theme_pairs = pairwise.loc[pairwise["same_manual_theme"]].copy()
    within_theme_distance = (
        same_theme_pairs.groupby("final_theme_1", as_index=False)
        .agg(
            number_within_theme_pairs=("cosine_distance", "size"),
            mean_within_theme_cosine_distance=(
                "cosine_distance",
                "mean",
            ),
            median_within_theme_cosine_distance=(
                "cosine_distance",
                "median",
            ),
            maximum_within_theme_cosine_distance=(
                "cosine_distance",
                "max",
            ),
            mean_within_theme_leaf_gap=(
                "absolute_static_leaf_gap",
                "mean",
            ),
        )
        .rename(columns={"final_theme_1": "final_theme"})
    )
    comparison = grouping_summary.merge(
        within_theme_distance,
        on="final_theme",
        how="left",
    )
    comparison["number_within_theme_pairs"] = (
        comparison["number_within_theme_pairs"]
        .fillna(0)
        .astype(int)
    )
    comparison.to_csv(
        OUTPUT_DIR / "manual_theme_dendrogram_comparison.csv",
        index=False,
    )
    overall_pairwise_summary = (
        pairwise.groupby("same_manual_theme", as_index=False)
        .agg(
            number_topic_pairs=("cosine_distance", "size"),
            mean_cosine_distance=("cosine_distance", "mean"),
            median_cosine_distance=("cosine_distance", "median"),
            mean_static_leaf_gap=("absolute_static_leaf_gap", "mean"),
            median_static_leaf_gap=(
                "absolute_static_leaf_gap",
                "median",
            ),
        )
    )
    overall_pairwise_summary.to_csv(
        OUTPUT_DIR / "overall_manual_vs_hierarchy_summary.csv",
        index=False,
    )
    leaf_order = pd.DataFrame(
        {
            "static_leaf_position": range(len(leaf_topic_ids)),
            "topic_id": leaf_topic_ids,
            "is_novel": [
                topic_id in novel_theme_lookup
                for topic_id in leaf_topic_ids
            ],
            "final_theme": [
                novel_theme_lookup.get(topic_id, "")
                for topic_id in leaf_topic_ids
            ],
            "display_label": [
                static_label_lookup[topic_id]
                for topic_id in leaf_topic_ids
            ],
        }
    )
    leaf_order.to_csv(
        OUTPUT_DIR / "full_dendrogram_leaf_order.csv",
        index=False,
    )
    run_summary = pd.DataFrame(
        [
            {
                "number_model_topics_excluding_outlier": len(model_topic_ids),
                "number_existing_topics": len(existing),
                "number_novel_topics": len(novel),
                "number_final_broader_themes": (
                    novel[final_theme_col].nunique()
                ),
                "primary_hierarchy_source": (
                    "Original BERTopic response_hierarchical_topics.csv"
                ),
                "primary_figure": (
                    "response_full_dendrogram_manual_overlay.html"
                ),
                "static_figure_basis": (
                    "All non-outlier BERTopic topic embeddings; "
                    "cosine distance; average linkage"
                ),
                "interpretation": (
                    "Supporting descriptive comparison only. "
                    "Manual grouping remains final because it incorporated "
                    "keywords and representative responses."
                ),
            }
        ]
    )
    run_summary.to_csv(
        OUTPUT_DIR / "run_summary.csv",
        index=False,
    )
    print("\n06b dendrogram comparison completed successfully.")
    print(f"Model topics excluding outlier: {len(model_topic_ids)}")
    print(f"Existing topics: {len(existing)}")
    print(f"Novel topics: {len(novel)}")
    print(
        "Final broader themes: "
        f"{novel[final_theme_col].nunique()}"
    )
    print(f"Outputs saved to: {OUTPUT_DIR}")
    print(
        "Primary figure: "
        f"{OUTPUT_DIR / 'response_full_dendrogram_manual_overlay.html'}"
    )
    print(
        "Static figure: "
        f"{FIGURE_DIR / 'response_full_dendrogram_manual_overlay.png'}"
    )
if __name__ == "__main__":
    main()
