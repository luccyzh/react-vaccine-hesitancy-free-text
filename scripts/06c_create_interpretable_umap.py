# Create descriptive 2D UMAP visualisations for the final response-level BERTopic analysis.

from __future__ import annotations
import os
from pathlib import Path
from typing import Any
os.environ["HF_HUB_OFFLINE"] = "1"
os.environ["TRANSFORMERS_OFFLINE"] = "1"
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import plotly.express as px
from matplotlib.lines import Line2D
from sentence_transformers import SentenceTransformer
from umap import UMAP
from pipeline_config import (
    DATA_INTERIM_DIR,
    EMBEDDING_BATCH_SIZE,
    EMBEDDING_MODEL_PATH,
    OUTPUT_ROOT,
    RANDOM_STATE,
    RESPONSE_DIR,
    GROUPING_DIR,
)
OUTPUT_DIR = OUTPUT_ROOT / "06c_interpretable_umap"
FIGURE_DIR = OUTPUT_DIR / "figures"
INTERACTIVE_DIR = OUTPUT_DIR / "interactive"
INPUT_PATH = DATA_INTERIM_DIR / "response_level_input.csv"
DOCUMENT_TOPICS_PATH = RESPONSE_DIR / "response_document_topics.csv"
MANUAL_MAPPING_PATH = GROUPING_DIR / "response_topic_mapping_review.xlsx"
EMBEDDINGS_PATH = OUTPUT_DIR / "response_embeddings.npy"
COORDINATES_PATH = OUTPUT_DIR / "response_umap_coordinates.csv"
for folder in [OUTPUT_DIR, FIGURE_DIR, INTERACTIVE_DIR]:
    folder.mkdir(parents=True, exist_ok=True)
UMAP_N_NEIGHBORS = 15
UMAP_MIN_DIST = 0.10
UMAP_METRIC = "cosine"
POINT_SIZE = 7
POINT_ALPHA = 0.58
OUTLIER_ALPHA = 0.16
MIN_GROUP_SIZE_FOR_LABEL = 20
MIN_TOPIC_SIZE_FOR_CENTROID_LABEL = 20
EXISTING_OPTION_LABELS = {
    1: "General side-effect concerns",
    2: "Effectiveness / more evidence",
    3: "Long-term health effects",
    4: "Travel to vaccination centre",
    5: "Difficulty reaching vaccination centre",
    6: "Low perceived personal COVID-19 risk",
    7: "Existing health condition",
    8: "Against vaccines in general",
    9: "Doubts vaccine will work personally",
    10: "Concern vaccine may cause COVID-19",
    11: "Fear of pain or needles",
    12: "Concern about feeling ill",
    13: "Prior COVID-19 / vaccination not needed",
    14: "Pregnancy or breastfeeding",
    15: "COVID-19 impact perceived as exaggerated",
    16: "Distrust in vaccine developers / development",
    17: "Others vaccinated, so own vaccination unnecessary",
    18: "Others need limited doses more",
    19: "Other",
    20: "Prefer not to say",
    21: "Fertility / trying to conceive",
    22: "Allergic reaction concerns",
    23: "Previous bad vaccine reaction",
}
EXISTING_CODE_TO_VISUAL_FAMILY = {
    1: "Side effects and previous reactions",
    2: "Effectiveness, evidence and testing",
    3: "Long-term safety",
    4: "Access and vaccination logistics",
    5: "Access and vaccination logistics",
    6: "Low perceived risk / natural immunity",
    7: "Existing health conditions",
    8: "General vaccine opposition",
    9: "Effectiveness, evidence and testing",
    10: "Side effects and previous reactions",
    11: "Needle fear",
    12: "Side effects and previous reactions",
    13: "Low perceived risk / natural immunity",
    14: "Pregnancy and fertility",
    15: "COVID-19 severity perceived as exaggerated",
    16: "Development and institutional distrust",
    17: "Allocation and priority for others",
    18: "Allocation and priority for others",
    19: "Other / unclear",
    20: "Other / unclear",
    21: "Pregnancy and fertility",
    22: "Allergy concerns",
    23: "Side effects and previous reactions",
}
EXPECTED_FINAL_NOVEL_THEMES = {
    "Vaccine brand preference and choice",
    "Access, availability and dosing arrangements",
    "Ethical, religious and animal-related concerns",
    "Government distrust, coercion and loss of autonomy",
    "Concerns about mRNA and gene-therapy technology",
    "Uncertainty or decision not yet made",
}
NOVEL_THEME_SHORT_LABELS = {
    "Vaccine brand preference and choice": "Brand preference",
    "Access, availability and dosing arrangements": "Access / dosing",
    "Ethical, religious and animal-related concerns": "Ethical / religious",
    "Government distrust, coercion and loss of autonomy": "Government distrust / autonomy",
    "Concerns about mRNA and gene-therapy technology": "mRNA / gene technology",
    "Uncertainty or decision not yet made": "Uncertainty / decision pending",
}
def find_column(df: pd.DataFrame, candidates: list[str]) -> str:
    """Find a column while allowing differences in capitalisation."""
    lookup = {str(col).strip().lower(): col for col in df.columns}
    for candidate in candidates:
        key = candidate.strip().lower()
        if key in lookup:
            return lookup[key]
    raise KeyError(
        f"Could not find any of {candidates}. "
        f"Available columns: {list(df.columns)}"
    )
def clean_existing_flag(series: pd.Series) -> pd.Series:
    """Standardise manually entered Yes/No values."""
    return (
        series.fillna("")
        .astype(str)
        .str.strip()
        .str.lower()
        .replace(
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
    )
def extract_existing_option_number(value: Any) -> int | None:
    """Convert VACCRUFUSE3_21 to integer 21."""
    text = str(value).strip().upper()
    if not text or text == "NAN":
        return None
    prefix = "VACCRUFUSE3_"
    if not text.startswith(prefix):
        raise ValueError(
            f"Existing_code has unexpected format: {value!r}"
        )
    number_text = text.replace(prefix, "", 1)
    if not number_text.isdigit():
        raise ValueError(
            f"Existing_code has unexpected format: {value!r}"
        )
    number = int(number_text)
    if number not in EXISTING_OPTION_LABELS:
        raise ValueError(
            f"Existing option number outside 1-23: {number}"
        )
    return number
def weighted_centroid(
    data: pd.DataFrame,
    x_col: str = "umap_1",
    y_col: str = "umap_2",
) -> tuple[float, float]:
    """Return the median coordinate, which is robust to long UMAP tails."""
    return (
        float(data[x_col].median()),
        float(data[y_col].median()),
    )
def add_group_labels(
    ax: plt.Axes,
    data: pd.DataFrame,
    group_col: str,
    short_label_lookup: dict[str, str] | None = None,
    minimum_n: int = MIN_GROUP_SIZE_FOR_LABEL,
) -> None:
    """Annotate group medians with readable labels."""
    counts = data[group_col].value_counts()
    for group_name, group_n in counts.items():
        if group_n < minimum_n:
            continue
        group_data = data.loc[data[group_col] == group_name]
        x, y = weighted_centroid(group_data)
        display_label = (
            short_label_lookup.get(group_name, group_name)
            if short_label_lookup
            else group_name
        )
        ax.text(
            x,
            y,
            f"{display_label}\n(n={group_n:,})",
            fontsize=8,
            ha="center",
            va="center",
            fontweight="bold",
            bbox={
                "boxstyle": "round,pad=0.25",
                "facecolor": "white",
                "edgecolor": "0.35",
                "alpha": 0.82,
            },
            zorder=20,
        )
def save_interactive_scatter(
    data: pd.DataFrame,
    colour_col: str,
    hover_columns: list[str],
    output_path: Path,
    title: str,
) -> None:
    """Save an interactive HTML version for enclave inspection."""
    figure = px.scatter(
        data,
        x="umap_1",
        y="umap_2",
        color=colour_col,
        hover_data=hover_columns,
        opacity=0.58,
        title=title,
        render_mode="webgl",
    )
    figure.update_traces(marker={"size": 5})
    figure.update_layout(
        legend_title_text=colour_col.replace("_", " ").title(),
        template="plotly_white",
    )
    figure.write_html(str(output_path))
def main() -> None:
    for path in [
        INPUT_PATH,
        DOCUMENT_TOPICS_PATH,
        MANUAL_MAPPING_PATH,
        EMBEDDING_MODEL_PATH,
    ]:
        if not path.exists():
            raise FileNotFoundError(f"Required input not found: {path}")
    response_input = pd.read_csv(INPUT_PATH,keep_default_na=False)
    document_topics = pd.read_csv(DOCUMENT_TOPICS_PATH)
    manual_mapping = pd.read_excel(MANUAL_MAPPING_PATH, engine="openpyxl")
    required_input_columns = {
        "participant_id",
        "response_id",
        "question",
        "model_text",
    }
    missing_input_columns = required_input_columns - set(response_input.columns)
    if missing_input_columns:
        raise ValueError(
            "response_level_input.csv is missing columns: "
            f"{sorted(missing_input_columns)}"
        )
    topic_col_document = find_column(
        document_topics,
        ["Topic", "topic", "topic_id"],
    )
    topic_col_mapping = find_column(
        manual_mapping,
        ["topic_id", "Topic", "topic"],
    )
    existing_col = find_column(
        manual_mapping,
        ["Existing?", "Existing", "existing"],
    )
    existing_code_col = find_column(
        manual_mapping,
        ["Existing_code", "existing_code", "Existing code"],
    )
    final_theme_col = find_column(
        manual_mapping,
        ["Final_theme", "final_theme", "Final theme"],
    )
    topic_size_col = find_column(
        manual_mapping,
        ["topic_size", "Count", "count"],
    )
    bertopic_label_col = find_column(
        manual_mapping,
        ["bertopic_label", "Name", "topic_label"],
    )
    if response_input["response_id"].duplicated().any():
        raise ValueError("response_id is not unique in response_level_input.csv")
    if len(response_input) != len(document_topics):
        raise ValueError(
            "response_level_input.csv and response_document_topics.csv "
            f"have different row counts: {len(response_input)} vs "
            f"{len(document_topics)}"
        )
    if "response_id" in document_topics.columns:
        order_matches = (
            response_input["response_id"].astype(str).reset_index(drop=True)
            == document_topics["response_id"].astype(str).reset_index(drop=True)
        ).all()
        if not order_matches:
            raise ValueError(
                "response_id order differs between input and document-topic "
                "files. Stop rather than attach topics to the wrong responses."
            )
    manual_mapping[topic_col_mapping] = pd.to_numeric(
        manual_mapping[topic_col_mapping],
        errors="raise",
    ).astype(int)
    manual_mapping["existing_clean"] = clean_existing_flag(
        manual_mapping[existing_col]
    )
    manual_mapping[existing_code_col] = (
        manual_mapping[existing_code_col]
        .fillna("")
        .astype(str)
        .str.strip()
    )
    manual_mapping[final_theme_col] = (
        manual_mapping[final_theme_col]
        .fillna("")
        .astype(str)
        .str.strip()
    )
    if manual_mapping[topic_col_mapping].duplicated().any():
        duplicate_topics = manual_mapping.loc[
            manual_mapping[topic_col_mapping].duplicated(keep=False),
            topic_col_mapping,
        ].tolist()
        raise ValueError(
            f"Duplicate topic IDs in manual mapping: {duplicate_topics}"
        )
    invalid_flags = sorted(
        set(manual_mapping["existing_clean"]) - {"yes", "no"}
    )
    if invalid_flags:
        raise ValueError(
            f"Unrecognised Existing? values: {invalid_flags}"
        )
    novel_rows = manual_mapping.loc[
        manual_mapping["existing_clean"] == "no"
    ].copy()
    observed_novel_themes = set(novel_rows[final_theme_col])
    if observed_novel_themes != EXPECTED_FINAL_NOVEL_THEMES:
        raise ValueError(
            "Final novel themes do not match the expected six labels.\n"
            f"Observed: {sorted(observed_novel_themes)}\n"
            f"Expected: {sorted(EXPECTED_FINAL_NOVEL_THEMES)}"
        )
    topic_lookup = manual_mapping.set_index(topic_col_mapping)
    plotting_data = response_input[
        [
            "participant_id",
            "response_id",
            "question",
            "model_text",
        ]
    ].copy()
    plotting_data["topic_id"] = pd.to_numeric(
        document_topics[topic_col_document],
        errors="raise",
    ).astype(int)
    visual_family_values: list[str] = []
    detailed_theme_values: list[str] = []
    novel_theme_values: list[str] = []
    existing_code_values: list[str] = []
    existing_label_values: list[str] = []
    bertopic_label_values: list[str] = []
    for topic_id in plotting_data["topic_id"]:
        if topic_id == -1:
            visual_family_values.append("BERTopic outlier")
            detailed_theme_values.append("BERTopic outlier")
            novel_theme_values.append("")
            existing_code_values.append("")
            existing_label_values.append("")
            bertopic_label_values.append("Outlier")
            continue
        if topic_id not in topic_lookup.index:
            raise ValueError(
                f"Topic {topic_id} is missing from manual mapping."
            )
        mapping_row = topic_lookup.loc[topic_id]
        existing_flag = mapping_row["existing_clean"]
        bertopic_label = str(mapping_row[bertopic_label_col]).strip()
        if existing_flag == "yes":
            existing_code = str(
                mapping_row[existing_code_col]
            ).strip()
            option_number = extract_existing_option_number(existing_code)
            if option_number is None:
                raise ValueError(
                    f"Existing topic {topic_id} has no Existing_code."
                )
            existing_label = EXISTING_OPTION_LABELS[option_number]
            visual_family = EXISTING_CODE_TO_VISUAL_FAMILY[option_number]
            visual_family_values.append(visual_family)
            detailed_theme_values.append(existing_label)
            novel_theme_values.append("")
            existing_code_values.append(existing_code)
            existing_label_values.append(existing_label)
            bertopic_label_values.append(bertopic_label)
        else:
            final_theme = str(mapping_row[final_theme_col]).strip()
            visual_family_values.append(
                NOVEL_THEME_SHORT_LABELS[final_theme]
            )
            detailed_theme_values.append(final_theme)
            novel_theme_values.append(final_theme)
            existing_code_values.append("")
            existing_label_values.append("")
            bertopic_label_values.append(bertopic_label)
    plotting_data["visual_family"] = visual_family_values
    plotting_data["detailed_manual_theme"] = detailed_theme_values
    plotting_data["final_novel_theme"] = novel_theme_values
    plotting_data["existing_code"] = existing_code_values
    plotting_data["existing_option_label"] = existing_label_values
    plotting_data["bertopic_label"] = bertopic_label_values
    plotting_data["is_outlier"] = plotting_data["topic_id"].eq(-1)
    plotting_data["is_novel"] = plotting_data[
        "final_novel_theme"
    ].ne("")
    docs = (
        response_input["model_text"]
        .fillna("")
        .astype(str)
        .str.strip()
        .tolist()
    )
    if any(doc == "" for doc in docs):
        raise ValueError("Blank model_text found in response-level input.")
    if EMBEDDINGS_PATH.exists():
        embeddings = np.load(EMBEDDINGS_PATH)
        if embeddings.shape[0] != len(plotting_data):
            raise ValueError(
                "Saved embeddings row count does not match current input. "
                "Delete response_embeddings.npy and rerun."
            )
        print(f"Loaded saved embeddings: {EMBEDDINGS_PATH}")
    else:
        print(f"Loading embedding model: {EMBEDDING_MODEL_PATH}")
        embedding_model = SentenceTransformer(
            str(EMBEDDING_MODEL_PATH)
        )
        embeddings = embedding_model.encode(
            docs,
            batch_size=EMBEDDING_BATCH_SIZE,
            show_progress_bar=True,
            convert_to_numpy=True,
            normalize_embeddings=False,
        )
        np.save(EMBEDDINGS_PATH, embeddings)
        print(f"Saved embeddings: {EMBEDDINGS_PATH}")
    if COORDINATES_PATH.exists():
        saved_coordinates = pd.read_csv(COORDINATES_PATH)
        if len(saved_coordinates) != len(plotting_data):
            raise ValueError(
                "Saved UMAP coordinates do not match current input. "
                "Delete response_umap_coordinates.csv and rerun."
            )
        plotting_data["umap_1"] = saved_coordinates["umap_1"].to_numpy()
        plotting_data["umap_2"] = saved_coordinates["umap_2"].to_numpy()
        print(f"Loaded saved 2D coordinates: {COORDINATES_PATH}")
    else:
        visual_umap = UMAP(
            n_neighbors=UMAP_N_NEIGHBORS,
            n_components=2,
            min_dist=UMAP_MIN_DIST,
            metric=UMAP_METRIC,
            random_state=RANDOM_STATE,
        )
        coordinates = visual_umap.fit_transform(embeddings)
        plotting_data["umap_1"] = coordinates[:, 0]
        plotting_data["umap_2"] = coordinates[:, 1]
        plotting_data[
            ["response_id", "umap_1", "umap_2"]
        ].to_csv(COORDINATES_PATH, index=False)
        print(f"Saved 2D coordinates: {COORDINATES_PATH}")
    plotting_data.drop(columns=["model_text"]).to_csv(
        OUTPUT_DIR / "response_umap_plotting_data.csv",
        index=False,
    )
    figure_data = plotting_data.copy()
    ordered_families = (
        figure_data.loc[
            ~figure_data["is_outlier"],
            "visual_family",
        ]
        .value_counts()
        .index
        .tolist()
    )
    fig, ax = plt.subplots(figsize=(15, 10))
    outliers = figure_data.loc[figure_data["is_outlier"]]
    ax.scatter(
        outliers["umap_1"],
        outliers["umap_2"],
        s=POINT_SIZE,
        alpha=OUTLIER_ALPHA,
        c="0.75",
        edgecolors="none",
        label=f"BERTopic outlier (n={len(outliers):,})",
        rasterized=True,
    )
    colour_map = plt.get_cmap("tab20")
    family_colour_lookup: dict[str, Any] = {}
    for index, family in enumerate(ordered_families):
        family_data = figure_data.loc[
            figure_data["visual_family"] == family
        ]
        colour = colour_map(index % 20)
        family_colour_lookup[family] = colour
        ax.scatter(
            family_data["umap_1"],
            family_data["umap_2"],
            s=POINT_SIZE,
            alpha=POINT_ALPHA,
            color=colour,
            edgecolors="none",
            label=f"{family} (n={len(family_data):,})",
            rasterized=True,
        )
    add_group_labels(
        ax=ax,
        data=figure_data.loc[~figure_data["is_outlier"]],
        group_col="visual_family",
        minimum_n=MIN_GROUP_SIZE_FOR_LABEL,
    )
    ax.set_title(
        "Response-level embedding space with interpretable thematic labels"
    )
    ax.set_xlabel("UMAP 1")
    ax.set_ylabel("UMAP 2")
    ax.legend(
        loc="center left",
        bbox_to_anchor=(1.01, 0.5),
        fontsize=8,
        frameon=False,
    )
    fig.tight_layout()
    fig.savefig(
        FIGURE_DIR / "response_umap_interpretable_families.png",
        dpi=300,
        bbox_inches="tight",
    )
    plt.close(fig)
    fig, ax = plt.subplots(figsize=(14, 9))
    background = figure_data.loc[~figure_data["is_novel"]]
    ax.scatter(
        background["umap_1"],
        background["umap_2"],
        s=5,
        alpha=0.10,
        c="0.72",
        edgecolors="none",
        rasterized=True,
        label="Existing-topic responses and outliers",
    )
    novel_theme_order = (
        figure_data.loc[
            figure_data["is_novel"],
            "final_novel_theme",
        ]
        .value_counts()
        .index
        .tolist()
    )
    novel_colour_map = plt.get_cmap("tab10")
    for index, final_theme in enumerate(novel_theme_order):
        theme_data = figure_data.loc[
            figure_data["final_novel_theme"] == final_theme
        ]
        ax.scatter(
            theme_data["umap_1"],
            theme_data["umap_2"],
            s=20,
            alpha=0.78,
            color=novel_colour_map(index),
            edgecolors="white",
            linewidths=0.25,
            label=(
                f"{NOVEL_THEME_SHORT_LABELS[final_theme]} "
                f"(n={len(theme_data):,})"
            ),
            rasterized=True,
        )
    add_group_labels(
        ax=ax,
        data=figure_data.loc[figure_data["is_novel"]],
        group_col="final_novel_theme",
        short_label_lookup=NOVEL_THEME_SHORT_LABELS,
        minimum_n=1,
    )
    ax.set_title(
        "Final novel free-text themes in the response embedding space"
    )
    ax.set_xlabel("UMAP 1")
    ax.set_ylabel("UMAP 2")
    ax.legend(
        loc="center left",
        bbox_to_anchor=(1.01, 0.5),
        fontsize=9,
        frameon=False,
    )
    fig.tight_layout()
    fig.savefig(
        FIGURE_DIR / "response_umap_final_novel_themes.png",
        dpi=300,
        bbox_inches="tight",
    )
    plt.close(fig)
    non_outlier_data = figure_data.loc[
        ~figure_data["is_outlier"]
    ].copy()
    topic_centroids = (
        non_outlier_data.groupby("topic_id", as_index=False)
        .agg(
            umap_1=("umap_1", "median"),
            umap_2=("umap_2", "median"),
            topic_size=("response_id", "size"),
            visual_family=("visual_family", "first"),
            detailed_manual_theme=("detailed_manual_theme", "first"),
            final_novel_theme=("final_novel_theme", "first"),
            existing_code=("existing_code", "first"),
            bertopic_label=("bertopic_label", "first"),
        )
    )
    topic_centroids["display_label"] = np.where(
        topic_centroids["final_novel_theme"].ne(""),
        topic_centroids["final_novel_theme"].map(
            NOVEL_THEME_SHORT_LABELS
        ),
        topic_centroids["detailed_manual_theme"],
    )
    fig, ax = plt.subplots(figsize=(16, 11))
    centroid_family_order = (
        topic_centroids["visual_family"]
        .value_counts()
        .index
        .tolist()
    )
    centroid_colour_lookup: dict[str, Any] = {}
    for index, family in enumerate(centroid_family_order):
        family_topics = topic_centroids.loc[
            topic_centroids["visual_family"] == family
        ]
        colour = colour_map(index % 20)
        centroid_colour_lookup[family] = colour
        ax.scatter(
            family_topics["umap_1"],
            family_topics["umap_2"],
            s=35 + family_topics["topic_size"] * 1.6,
            alpha=0.72,
            color=colour,
            edgecolors="white",
            linewidths=0.6,
            label=family,
        )
    for _, row in topic_centroids.iterrows():
        ax.text(
            row["umap_1"],
            row["umap_2"],
            str(int(row["topic_id"])),
            fontsize=7,
            ha="center",
            va="center",
            color="black",
            fontweight="bold",
        )
        should_draw_theme = (
            row["topic_size"] >= MIN_TOPIC_SIZE_FOR_CENTROID_LABEL
            or row["final_novel_theme"] != ""
        )
        if should_draw_theme:
            ax.annotate(
                row["display_label"],
                xy=(row["umap_1"], row["umap_2"]),
                xytext=(5, 5),
                textcoords="offset points",
                fontsize=7,
                alpha=0.86,
            )
    ax.set_title(
        "BERTopic topic centroids in the document-level embedding space"
    )
    ax.set_xlabel("UMAP 1")
    ax.set_ylabel("UMAP 2")
    ax.legend(
        loc="center left",
        bbox_to_anchor=(1.01, 0.5),
        fontsize=8,
        frameon=False,
    )
    fig.tight_layout()
    fig.savefig(
        FIGURE_DIR / "response_topic_centroid_umap.png",
        dpi=300,
        bbox_inches="tight",
    )
    plt.close(fig)
    topic_centroids.to_csv(
        OUTPUT_DIR / "topic_centroid_umap_coordinates.csv",
        index=False,
    )
    fig, ax = plt.subplots(figsize=(13, 9))
    question_order = (
        figure_data["question"]
        .fillna("Unknown")
        .value_counts()
        .index
        .tolist()
    )
    for index, question_value in enumerate(question_order):
        question_data = figure_data.loc[
            figure_data["question"].fillna("Unknown")
            == question_value
        ]
        ax.scatter(
            question_data["umap_1"],
            question_data["umap_2"],
            s=7,
            alpha=0.46,
            color=novel_colour_map(index),
            edgecolors="none",
            label=f"{question_value} (n={len(question_data):,})",
            rasterized=True,
        )
    ax.set_title(
        "Response-level embedding space coloured by source question"
    )
    ax.set_xlabel("UMAP 1")
    ax.set_ylabel("UMAP 2")
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(
        FIGURE_DIR / "response_umap_by_question.png",
        dpi=300,
        bbox_inches="tight",
    )
    plt.close(fig)
    hover_columns = [
        "response_id",
        "question",
        "topic_id",
        "bertopic_label",
        "existing_code",
        "detailed_manual_theme",
    ]
    save_interactive_scatter(
        data=figure_data.drop(columns=["model_text"]),
        colour_col="visual_family",
        hover_columns=hover_columns,
        output_path=(
            INTERACTIVE_DIR
            / "response_umap_interpretable_families.html"
        ),
        title=(
            "Response-level embedding space with interpretable "
            "thematic labels"
        ),
    )
    save_interactive_scatter(
        data=figure_data.drop(columns=["model_text"]),
        colour_col="final_novel_theme",
        hover_columns=hover_columns,
        output_path=(
            INTERACTIVE_DIR
            / "response_umap_final_novel_themes.html"
        ),
        title="Final novel themes in the response embedding space",
    )
    dictionary_rows: list[dict[str, Any]] = []
    for option_number, option_label in EXISTING_OPTION_LABELS.items():
        dictionary_rows.append(
            {
                "source": "Existing questionnaire option",
                "code_or_theme": f"VACCRUFUSE3_{option_number}",
                "detailed_label": option_label,
                "visual_family": (
                    EXISTING_CODE_TO_VISUAL_FAMILY[option_number]
                ),
            }
        )
    for final_theme in sorted(EXPECTED_FINAL_NOVEL_THEMES):
        dictionary_rows.append(
            {
                "source": "Novel free-text theme",
                "code_or_theme": final_theme,
                "detailed_label": final_theme,
                "visual_family": NOVEL_THEME_SHORT_LABELS[final_theme],
            }
        )
    pd.DataFrame(dictionary_rows).to_csv(
        OUTPUT_DIR / "figure_theme_dictionary.csv",
        index=False,
    )
    run_summary = pd.DataFrame(
        [
            {
                "number_responses": len(figure_data),
                "number_unique_participants": (
                    figure_data["participant_id"].nunique()
                ),
                "number_non_outlier_responses": int(
                    (~figure_data["is_outlier"]).sum()
                ),
                "number_outlier_responses": int(
                    figure_data["is_outlier"].sum()
                ),
                "number_non_outlier_topics": int(
                    topic_centroids["topic_id"].nunique()
                ),
                "number_final_novel_themes": int(
                    figure_data.loc[
                        figure_data["is_novel"],
                        "final_novel_theme",
                    ].nunique()
                ),
                "embedding_model_path": str(EMBEDDING_MODEL_PATH),
                "visual_umap_n_neighbors": UMAP_N_NEIGHBORS,
                "visual_umap_min_dist": UMAP_MIN_DIST,
                "visual_umap_metric": UMAP_METRIC,
                "random_state": RANDOM_STATE,
                "interpretation": (
                    "Descriptive visualisation only; topic assignments and "
                    "manual themes were fixed before this 2D UMAP was created."
                ),
            }
        ]
    )
    run_summary.to_csv(
        OUTPUT_DIR / "run_summary.csv",
        index=False,
    )
    print("\n06c interpretable UMAP completed successfully.")
    print(f"Responses: {len(figure_data):,}")
    print(
        "Unique participants: "
        f"{figure_data['participant_id'].nunique():,}"
    )
    print(
        "Outliers: "
        f"{figure_data['is_outlier'].sum():,}"
    )
    print(
        "Non-outlier topics: "
        f"{topic_centroids['topic_id'].nunique():,}"
    )
    print(f"Outputs saved to: {OUTPUT_DIR}")
    print("\nMain static figures:")
    print(
        FIGURE_DIR / "response_umap_interpretable_families.png"
    )
    print(
        FIGURE_DIR / "response_umap_final_novel_themes.png"
    )
    print(
        FIGURE_DIR / "response_topic_centroid_umap.png"
    )
    print(
        FIGURE_DIR / "response_umap_by_question.png"
    )
if __name__ == "__main__":
    main()
