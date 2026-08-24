from __future__ import annotations

from pathlib import Path

import pandas as pd

from pipeline_config import COMPARISON_DIR, RESPONSE_DIR, SENTENCE_DIR


REVIEW_DIR = COMPARISON_DIR / "manual_interpretability_review"
REVIEW_DIR.mkdir(parents=True, exist_ok=True)


def require_columns(df: pd.DataFrame, required: list[str], file_label: str) -> None:
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise KeyError(f"{file_label} is missing required columns: {missing}")


def select_topics_for_review(
    topic_info: pd.DataFrame,
    n_large: int = 10,
    n_medium: int = 5,
    n_small: int = 5,
) -> list[int]:
    topics = (
        topic_info.loc[topic_info["Topic"] != -1]
        .sort_values("Count", ascending=False)
        .reset_index(drop=True)
    )

    if topics.empty:
        return []

    large = topics.head(n_large)
    remaining = topics.iloc[n_large:].copy()

    if remaining.empty:
        selected = large
    else:
        middle_start = max(0, len(remaining) // 2 - n_medium // 2)
        medium = remaining.iloc[middle_start : middle_start + n_medium]
        small = remaining.tail(n_small)
        selected = pd.concat([large, medium, small], ignore_index=True)

    return selected["Topic"].drop_duplicates().astype(int).tolist()


def build_review_table(
    model_name: str,
    topic_path: Path,
    document_path: Path,
    text_col: str,
    n_examples: int = 5,
) -> pd.DataFrame:
    if not topic_path.exists():
        raise FileNotFoundError(f"Missing topic file: {topic_path}")
    if not document_path.exists():
        raise FileNotFoundError(f"Missing document file: {document_path}")

    topic_info = pd.read_csv(topic_path)
    documents = pd.read_csv(document_path)

    require_columns(topic_info, ["Topic", "Count", "Representation"], str(topic_path))
    require_columns(documents, ["Topic", text_col], str(document_path))

    rows: list[dict] = []
    selected_topics = select_topics_for_review(topic_info)

    for topic_id in selected_topics:
        topic_row = topic_info.loc[topic_info["Topic"] == topic_id].iloc[0]
        subset = documents.loc[documents["Topic"] == topic_id].copy()

        if "Representative_document" in subset.columns:
            subset["_rep_order"] = subset["Representative_document"].fillna(False).astype(bool)
            sort_cols = ["_rep_order"]
            ascending = [False]
            if "Probability" in subset.columns:
                sort_cols.append("Probability")
                ascending.append(False)
            subset = subset.sort_values(sort_cols, ascending=ascending)
        elif "Probability" in subset.columns:
            subset = subset.sort_values("Probability", ascending=False)

        examples = (
            subset[text_col]
            .dropna()
            .astype(str)
            .drop_duplicates()
            .head(n_examples)
            .tolist()
        )

        row = {
            "model": model_name,
            "topic_id": int(topic_id),
            "topic_count": int(topic_row["Count"]),
            "topic_name": topic_row.get("Name", ""),
            "representation": topic_row.get("Representation", ""),
            "interpretability_score_0_to_2": "",
            "proposed_human_label": "",
            "mixed_topic_flag_yes_no": "",
            "fragmented_text_flag_yes_no": "",
            "review_notes": "",
        }
        for i in range(n_examples):
            row[f"example_{i + 1}"] = examples[i] if i < len(examples) else ""
        rows.append(row)

    return pd.DataFrame(rows)


def main() -> None:
    response_review = build_review_table(
        model_name="response",
        topic_path=RESPONSE_DIR / "response_topic_info.csv",
        document_path=RESPONSE_DIR / "response_document_topics.csv",
        text_col="model_text",
    )

    sentence_review = build_review_table(
        model_name="sentence",
        topic_path=SENTENCE_DIR / "sentence_topic_info.csv",
        document_path=SENTENCE_DIR / "sentence_document_topics.csv",
        text_col="sentence",
    )

    combined = pd.concat([response_review, sentence_review], ignore_index=True)
    output_path = REVIEW_DIR / "manual_interpretability_review.csv"
    combined.to_csv(output_path, index=False, encoding="utf-8-sig")

    instructions = pd.DataFrame(
        [
            {
                "score": 0,
                "definition": "No clear common theme; cannot assign a defensible label.",
            },
            {
                "score": 1,
                "definition": "Partly interpretable, but mixed, broad, or unstable.",
            },
            {
                "score": 2,
                "definition": "Clear and coherent topic that can be labelled consistently.",
            },
        ]
    )
    instructions.to_csv(REVIEW_DIR / "interpretability_scoring_guide.csv", index=False)

    print("Created manual interpretability review sheet:")
    print(output_path)
    print(combined.groupby("model").size().to_string())


if __name__ == "__main__":
    main()
