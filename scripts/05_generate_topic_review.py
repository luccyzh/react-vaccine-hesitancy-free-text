from __future__ import annotations

import argparse
import ast
from pathlib import Path

import pandas as pd

from pipeline_config import (
    GROUPING_DIR,
    N_REPRESENTATIVE_EXAMPLES,
    PREP_DIR,
    RESPONSE_DIR,
    SENTENCE_DIR,
)


def parse_representation(value: object, n_words: int = 10) -> str:
    """Convert BERTopic Representation into a readable comma-separated string."""
    if pd.isna(value):
        return ""

    try:
        parsed = ast.literal_eval(str(value))
        if isinstance(parsed, list):
            return ", ".join(str(x) for x in parsed[:n_words])
    except (ValueError, SyntaxError):
        pass

    return str(value)


def load_threshold_reference() -> dict:
    """
    Load the lowest prevalence among existing questionnaire options.

    Important:
    This is a REFERENCE for the prevalence of FINAL BROADER THEMES.
    It must not be used to remove individual BERTopic topics, because
    one broader theme may contain several smaller machine-generated topics.
    """
    path = PREP_DIR / "existing_option_threshold.csv"

    if not path.exists():
        return {
            "available": False,
            "minimum_existing_prevalence": float("nan"),
            "denominator": pd.NA,
            "threshold_variable": "",
            "threshold_option_label": "",
            "source_file": str(path),
        }

    threshold_df = pd.read_csv(path)

    if threshold_df.empty:
        raise ValueError(f"Threshold file is empty: {path}")

    row = threshold_df.iloc[0]

    return {
        "available": True,
        "minimum_existing_prevalence": float(
            row.get("minimum_existing_prevalence", float("nan"))
        ),
        "denominator": int(row["denominator"])
        if "denominator" in row and pd.notna(row["denominator"])
        else pd.NA,
        "threshold_variable": str(row.get("threshold_variable", "")),
        "threshold_option_label": str(row.get("threshold_option_label", "")),
        "source_file": str(path),
    }


def require_columns(df: pd.DataFrame, required: list[str], file_label: str) -> None:
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise KeyError(
            f"{file_label} is missing required columns: {', '.join(missing)}"
        )


def model_paths(model: str) -> tuple[Path, Path, Path, str]:
    if model == "response":
        return (
            RESPONSE_DIR / "response_topic_info.csv",
            RESPONSE_DIR / "response_document_topics.csv",
            RESPONSE_DIR / "response_hierarchical_topics.csv",
            "model_text",
        )

    return (
        SENTENCE_DIR / "sentence_topic_info.csv",
        SENTENCE_DIR / "sentence_document_topics.csv",
        SENTENCE_DIR / "sentence_hierarchical_topics.csv",
        "sentence",
    )


def build_topic_review(
    model: str,
    topics: pd.DataFrame,
    docs: pd.DataFrame,
    text_col: str,
    threshold_reference: dict,
) -> pd.DataFrame:
    topics = topics.loc[topics["Topic"] != -1].copy()
    topics["keywords"] = topics["Representation"].apply(parse_representation)

    rows: list[dict] = []

    for topic_id in topics["Topic"].astype(int):
        subset = docs.loc[docs["Topic"] == topic_id].copy()

        # Put BERTopic-designated representative documents first when available.
        if "Representative_document" in subset.columns:
            representative_mask = (
                subset["Representative_document"]
                .astype(str)
                .str.lower()
                .isin(["true", "1"])
            )
            subset = pd.concat(
                [
                    subset.loc[representative_mask],
                    subset.loc[~representative_mask],
                ],
                ignore_index=True,
            ).drop_duplicates()

        examples = (
            subset[text_col]
            .dropna()
            .astype(str)
            .drop_duplicates()
            .head(N_REPRESENTATIVE_EXAMPLES)
            .tolist()
        )

        topic_info_row = topics.loc[topics["Topic"] == topic_id].iloc[0]

        unique_participants = int(subset["participant_id"].nunique())
        unique_responses = int(subset["response_id"].nunique())

        # Topic prevalence is reported descriptively using the free-text cohort,
        # not compared against the questionnaire threshold for deletion.
        free_text_denominator = int(docs["participant_id"].nunique())
        topic_participant_prevalence = (
            unique_participants / free_text_denominator
            if free_text_denominator > 0
            else float("nan")
        )

        row = {
            # ----------------------------
            # Machine-generated topic data
            # ----------------------------
            "model": model,
            "topic_id": int(topic_id),
            "analysis_unit_count": int(len(subset)),
            "unique_response_count": unique_responses,
            "unique_participant_count": unique_participants,
            "free_text_cohort_denominator": free_text_denominator,
            "topic_participant_prevalence_in_free_text_cohort": (
                topic_participant_prevalence
            ),
            "keywords": topic_info_row["keywords"],
            "bertopic_name": topic_info_row.get("Name", ""),
            "representative_examples_ENCLAVE_ONLY": " || ".join(examples),

            # ----------------------------
            # LLM first-pass suggestions
            # ----------------------------
            "llm_topic_label": "",
            "llm_broader_theme": "",
            "llm_existing_questionnaire_match": "",
            "llm_existing_option_label": "",
            "llm_novel_theme_flag": "",
            "llm_confidence": "",
            "llm_reason": "",
            "llm_uncertainty": "",

            # ----------------------------
            # Human final decisions
            # ----------------------------
            "human_topic_label": "",
            "human_broader_theme": "",
            "human_existing_questionnaire_match": "",
            "human_existing_option_label": "",
            "human_novel_theme_flag": "",
            "human_decision": "",
            # allowed examples:
            # keep_existing / keep_new / merge / ignore / review
            "merge_with_topic_ids": "",
            "final_theme_id": "",
            "final_theme_name": "",
            "final_variable_name": "",
            "human_confidence": "",
            "human_rationale": "",

            # ----------------------------
            # Threshold reference
            # ----------------------------
            "existing_option_threshold_prevalence_REFERENCE_ONLY": (
                threshold_reference["minimum_existing_prevalence"]
            ),
            "existing_option_threshold_denominator_REFERENCE_ONLY": (
                threshold_reference["denominator"]
            ),
            "existing_option_threshold_variable_REFERENCE_ONLY": (
                threshold_reference["threshold_variable"]
            ),
            "existing_option_threshold_label_REFERENCE_ONLY": (
                threshold_reference["threshold_option_label"]
            ),
            "threshold_instruction": (
                "Do not exclude this individual topic using the threshold. "
                "Recalculate prevalence after topics are combined into final broader themes."
            ),
        }

        rows.append(row)

    review = pd.DataFrame(rows)

    return review.sort_values(
        ["unique_participant_count", "analysis_unit_count"],
        ascending=False,
    ).reset_index(drop=True)


def write_llm_prompt(model: str, output_path: Path) -> None:
    prompt = f"""You are helping group BERTopic topics from a vaccine hesitancy free-text study.

INPUT
- Model: {model}
- Each row is one machine-generated BERTopic topic.
- Use the keywords, topic size and representative examples.
- Representative examples are enclave-only and must not be copied outside the secure environment.

TASK
For every topic, propose:
1. llm_topic_label: a short, specific human-readable topic label.
2. llm_broader_theme: a broader conceptual theme that may contain several BERTopic topics.
3. llm_existing_questionnaire_match: Yes / No / Partial / Unclear.
4. llm_existing_option_label: the closest existing questionnaire option, when relevant.
5. llm_novel_theme_flag: Yes / No / Unclear.
6. llm_confidence: Low / Medium / High.
7. llm_reason: one concise explanation.
8. llm_uncertainty: note ambiguity, mixed content, overlap or insufficient evidence.

IMPORTANT RULES
- Do not delete a topic merely because it is small.
- Do not use the questionnaire prevalence threshold at individual-topic level.
- Several small BERTopic topics may combine into one meaningful broader theme.
- Keep conceptually distinct topics separate, even when their words overlap.
- Distinguish machine-generated topics from researcher-defined broader themes.
- Do not invent evidence not present in the keywords or examples.
- Preserve topic_id exactly.
- Return one row for every topic_id.
- The researcher will review and edit all LLM suggestions.

OUTPUT FORMAT
Return a CSV-style table with exactly these columns:
topic_id,llm_topic_label,llm_broader_theme,llm_existing_questionnaire_match,llm_existing_option_label,llm_novel_theme_flag,llm_confidence,llm_reason,llm_uncertainty
"""

    output_path.write_text(prompt, encoding="utf-8")


def main(model: str) -> None:
    GROUPING_DIR.mkdir(parents=True, exist_ok=True)

    threshold_reference = load_threshold_reference()
    topic_path, doc_path, hierarchy_path, text_col = model_paths(model)

    for path in [topic_path, doc_path]:
        if not path.exists():
            raise FileNotFoundError(f"Missing {model} output: {path}")

    topics = pd.read_csv(topic_path)
    docs = pd.read_csv(doc_path)

    require_columns(
        topics,
        ["Topic", "Count", "Representation"],
        f"{model} topic information",
    )
    require_columns(
        docs,
        ["Topic", "participant_id", "response_id", text_col],
        f"{model} document topics",
    )

    review = build_topic_review(
        model=model,
        topics=topics,
        docs=docs,
        text_col=text_col,
        threshold_reference=threshold_reference,
    )

    enclave_path = (
        GROUPING_DIR
        / f"{model}_topic_review_LLM_READY_ENCLAVE_ONLY.csv"
    )
    keywords_path = (
        GROUPING_DIR
        / f"{model}_topic_review_LLM_READY_KEYWORDS_ONLY.csv"
    )
    summary_path = (
        GROUPING_DIR
        / f"{model}_topic_review_LLM_READY_summary.csv"
    )
    prompt_path = GROUPING_DIR / f"{model}_LLM_grouping_prompt.txt"

    review.to_csv(
        enclave_path,
        index=False,
        encoding="utf-8-sig",
    )

    safe_review = review.drop(
        columns=["representative_examples_ENCLAVE_ONLY"]
    )
    safe_review.to_csv(
        keywords_path,
        index=False,
        encoding="utf-8-sig",
    )

    if hierarchy_path.exists():
        hierarchy = pd.read_csv(hierarchy_path)
        hierarchy.to_csv(
            GROUPING_DIR / f"{model}_hierarchical_topics_reference.csv",
            index=False,
            encoding="utf-8-sig",
        )

    summary = pd.DataFrame(
        [
            {
                "model": model,
                "number_topics_reviewed": int(len(review)),
                "free_text_cohort_participants": int(
                    docs["participant_id"].nunique()
                ),
                "free_text_responses": int(
                    docs["response_id"].nunique()
                ),
                "existing_option_threshold_available": bool(
                    threshold_reference["available"]
                ),
                "existing_option_threshold_prevalence_REFERENCE_ONLY": (
                    threshold_reference["minimum_existing_prevalence"]
                ),
                "existing_option_threshold_denominator_REFERENCE_ONLY": (
                    threshold_reference["denominator"]
                ),
                "threshold_use": (
                    "Reference only. Apply after BERTopic topics are merged "
                    "into final broader themes; never use for automatic "
                    "individual-topic exclusion."
                ),
                "next_step": (
                    "Run local LLM first-pass grouping, then complete human "
                    "review columns and create a final topic-to-theme mapping."
                ),
            }
        ]
    )
    summary.to_csv(
        summary_path,
        index=False,
        encoding="utf-8-sig",
    )

    write_llm_prompt(model, prompt_path)

    print("Created final LLM-ready topic review outputs.")
    print(f"Model: {model}")
    print(f"Topics reviewed: {len(review)}")
    print(f"Enclave file: {enclave_path}")
    print(f"Keywords-only file: {keywords_path}")
    print(f"Prompt: {prompt_path}")
    print(
        "Threshold rule: reference only; recalculate after final broader-theme grouping."
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--model",
        choices=["response", "sentence"],
        default="response",
        help="Model to review. Default: response",
    )
    args = parser.parse_args()
    main(args.model)
