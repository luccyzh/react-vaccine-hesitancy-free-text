from __future__ import annotations

import ast
import re
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
from gensim.corpora import Dictionary
from gensim.models import CoherenceModel

from pipeline_config import COMPARISON_DIR, RESPONSE_DIR, SENTENCE_DIR


def parse_topic_words(value: object, n_words: int = 10) -> list[str]:
    if pd.isna(value):
        return []
    if isinstance(value, list):
        return [str(x) for x in value[:n_words]]
    text = str(value)
    try:
        parsed = ast.literal_eval(text)
        if isinstance(parsed, list):
            return [str(x) for x in parsed[:n_words]]
    except (ValueError, SyntaxError):
        pass
    return [x.strip() for x in text.split(",") if x.strip()][:n_words]


def tokenize(text: object) -> list[str]:
    return re.findall(r"[a-zA-Z]{3,}", str(text).lower())


def evaluate_model(
    model_name: str,
    topic_path: Path,
    doc_path: Path,
    text_col: str,
) -> tuple[dict, pd.DataFrame]:
    if not topic_path.exists() or not doc_path.exists():
        raise FileNotFoundError(f"Missing outputs for {model_name}. Run its fit script first.")

    topic_info = pd.read_csv(topic_path)
    doc_info = pd.read_csv(doc_path)
    topic_non_outlier = topic_info[topic_info["Topic"] != -1].copy()

    topic_words = [parse_topic_words(x) for x in topic_non_outlier["Representation"]]
    tokenized_docs = [tokenize(x) for x in doc_info[text_col].fillna("")]
    tokenized_docs = [tokens for tokens in tokenized_docs if tokens]
    dictionary = Dictionary(tokenized_docs)

    vocab = set(dictionary.token2id)
    filtered_topic_words = [
        [word.lower() for word in words if word.lower() in vocab]
        for words in topic_words
    ]
    filtered_topic_words = [words for words in filtered_topic_words if len(words) >= 2]

    coherence = float("nan")
    if filtered_topic_words and tokenized_docs:
        coherence = CoherenceModel(
            topics=filtered_topic_words,
            texts=tokenized_docs,
            dictionary=dictionary,
            coherence="c_v",
            processes=1,
        ).get_coherence()

    flattened = [word.lower() for topic in topic_words for word in topic]
    diversity = len(set(flattened)) / len(flattened) if flattened else float("nan")

    outlier_mask = doc_info["Topic"] == -1
    sizes = topic_non_outlier["Count"]

    summary = {
        "model": model_name,
        "analysis_units": int(len(doc_info)),
        "unique_responses": int(doc_info["response_id"].nunique()),
        "unique_participants": int(doc_info["participant_id"].nunique()),
        "number_topics_excluding_minus1": int(topic_non_outlier["Topic"].nunique()),
        "outlier_count": int(outlier_mask.sum()),
        "outlier_percentage": float(outlier_mask.mean() * 100),
        "topic_size_median": float(sizes.median()),
        "topic_size_q1": float(sizes.quantile(0.25)),
        "topic_size_q3": float(sizes.quantile(0.75)),
        "topic_size_iqr": float(sizes.quantile(0.75) - sizes.quantile(0.25)),
        "topic_size_min": int(sizes.min()),
        "topic_size_max": int(sizes.max()),
        "topic_diversity_top10": float(diversity),
        "c_v_coherence": float(coherence),
    }

    doc_info = doc_info.copy()
    doc_info["word_count"] = doc_info[text_col].fillna("").astype(str).str.split().str.len()
    length_summary = (
        doc_info.assign(outlier=outlier_mask)
        .groupby("outlier")["word_count"]
        .agg(["count", "mean", "median"])
        .reset_index()
    )
    length_summary.insert(0, "model", model_name)
    return summary, length_summary


def main() -> None:
    response_summary, response_length = evaluate_model(
        "response",
        RESPONSE_DIR / "response_topic_info.csv",
        RESPONSE_DIR / "response_document_topics.csv",
        "model_text",
    )
    sentence_summary, sentence_length = evaluate_model(
        "sentence",
        SENTENCE_DIR / "sentence_topic_info.csv",
        SENTENCE_DIR / "sentence_document_topics.csv",
        "sentence",
    )

    comparison = pd.DataFrame([response_summary, sentence_summary])
    comparison.to_csv(COMPARISON_DIR / "response_vs_sentence_metrics.csv", index=False)
    pd.concat([response_length, sentence_length], ignore_index=True).to_csv(
        COMPARISON_DIR / "outlier_length_comparison.csv", index=False
    )

    plt.figure(figsize=(6, 5))
    plt.bar(comparison["model"], comparison["outlier_percentage"])
    plt.ylabel("Outlier percentage")
    plt.title("BERTopic outlier rate")
    plt.tight_layout()
    plt.savefig(COMPARISON_DIR / "figures" / "outlier_rate_comparison.png", dpi=300)
    plt.savefig(COMPARISON_DIR / "figures" / "outlier_rate_comparison.pdf")
    plt.close()

    plt.figure(figsize=(6, 5))
    plt.bar(comparison["model"], comparison["c_v_coherence"])
    plt.ylabel("C_v coherence")
    plt.title("BERTopic coherence comparison")
    plt.tight_layout()
    plt.savefig(COMPARISON_DIR / "figures" / "coherence_comparison.png", dpi=300)
    plt.savefig(COMPARISON_DIR / "figures" / "coherence_comparison.pdf")
    plt.close()

    decision_template = pd.DataFrame(
        [
            {
                "criterion": "c_v_coherence",
                "response_result": response_summary["c_v_coherence"],
                "sentence_result": sentence_summary["c_v_coherence"],
                "preferred_model": "",
                "notes": "Higher is better, but use with interpretability and outlier rate.",
            },
            {
                "criterion": "outlier_percentage",
                "response_result": response_summary["outlier_percentage"],
                "sentence_result": sentence_summary["outlier_percentage"],
                "preferred_model": "",
                "notes": "Lower may be preferable; small differences are not decisive.",
            },
            {
                "criterion": "manual_interpretability",
                "response_result": "TO REVIEW",
                "sentence_result": "TO REVIEW",
                "preferred_model": "",
                "notes": "Review fixed samples of topics and representative text inside the enclave.",
            },
            {
                "criterion": "context_preservation",
                "response_result": "Full response context",
                "sentence_result": "Sentence-level fragments",
                "preferred_model": "",
                "notes": "Prefer response-level modelling when performance is similar because it preserves full-response context.",
            },
        ]
    )
    decision_template.to_csv(COMPARISON_DIR / "final_model_decision_template.csv", index=False)
    print(comparison.to_string(index=False))


if __name__ == "__main__":
    main()
