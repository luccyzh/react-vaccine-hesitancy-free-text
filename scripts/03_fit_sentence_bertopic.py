from __future__ import annotations

import json
import platform
import re
from datetime import datetime

import matplotlib.pyplot as plt
import pandas as pd
from bertopic import BERTopic
from hdbscan import HDBSCAN
from sentence_transformers import SentenceTransformer
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP

from pipeline_config import (
    DATA_INTERIM_DIR,
    EMBEDDING_BATCH_SIZE,
    EMBEDDING_MODEL_PATH,
    MIN_DIST,
    MIN_TOPIC_SIZE,
    N_COMPONENTS,
    N_NEIGHBORS,
    RANDOM_STATE,
    SENTENCE_DIR,
)


def split_sentences(text: str) -> list[str]:
    """Conservative sentence splitting; keeps punctuation-based context."""
    parts = re.split(r"(?<=[.!?;])\s+|\n+", str(text))
    return [part.strip() for part in parts if part and part.strip()]


def main() -> None:
    input_path = DATA_INTERIM_DIR / "response_level_input.csv"
    if not input_path.exists():
        raise FileNotFoundError(f"Run 01_prepare_data.R first. Missing: {input_path}")
    if not EMBEDDING_MODEL_PATH.exists():
        raise FileNotFoundError(
            f"Embedding model path not found: {EMBEDDING_MODEL_PATH}. "
            "Check pipeline_config.py in the enclave."
        )

    response_df = pd.read_csv(input_path)
    required = {"participant_id", "response_id", "question", "original_text", "model_text"}
    missing = required.difference(response_df.columns)
    if missing:
        raise ValueError(f"Missing required input columns: {sorted(missing)}")

    rows: list[dict] = []
    for row in response_df.itertuples(index=False):
        for sentence_number, sentence in enumerate(split_sentences(row.model_text), start=1):
            rows.append(
                {
                    "participant_id": row.participant_id,
                    "response_id": row.response_id,
                    "sentence_id": f"{row.response_id}__S{sentence_number:02d}",
                    "question": row.question,
                    "original_text": row.original_text,
                    "response_model_text": row.model_text,
                    "sentence_number": sentence_number,
                    "sentence": sentence,
                    "sentence_n_char": len(sentence),
                    "sentence_n_word": len(sentence.split()),
                }
            )

    sentence_df = pd.DataFrame(rows)
    if sentence_df.empty:
        raise ValueError("Sentence splitting produced no rows")
    if sentence_df["sentence_id"].duplicated().any():
        raise ValueError("sentence_id is not unique")

    sentence_df.to_csv(DATA_INTERIM_DIR / "sentence_level_input.csv", index=False)
    docs = sentence_df["sentence"].tolist()

    embedding_model = SentenceTransformer(str(EMBEDDING_MODEL_PATH))
    embeddings = embedding_model.encode(
        docs,
        batch_size=EMBEDDING_BATCH_SIZE,
        show_progress_bar=True,
        convert_to_numpy=True,
        normalize_embeddings=False,
    )

    umap_model = UMAP(
        n_neighbors=N_NEIGHBORS,
        n_components=N_COMPONENTS,
        min_dist=MIN_DIST,
        metric="cosine",
        random_state=RANDOM_STATE,
    )
    hdbscan_model = HDBSCAN(
        min_cluster_size=MIN_TOPIC_SIZE,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )
    vectorizer_model = CountVectorizer(stop_words="english")

    topic_model = BERTopic(
        embedding_model=embedding_model,
        umap_model=umap_model,
        hdbscan_model=hdbscan_model,
        vectorizer_model=vectorizer_model,
        calculate_probabilities=False,
        verbose=True,
    )
    topics, probabilities = topic_model.fit_transform(docs, embeddings)

    doc_info = topic_model.get_document_info(docs)
    doc_info = pd.concat([sentence_df.reset_index(drop=True), doc_info.reset_index(drop=True)], axis=1)
    topic_info = topic_model.get_topic_info()

    doc_info.to_csv(SENTENCE_DIR / "sentence_document_topics.csv", index=False)
    topic_info.to_csv(SENTENCE_DIR / "sentence_topic_info.csv", index=False)
    pd.DataFrame(embeddings).to_csv(SENTENCE_DIR / "sentence_embeddings.csv", index=False)

    try:
        topic_model.save(
            str(SENTENCE_DIR / "sentence_bertopic_model"),
            serialization="safetensors",
            save_ctfidf=True,
        )
    except TypeError:
        topic_model.save(str(SENTENCE_DIR / "sentence_bertopic_model"))

    topic_model.visualize_topics().write_html(str(SENTENCE_DIR / "sentence_intertopic_map.html"))
    topic_model.visualize_barchart(top_n_topics=30).write_html(
        str(SENTENCE_DIR / "sentence_top_topics_barchart.html")
    )

    hierarchical_topics = topic_model.hierarchical_topics(docs)
    hierarchical_topics.to_csv(SENTENCE_DIR / "sentence_hierarchical_topics.csv", index=False)
    topic_model.visualize_hierarchy(
        hierarchical_topics=hierarchical_topics
    ).write_html(str(SENTENCE_DIR / "sentence_topic_dendrogram.html"))

    non_outlier = topic_info[topic_info["Topic"] != -1].copy()
    plt.figure(figsize=(10, 6))
    plt.bar(non_outlier["Topic"].astype(str), non_outlier["Count"])
    plt.xticks([])
    plt.xlabel("Topic")
    plt.ylabel("Number of sentences")
    plt.title("Sentence-level BERTopic topic sizes")
    plt.tight_layout()
    plt.savefig(SENTENCE_DIR / "figures" / "sentence_topic_sizes.png", dpi=300)
    plt.savefig(SENTENCE_DIR / "figures" / "sentence_topic_sizes.pdf")
    plt.close()

    run_metadata = {
        "run_time": datetime.now().isoformat(timespec="seconds"),
        "python_version": platform.python_version(),
        "analysis_unit": "sentence",
        "n_analysis_units": len(sentence_df),
        "n_responses": int(sentence_df["response_id"].nunique()),
        "n_participants": int(sentence_df["participant_id"].nunique()),
        "n_topics_excluding_minus1": int((topic_info["Topic"] != -1).sum()),
        "outlier_count": int((doc_info["Topic"] == -1).sum()),
        "random_state": RANDOM_STATE,
        "min_topic_size": MIN_TOPIC_SIZE,
        "embedding_model_path": str(EMBEDDING_MODEL_PATH),
    }
    with open(SENTENCE_DIR / "sentence_run_metadata.json", "w", encoding="utf-8") as f:
        json.dump(run_metadata, f, indent=2)

    print("Sentence-level BERTopic completed")
    print(SENTENCE_DIR)


if __name__ == "__main__":
    main()
