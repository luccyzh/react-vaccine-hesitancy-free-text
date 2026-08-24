# ============================================================
# 02_fit_response_bertopic.py
#
# Purpose:
#   Fit the response-level BERTopic model using the canonical input
#   produced by 01_prepare_data.R.
#
# Pipeline:
#   response_level_input.csv
#   -> sentence-transformer embeddings
#   -> UMAP dimensionality reduction
#   -> HDBSCAN clustering
#   -> BERTopic topic representation
#   -> document assignments, topic summaries, model and figures
#
# Notes:
#   - BERTopic uses model_text, which should contain only minimally
#     processed text (trimmed text from 01_prepare_data.R).
#   - original_text is retained in the exported document-level file
#     for manual interpretation and audit.
#   - Model parameters are imported from pipeline_config.py.
# ============================================================

from __future__ import annotations

import os

# The enclave uses a locally stored Hugging Face model.
# These must be set before importing sentence_transformers.
os.environ["HF_HUB_OFFLINE"] = "1"
os.environ["TRANSFORMERS_OFFLINE"] = "1"

import matplotlib.pyplot as plt
import pandas as pd
from bertopic import BERTopic
from hdbscan import HDBSCAN
from sentence_transformers import SentenceTransformer
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP

# pipeline_config.py is loaded automatically by this import.
# Keep pipeline_config.py in the same folder as this script.
from pipeline_config import (
    DATA_INTERIM_DIR,
    EMBEDDING_BATCH_SIZE,
    EMBEDDING_MODEL_PATH,
    MIN_DIST,
    MIN_TOPIC_SIZE,
    N_COMPONENTS,
    N_NEIGHBORS,
    RANDOM_STATE,
    RESPONSE_DIR,
)


def main() -> None:
    # --------------------------------------------------------
    # 1. CHECK PATHS AND LOAD THE CANONICAL INPUT
    # --------------------------------------------------------
    input_path = DATA_INTERIM_DIR / "response_level_input.csv"
    figure_dir = RESPONSE_DIR / "figures"

    RESPONSE_DIR.mkdir(parents=True, exist_ok=True)
    figure_dir.mkdir(parents=True, exist_ok=True)

    if not input_path.exists():
        raise FileNotFoundError(
            f"Run 01_prepare_data.R first. Missing input file: {input_path}"
        )

    if not EMBEDDING_MODEL_PATH.exists():
        raise FileNotFoundError(
            f"Embedding model was not found at: {EMBEDDING_MODEL_PATH}\n"
            "Check EMBEDDING_MODEL_PATH in pipeline_config.py."
        )

    df = pd.read_csv(input_path)

    required_columns = {
        "participant_id",
        "response_id",
        "question",
        "original_text",
        "model_text",
        "n_char",
        "n_word",
    }
    missing_columns = required_columns.difference(df.columns)

    if missing_columns:
        raise ValueError(
            f"Missing required input columns: {sorted(missing_columns)}"
        )

    if df["response_id"].duplicated().any():
        duplicated_ids = df.loc[
            df["response_id"].duplicated(keep=False), "response_id"
        ].tolist()
        raise ValueError(
            "response_id is not unique. Example duplicates: "
            f"{duplicated_ids[:10]}"
        )

    # model_text is minimally processed text from Script 01:
    # leading/trailing spaces removed, but no stemming, lemmatisation,
    # lowercasing or stop-word deletion before embedding.
    docs = df["model_text"].fillna("").astype(str).str.strip().tolist()

    if any(text == "" for text in docs):
        raise ValueError("Blank model_text remains in the BERTopic input.")

    print(f"Number of responses used for BERTopic: {len(docs)}")
    print(f"Number of unique participants: {df['participant_id'].nunique()}")

    # --------------------------------------------------------
    # 2. LOAD THE LOCAL EMBEDDING MODEL
    # --------------------------------------------------------
    # Embedding model used in this analysis:
    # EMBEDDING_MODEL_PATH points to all-MiniLM-L6-v2.
    embedding_model = SentenceTransformer(str(EMBEDDING_MODEL_PATH))

    # Explicitly create embeddings so the same vectors can be passed
    # directly to BERTopic. They are not exported as a very large CSV.
    embeddings = embedding_model.encode(
        docs,
        batch_size=EMBEDDING_BATCH_SIZE,
        show_progress_bar=True,
        convert_to_numpy=True,
        normalize_embeddings=False,
    )

    # --------------------------------------------------------
    # 3. DEFINE BERTopic COMPONENTS
    # --------------------------------------------------------
    # UMAP reduces the high-dimensional embeddings before clustering.
    #
    # UMAP parameters:
    #   N_NEIGHBORS, N_COMPONENTS, MIN_DIST and RANDOM_STATE
    #   are defined in pipeline_config.py.
    umap_model = UMAP(
        n_neighbors=N_NEIGHBORS,
        n_components=N_COMPONENTS,
        min_dist=MIN_DIST,
        metric="cosine",
        random_state=RANDOM_STATE,
    )

    # HDBSCAN identifies dense groups and labels unassigned responses as -1.
    #
    # HDBSCAN parameter:
    #   MIN_TOPIC_SIZE is defined in pipeline_config.py.
    hdbscan_model = HDBSCAN(
        min_cluster_size=MIN_TOPIC_SIZE,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )

    # CountVectorizer is used for topic-word representation (c-TF-IDF).
    # It does NOT remove stop words before sentence-transformer embedding.
    vectorizer_model = CountVectorizer(stop_words="english")

    topic_model = BERTopic(
        embedding_model=embedding_model,
        umap_model=umap_model,
        hdbscan_model=hdbscan_model,
        vectorizer_model=vectorizer_model,
        # False avoids calculating a full document-by-topic probability matrix.
        # BERTopic still exports the assigned-topic confidence in document info.
        calculate_probabilities=False,
        verbose=True,
    )

    # --------------------------------------------------------
    # 4. FIT THE RESPONSE-LEVEL MODEL
    # --------------------------------------------------------
    topics, _ = topic_model.fit_transform(docs, embeddings)

    # --------------------------------------------------------
    # 5. EXPORT DOCUMENT- AND TOPIC-LEVEL RESULTS
    # --------------------------------------------------------
    bertopic_doc_info = topic_model.get_document_info(docs)

    # Retain participant IDs, response IDs and original text so every
    # assignment can be mapped back and manually reviewed.
    document_topics = pd.concat(
        [
            df[
                [
                    "participant_id",
                    "response_id",
                    "question",
                    "original_text",
                    "model_text",
                    "n_char",
                    "n_word",
                ]
            ].reset_index(drop=True),
            bertopic_doc_info.reset_index(drop=True),
        ],
        axis=1,
    )

    topic_info = topic_model.get_topic_info()

    document_topics.to_csv(
        RESPONSE_DIR / "response_document_topics.csv",
        index=False,
    )
    topic_info.to_csv(
        RESPONSE_DIR / "response_topic_info.csv",
        index=False,
    )

    # --------------------------------------------------------
    # 6. SAVE THE FITTED MODEL
    # --------------------------------------------------------
    # BERTopic versions differ in supported save arguments.
    try:
        topic_model.save(
            str(RESPONSE_DIR / "response_bertopic_model"),
            serialization="safetensors",
            save_ctfidf=True,
        )
    except TypeError:
        topic_model.save(
            str(RESPONSE_DIR / "response_bertopic_model")
        )

    # --------------------------------------------------------
    # 7. EXPORT INTERACTIVE VISUALISATIONS
    # --------------------------------------------------------
    topic_model.visualize_topics().write_html(
        str(RESPONSE_DIR / "response_intertopic_map.html")
    )

    topic_model.visualize_barchart(top_n_topics=30).write_html(
        str(RESPONSE_DIR / "response_top_topics_barchart.html")
    )

    # --------------------------------------------------------
    # 8. HIERARCHICAL TOPIC OUTPUT
    # --------------------------------------------------------
    hierarchical_topics = topic_model.hierarchical_topics(docs)

    hierarchical_topics.to_csv(
        RESPONSE_DIR / "response_hierarchical_topics.csv",
        index=False,
    )

    topic_model.visualize_hierarchy(
        hierarchical_topics=hierarchical_topics
    ).write_html(
        str(RESPONSE_DIR / "response_topic_dendrogram.html")
    )

    # --------------------------------------------------------
    # 9. STATIC TOPIC-SIZE FIGURE
    # --------------------------------------------------------
    non_outlier_topics = topic_info.loc[
        topic_info["Topic"] != -1
    ].copy()

    plt.figure(figsize=(10, 6))
    plt.bar(
        non_outlier_topics["Topic"].astype(str),
        non_outlier_topics["Count"],
    )
    plt.xticks([])
    plt.xlabel("Topic")
    plt.ylabel("Number of responses")
    plt.title("Response-level BERTopic topic sizes")
    plt.tight_layout()

    plt.savefig(
        figure_dir / "response_topic_sizes.png",
        dpi=300,
    )
    plt.savefig(
        figure_dir / "response_topic_sizes.pdf",
    )
    plt.close()

    # --------------------------------------------------------
    # 10. SIMPLE, READABLE RUN SUMMARY
    # --------------------------------------------------------
    # Save a readable run summary.
    # n_topics is a model result, NOT a manually specified topic number.
    run_summary = pd.DataFrame(
        {
            "measure": [
                "analysis_unit",
                "n_responses",
                "n_unique_participants",
                "n_topics_excluding_minus1",
                "outlier_count",
                "outlier_percentage",
                "embedding_model_path",
                "random_state",
                "min_topic_size",
                "umap_n_neighbors",
                "umap_n_components",
                "umap_min_dist",
            ],
            "value": [
                "response",
                len(document_topics),
                document_topics["participant_id"].nunique(),
                int((topic_info["Topic"] != -1).sum()),
                int((document_topics["Topic"] == -1).sum()),
                round(
                    100 * (document_topics["Topic"] == -1).mean(),
                    2,
                ),
                str(EMBEDDING_MODEL_PATH),
                RANDOM_STATE,
                MIN_TOPIC_SIZE,
                N_NEIGHBORS,
                N_COMPONENTS,
                MIN_DIST,
            ],
        }
    )

    run_summary.to_csv(
        RESPONSE_DIR / "response_run_summary.csv",
        index=False,
    )

    print("Response-level BERTopic completed successfully.")
    print(f"Outputs saved to: {RESPONSE_DIR}")


if __name__ == "__main__":
    main()
