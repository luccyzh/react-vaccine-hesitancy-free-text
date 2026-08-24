# REACT vaccine hesitancy free-text analysis

Code supporting an MSc dissertation analysing free-text responses provided by vaccine-hesitant participants in the REACT study.

## Overview

The analysis examines whether free-text responses entered under the questionnaire option `Other` provide information beyond predefined vaccine-hesitancy reasons. The workflow includes data preparation, response- and sentence-level BERTopic models, model comparison, topic review and mapping, participant-level augmentation of structured reasons, consensus clustering, and NHS-linked vaccination outcome analyses.

## Repository structure

- `scripts/01_prepare_data.R` – prepare the free-text analysis dataset.
- `scripts/02_fit_response_bertopic.py` – fit the response-level BERTopic model.
- `scripts/03_fit_sentence_bertopic.py` – fit the sentence-level BERTopic model.
- `scripts/04_compare_models.py` and `scripts/04b_manual_interpretability_review.py` – compare topic-model specifications and prepare interpretability review outputs.
- `scripts/05_generate_topic_review.py` – prepare topic-level material for researcher review and LLM-assisted interpretation.
- `scripts/06_*` – map reviewed topics to participants and produce descriptive/QC outputs.
- `scripts/07_*` – augmented consensus clustering and participant-level cluster indicators.
- `scripts/08_*` – NHS-linked vaccination outcome models and original-versus-augmented comparisons.
- `scripts/09_*` and `scripts/10*` – final figures, tables, descriptive analyses and quality-control outputs.
- `scripts/pipeline_config.py` – shared Python paths and model settings used in the secure analysis environment.

## Reproducibility

The analysis was conducted in a secure research environment using R 4.4.3 and Python 3.12.10. BERTopic was run locally using sentence-transformer embeddings, UMAP dimensionality reduction and HDBSCAN clustering. A locally hosted Qwen3 8B model accessed through Ollama was used only to suggest topic labels, groupings and questionnaire matches; final classifications were made by the researcher.

Scripts retain the file paths and environment assumptions used during the original analysis. These paths will need to be adapted before use in another environment.

## Data availability

Participant-level REACT data, linked NHS vaccination records, free-text responses, intermediate datasets and derived participant-level outputs are not included in this repository because they are held within a secure research environment and are subject to data-governance restrictions.

The repository therefore provides analysis code only and is not intended to enable reconstruction of restricted participant-level data outside the approved environment.

## Notes

Some scripts depend on project-specific functions or questionnaire metadata available only within the secure REACT analysis environment. The code is provided to document the analytical workflow used for the dissertation.
