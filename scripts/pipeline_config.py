from pathlib import Path

# ============================================================
# PROJECT PATHS
# ============================================================
# Project root on enclave.
PROJECT_ROOT = Path(r"D:\projects\hda_2026\han")

# Local sentence-transformer model path from the original scripts.
EMBEDDING_MODEL_PATH = Path(r"S:\python_repo\models\all-MiniLM-L6-v2")

DATA_INTERIM_DIR = PROJECT_ROOT / "data" / "interim"
OUTPUT_ROOT = PROJECT_ROOT / "outputs"

PREP_DIR = OUTPUT_ROOT / "01_data_preparation"
RESPONSE_DIR = OUTPUT_ROOT / "02_response_bertopic"
SENTENCE_DIR = OUTPUT_ROOT / "03_sentence_bertopic"
COMPARISON_DIR = OUTPUT_ROOT / "04_model_comparison"
GROUPING_DIR = OUTPUT_ROOT / "05_topic_review"

# Each script gets one stage folder and one figures subfolder only.
for folder in [
    DATA_INTERIM_DIR,
    PREP_DIR,
    PREP_DIR / "figures",
    RESPONSE_DIR,
    RESPONSE_DIR / "figures",
    SENTENCE_DIR,
    SENTENCE_DIR / "figures",
    COMPARISON_DIR,
    COMPARISON_DIR / "figures",
    GROUPING_DIR,
    GROUPING_DIR / "figures",
]:
    folder.mkdir(parents=True, exist_ok=True)

# ============================================================
# REPRODUCIBILITY / MODEL SETTINGS
# ============================================================
RANDOM_STATE = 42
MIN_TOPIC_SIZE = 10
N_NEIGHBORS = 15
N_COMPONENTS = 5
MIN_DIST = 0.0
EMBEDDING_BATCH_SIZE = 32

# Used only for exported topic review examples.
N_REPRESENTATIVE_EXAMPLES = 5
