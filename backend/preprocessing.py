import pandas as pd
import json
from pathlib import Path

def load_json_data(path):
    return json.loads(Path(path).read_text())

def load_csv_data(path):
    return pd.read_csv(path)