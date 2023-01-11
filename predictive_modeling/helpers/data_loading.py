import pandas as pd
import numpy as np
from pathlib import Path

def get_midpoint(labels: pd.Series):
    labs = labels.str.replace(r"[k+$miles]", "", regex=True)
    labs = labs.str.split(' to ', 1, expand=True)
    labs = labs.fillna(value=np.nan)
    labs[0] = labs[0].astype(float)
    labs[1] = labs[1].astype(float)
    midpoints = np.mean(labs, axis=1)
    return midpoints

def encode_ordinals(data: pd.DataFrame):
    return data

def load_carmax_data(data_path: Path):
    data =  pd.read_csv(data_path)

    data['appraisal_offer_midpoint'] = get_midpoint(data['appraisal_offer'])
    data['price_midpoint'] = get_midpoint(data['price'])
    
    data = encode_ordinals(data)

    return data