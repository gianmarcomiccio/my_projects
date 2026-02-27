from pydantic import BaseModel
from typing import Literal

class ColumnTypes(BaseModel):
    types: list[str]

class TimeFormatDecision(BaseModel):
    reasoning: str
    target_format: Literal[
        "%H:%M", "%H:%M:%S", "%S", 
        "%d/%m/%Y", "%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M:%S", 
        "%m/%Y", "%Y"
    ]

class MoneyFormatDecision(BaseModel):
    reasoning: str
    is_mixed_currency: bool  
    detected_currency: str   
    scale_decision: Literal["None", "Thousands", "Millions", "Billions"]
    decimal_separator: Literal[".", ","]

class NameFormatDecision(BaseModel):
    reasoning: str
    entity_type: Literal["Human Names", "Locations/Other"]
    dominant_format: Literal["First Last", "Last First", "N/A"]

class HeaderDecision(BaseModel):
    reasoning: str
    header_row_index: int
    header_col_index: int

class MissingDataDecision(BaseModel):
    reasoning: str
    custom_na_strings_to_wipe: list[str]
    remove_completely_empty_rows: bool
    remove_completely_empty_columns: bool

class FeatureDescription(BaseModel):
    feature_name: str
    inferred_data_type: str
    description: str

class DatasetDescription(BaseModel):
    general_summary: str
    features: list[FeatureDescription]