import os
import string
import pandas as pd
from openai import OpenAI
from dotenv import load_dotenv

# Import Pydantic models and Tools
from schemas import (
    ColumnTypes, TimeFormatDecision, MoneyFormatDecision, 
    NameFormatDecision, HeaderDecision, MissingDataDecision, DatasetDescription
)
from tools import (
    execute_time_formatting, execute_money_formatting, execute_int_formatting, 
    execute_float_formatting, execute_name_formatting, execute_missing_data_cleaning
)

load_dotenv()
client = OpenAI()

# Paste these exact functions from your original code:
# - reader_agent
# - time_agent_workflow
# - money_agent_workflow
# - int_agent_workflow
# - float_agent_workflow
# - name_agent_workflow
# - header_detection_agent
# - na_agent_workflow
# - dataset_description_agent

def reader_agent(df: pd.DataFrame) -> list[str]:
    # 1. We changed the input parameter to 'df: pd.DataFrame'
    print("[Reader Agent] Reading cropped dataframe to classify columns...")
    
    # 2. REMOVED the pd.read_excel() line entirely because the data is already loaded!
    
    # 3. Grab the sample directly from the passed dataframe
    sample_data = df.head(5).to_dict(orient="list")
    
    prompt = f"""
    Analyze the following data sample from an Excel file.
    For each column, determine its data type based on the values.
    You must return a list where each element corresponds to a column from left to right.
    
    You are ONLY allowed to use these exact categories: "time", "money", "int", "string", "float", "name", "unknown".
    
    CRITICAL DEFINITIONS:
    - "time": Includes standard formats (2023-01-01, 14:30), timestamps, AND natural language dates (e.g., "first of january 2016", "Q1 2024", "yesterday"). If the core meaning represents a date or time, it is "time", NEVER "string".
    - "money": Includes currency symbols ($100, €50), accounting formats, or financial abbreviations (100 USD) and natural language money expressions ("100 dollars", "fifty euros"). If the core meaning represents a monetary value, it is "money", NEVER "string".
    - "int": Whole numbers without decimals.
    - "float": Numbers containing decimals.
    - "name": Proper nouns. This includes human names (John Smith, Smith, John), cities, states (Alabama), or company names.
    - "string": General text, sentences, descriptions, or specific codes (e.g., ID-4552) that have no mathematical or temporal value.
    - "unknown": Use this ONLY if the column is complete gibberish or you cannot confidently assign it to any other category.

    Data sample (Columns and their first 5 values):
    {sample_data}
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are a data classification agent."},
            {"role": "user", "content": prompt}
        ],
        response_format=ColumnTypes
    )
    
    types = response.choices[0].message.parsed.types
    print(f"     [Reader Agent Classification] {types}")
    return types





def time_agent_workflow(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"  -> [Time Agent] Taking control of column: '{col_name}'")
    sample_data = df[col_name].dropna().head(5).tolist()
    
    prompt = f"""
    Look at this sample of time/date data from the column '{col_name}'.
    Data sample: {sample_data}
    
    Determine the appropriate standardized format for this data based on its granularity.
    - Hours and minutes: "%H:%M"
    - Hours, minutes, and seconds: "%H:%M:%S"
    - Just seconds: "%S"
    - Specific dates: "%d/%m/%Y"
    - Date and time: "%d/%m/%Y %H:%M"
    - Date and exact time: "%d/%m/%Y %H:%M:%S"
    - Month and year: "%m/%Y"
    - Year only: "%Y"
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are an expert data formatting agent."},
            {"role": "user", "content": prompt}
        ],
        response_format=TimeFormatDecision
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [Time Agent Decision] {decision.reasoning}")
    
    df = execute_time_formatting(df, col_name, decision.target_format)
    return df






def money_agent_workflow(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"  -> [Money Agent] Taking control of column: '{col_name}'")
    sample_data = df[col_name].dropna().head(10).tolist()
    
    prompt = f"""
    Look at this sample of financial data from the column '{col_name}'.
    Data sample: {sample_data}
    
    Your task:
    1. Identify the primary currency being used (e.g., $, USD, €, Yen, "dollars", "euros"). 
       - CRITICAL RULE: If a currency is specified even just once in the sample, and NO OTHER currencies are mentioned, assume that single currency applies to the entire column.
    2. Set `is_mixed_currency` to True ONLY if you see multiple DIFFERENT currencies (e.g., "dollars" in one row and "eur" in another).
    3. Determine the best scale ("None", "Thousands", "Millions", "Billions").
       - Evaluate the TRUE underlying numerical value. "100 million" means 100,000,000. 
       - If the true values are predominantly in the millions, you MUST choose "Millions".
    4. Identify the decimal separator used in the numbers ("." or ",").
       - WARNING: Commas that group thousands (like "200,000,000") are NOT decimal separators. If a comma groups thousands, the decimal separator is ".".
       - Only choose "," if the comma specifically separates fractional cents at the very end of the number (e.g., "1.500,00").
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are a precise financial data standardization agent."},
            {"role": "user", "content": prompt}
        ],
        response_format=MoneyFormatDecision
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [Money Agent Decision] Mixed: {decision.is_mixed_currency} | Currency: {decision.detected_currency} | Scale: {decision.scale_decision}")
    
    df = execute_money_formatting(df, col_name, decision)
    return df




def int_agent_workflow(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"  -> [Int Agent] Taking control of column: '{col_name}' (Bypassing LLM for deterministic math)")
    
    df = execute_int_formatting(df, col_name)
    return df




def float_agent_workflow(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"  -> [Float Agent] Taking control of column: '{col_name}' (Bypassing LLM)")
    
    df = execute_float_formatting(df, col_name)
    return df




def na_agent_workflow(df: pd.DataFrame) -> pd.DataFrame:
    print(f"\n[NA Agent] Scanning dataframe for custom missing values, empty rows, and empty columns...")
    
    # Pre-scan the data for 1-2 character strings that are pure punctuation
    potential_nas = set()
    for col in df.columns:
        str_vals = df[col].dropna().astype(str)
        for val in str_vals:
            val = val.strip()
            if 0 < len(val) <= 2 and all(c in string.punctuation for c in val):
                potential_nas.add(val)
                
    sample_data = df.head(10).to_dict(orient="records")
    
    prompt = f"""
    Analyze this dataset sample to identify how missing data is represented.
    Pandas has already handled standard 'NaN' and 'N/A' automatically.
    
    However, we detected these specific punctuation-only strings in the dataset: {list(potential_nas)}
    
    Your task:
    1. Evaluate if any of these strings (like "-", ".") are being used as placeholders for missing data. If so, add them to `custom_na_strings_to_wipe`.
    2. Decide if completely empty rows (rows where every single column is missing) should be removed. For standard tables, this is usually True.
    3. Decide if completely empty columns (columns where every single row is missing) should be removed. For standard tables, this is usually True.
    
    Data Sample:
    {sample_data}
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are a data cleaning agent focused on missing values."},
            {"role": "user", "content": prompt}
        ],
        response_format=MissingDataDecision
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [NA Agent Decision] Wipe strings: {decision.custom_na_strings_to_wipe} | Drop rows: {decision.remove_completely_empty_rows} | Drop cols: {decision.remove_completely_empty_columns}")
    
    df = execute_missing_data_cleaning(df, decision)
    return df


def name_agent_workflow(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"  -> [Name Agent] Taking control of column: '{col_name}'")
    
    # Grab 10 rows to give the LLM enough pattern context
    sample_data = df[col_name].dropna().head(10).tolist()
    
    prompt = f"""
    Look at this sample of proper nouns from the column '{col_name}'.
    Data sample: {sample_data}
    
    Your task:
    1. Determine if this column primarily contains "Human Names" or "Locations/Other" (like cities, states, companies).
    2. If it is "Human Names", deduce the dominant structural format.
       - Are they mostly "First Last" (e.g., John Smith)?
       - Are they mostly "Last First" (e.g., Smith John)?
       - NOTE: If you see ambiguous names (like "Harper Taylor"), look at the other names in the sample to deduce the pattern.
    3. If it is "Locations/Other", select "N/A" for the format.
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are a precise text standardization agent."},
            {"role": "user", "content": prompt}
        ],
        response_format=NameFormatDecision
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [Name Agent Decision] Type: {decision.entity_type} | Dominant Format: {decision.dominant_format}")
    
    df = execute_name_formatting(df, col_name, decision)
    return df


def header_detection_agent(file_path: str):
    print(f"\n[Header Agent] Scanning '{file_path}' for the true table coordinates...")
    
    try:
        if file_path.endswith('.csv'):
            df_raw = pd.read_csv(file_path, header=None, nrows=15)
        else:
            df_raw = pd.read_excel(file_path, header=None, nrows=15)
    except Exception as e:
        print(f"Error reading file for header detection: {e}")
        return 0, 0

    # Fill NaN values with empty strings so the LLM can easily see the "blank" cells
    df_raw = df_raw.fillna("")
    raw_sample = df_raw.to_dict(orient="records")
    
    prompt = f"""
    Look at the first 15 rows of this raw data file.
    Real-world files often have titles, export dates, or blank rows at the very top. They also frequently have blank columns on the left.
    
    Your task is to identify the 2D starting coordinate of the ACTUAL data table:
    1. `header_row_index`: The 0-based index of the row containing the column headers (e.g., 'Txn ID', 'Date', 'Amount').
    2. `header_col_index`: The 0-based index of the column where the actual data starts (ignoring empty/blank columns to the left).
    
    Raw data sample: {raw_sample}
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are a data parsing agent specialized in finding table structures."},
            {"role": "user", "content": prompt}
        ],
        response_format=HeaderDecision
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [Header Agent Decision] Table starts at Row {decision.header_row_index}, Column {decision.header_col_index}.")
    print(f"     [Reasoning] {decision.reasoning}")
    
    return decision.header_row_index, decision.header_col_index


def dataset_description_agent(df: pd.DataFrame) -> pd.DataFrame:
    print(f"\n  -> [Description Agent] Analyzing the final dataset to generate feature documentation...")
    
    # Grab a sample of the cleaned data
    sample_data = df.head(5).to_dict(orient="list")
    
    prompt = f"""
    Analyze the following sample of a cleaned dataset.
    Your task is to generate a comprehensive data dictionary.
    
    1. Provide a 1-2 sentence `general_summary` of what this dataset represents.
    2. For every single column in the dataset, create a `FeatureDescription` detailing:
       - The exact column name.
       - The conceptual data type (e.g., Categorical, Datetime, Continuous Numeric, Text).
       - A clear, concise description of what the data represents based on the column name and the values.
    
    Data Sample:
    {sample_data}
    """

    response = client.beta.chat.completions.parse(
        model="gpt-4o-2024-08-06",
        messages=[
            {"role": "system", "content": "You are an expert data analyst and documentation agent."},
            {"role": "user", "content": prompt}
        ],
        response_format=DatasetDescription
    )
    
    decision = response.choices[0].message.parsed
    print(f"     [Description Agent] Summary: {decision.general_summary}")
    
    # Convert the LLM's Pydantic objects into a list of dictionaries for Pandas
    description_records = []
    for f in decision.features:
        description_records.append({
            "Feature Name": f.feature_name,
            "Conceptual Data Type": f.inferred_data_type,
            "Description": f.description
        })
        
    # Create the DataFrame for the new Excel sheet
    description_df = pd.DataFrame(description_records)
    print(f"     [Description Agent] Successfully generated dictionary for {len(description_records)} features.")
    
    return description_df