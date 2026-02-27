import pandas as pd
import dateparser
import re
from schemas import MoneyFormatDecision, NameFormatDecision, MissingDataDecision

# Paste these exact functions from your original code:
# - execute_time_formatting
# - execute_money_formatting
# - execute_int_formatting
# - execute_float_formatting
# - execute_name_formatting
# - execute_missing_data_cleaning


def execute_time_formatting(df: pd.DataFrame, col_name: str, target_format: str) -> pd.DataFrame:
    """The Tool used to physically alter the dataframe."""
    print(f"       [Tool Executing] Formatting '{col_name}' to '{target_format}'...")
    
    def parse_natural_language(date_str):
        if pd.isna(date_str):
            return pd.NaT
            
        clean_str = str(date_str).lower()
        replacements = {
            "first": "1st", "second": "2nd", "third": "3rd", 
            "fourth": "4th", "fifth": "5th", "sixth": "6th", 
            "seventh": "7th", "eighth": "8th", "ninth": "9th", 
            "tenth": "10th", "eleventh": "11th", "twelfth": "12th", 
            "thirteenth": "13th", "fourteenth": "14th", "fifteenth": "15th", 
            "sixteenth": "16th", "seventeenth": "17th", "eighteenth": "18th", 
            "nineteenth": "19th", "twentieth": "20th",
            "twenty-first": "21st", "twenty first": "21st",
            "twenty-second": "22nd", "twenty second": "22nd",
            "twenty-third": "23rd", "twenty third": "23rd",
            "twenty-fourth": "24th", "twenty fourth": "24th",
            "twenty-fifth": "25th", "twenty fifth": "25th",
            "twenty-sixth": "26th", "twenty sixth": "26th",
            "twenty-seventh": "27th", "twenty seventh": "27th",
            "twenty-eighth": "28th", "twenty eighth": "28th",
            "twenty-ninth": "29th", "twenty ninth": "29th",
            "thirtieth": "30th", 
            "thirty-first": "31st", "thirty first": "31st",
            "last": "last"
        }
        for word, num in replacements.items():
            clean_str = clean_str.replace(word, num)
            
        parsed = dateparser.parse(clean_str)
        return parsed if parsed else pd.NaT

    try:
        df[col_name] = df[col_name].apply(parse_natural_language)
        df[col_name] = df[col_name].dt.strftime(target_format)
        print(f"       [Tool Success] Column updated.")
    except Exception as e:
        print(f"       [Tool Error] Failed: {e}")
    return df






def execute_money_formatting(df: pd.DataFrame, col_name: str, decision: MoneyFormatDecision) -> pd.DataFrame:
    print(f"       [Tool Executing] Scale: {decision.scale_decision}, Mixed Currency: {decision.is_mixed_currency}...")
    
    def parse_money_string(val):
        if pd.isna(val):
            return pd.NA, ""
            
        val_str = str(val).lower().strip()
        original_str = str(val).strip() 
        
        # 1. Extract the currency symbol, code, or full word
        symbol_match = re.search(r'([\$€£¥]|(?:usd|eur|gbp|jpy|dollars?|euros?|pounds?|yen))', original_str, re.IGNORECASE)
        raw_symbol = symbol_match.group(1).lower() if symbol_match else ""
        
        # 2. Normalize the currency
        currency_map = {
            "dollar": "USD", "dollars": "USD", "$": "USD", "usd": "USD",
            "euro": "EUR", "euros": "EUR", "eur": "EUR", "€": "EUR",
            "pound": "GBP", "pounds": "GBP", "gbp": "GBP", "£": "GBP",
            "yen": "JPY", "jpy": "JPY", "¥": "JPY"
        }
        symbol = currency_map.get(raw_symbol, raw_symbol.upper())
        
        # 3. Handle International Decimals
        if decision.decimal_separator == ",":
            val_str = val_str.replace('.', '').replace(',', '.')
        else:
            val_str = val_str.replace(',', '')
            
        # DEFENSIVE SHIELD: Remove extra dots
        if val_str.count('.') > 1:
            parts = val_str.rsplit('.', 1)
            val_str = parts[0].replace('.', '') + '.' + parts[1]
            
        # 4. Extract the core number
        match = re.search(r'[\d\.]+', val_str)
        if not match:
            return pd.NA, symbol
        try:
            num = float(match.group())
        except ValueError:
            return pd.NA, symbol
            
        # 5. Apply word multipliers
        isolated_words = re.sub(r'[\d\.\,€\$£¥]', ' ', val_str).split()
        
        if any(w in isolated_words for w in ['billion', 'billions', 'bill', 'bil', 'b']):
            num *= 1_000_000_000
        elif any(w in isolated_words for w in ['million', 'millions', 'mill', 'mil', 'm']):
            num *= 1_000_000
        elif any(w in isolated_words for w in ['thousand', 'thousands', 'k']):
            num *= 1_000
        elif any(w in isolated_words for w in ['cent', 'cents']):
            num /= 100
            
        return num, symbol

    try:
        parsed_data = df[col_name].apply(parse_money_string)
        
        nums = [x[0] if isinstance(x, tuple) else pd.NA for x in parsed_data]
        symbols = [x[1] if isinstance(x, tuple) else "" for x in parsed_data]
        
        # Overwrite the original column with just the pure math numbers
        df[col_name] = nums
        
        # 6. Apply the Scale Decision to the numbers
        scale_suffix = ""
        if decision.scale_decision == "Billions":
            df[col_name] = df[col_name] / 1_000_000_000
            scale_suffix = "in billions"
        elif decision.scale_decision == "Millions":
            df[col_name] = df[col_name] / 1_000_000
            scale_suffix = "in millions"
        elif decision.scale_decision == "Thousands":
            df[col_name] = df[col_name] / 1_000
            scale_suffix = "in thousands"

        # 7. Final Formatting: Split column vs Single Currency Header
        if decision.is_mixed_currency:
            # Find exactly where the current column is located
            col_idx = df.columns.get_loc(col_name)
            
            # Create the new column name
            new_currency_col = f"{col_name}_currency"
            
            # Insert the symbols list as a new column directly to the right
            df.insert(loc=col_idx + 1, column=new_currency_col, value=symbols)
            
            # Rename original number column if scaling was applied
            if scale_suffix:
                new_col_name = f"{col_name} ({scale_suffix})"
                df.rename(columns={col_name: new_col_name}, inplace=True)
                print(f"       [Tool Success] Mixed currencies split into '{new_currency_col}'. Numbers renamed to '{new_col_name}'.")
            else:
                print(f"       [Tool Success] Mixed currencies split into '{new_currency_col}'.")
                
        else:
            # Single currency: Keep as floats, put currency in the header, no new column needed
            parts = []
            if decision.detected_currency and decision.detected_currency != "Unknown":
                parts.append(decision.detected_currency)
            if scale_suffix:
                parts.append(scale_suffix)
                
            if parts:
                header_addition = " ".join(parts)
                new_col_name = f"{col_name} ({header_addition})"
                df.rename(columns={col_name: new_col_name}, inplace=True)
                print(f"       [Tool Success] Floats extracted. Renamed to '{new_col_name}'.")

    except Exception as e:
        print(f"       [Tool Error] Failed: {e}")
        
    return df



def execute_float_formatting(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"       [Tool Executing] Standardizing floats for '{col_name}'...")
    
    # 1. Clean the data and convert to pure floats
    def extract_float(val):
        if pd.isna(val):
            return pd.NA
        val_str = str(val).lower().replace(',', '').strip()
        try:
            return float(val_str)
        except ValueError:
            return pd.NA
            
    raw_floats = df[col_name].apply(extract_float)
    
    # 2. Determine the maximum number of decimal places in the column
    max_decimals = 0
    for val in raw_floats.dropna():
        # Convert float to string (e.g., 0.876 -> "0.876") and split at the dot
        parts = str(val).split('.')
        if len(parts) == 2:
            decimals = len(parts[1])
            if max_decimals < decimals:
                max_decimals = decimals
                
    # 3. Format every number to match the max_decimals length
    def pad_float(val):
        if pd.isna(val):
            return pd.NA
        # This dynamically creates a format rule like "{:.3f}"
        return f"{val:.{max_decimals}f}"
        
    df[col_name] = raw_floats.apply(pad_float)
    
    print(f"       [Tool Success] Floats standardized to {max_decimals} decimal places.")
    return df


def execute_name_formatting(df: pd.DataFrame, col_name: str, decision: NameFormatDecision) -> pd.DataFrame:
    print(f"       [Tool Executing] Cleaning names. Type: {decision.entity_type}, Format: {decision.dominant_format}...")
    
    def parse_name(val):
        if pd.isna(val):
            return pd.NA
            
        # 1. Standardize capitalization (e.g., "JOHN smith" -> "John Smith")
        clean_name = str(val).strip().title()
        
        # 2. If it's a Location/Other, we just return the title-cased string
        if decision.entity_type == "Locations/Other":
            return clean_name
            
        # 3. Handle Human Names
        # If there's a comma (e.g., "Smith, John"), split it and force "First Last"
        if "," in clean_name:
            parts = [p.strip() for p in clean_name.split(",")]
            if len(parts) == 2:
                return f"{parts[1]} {parts[0]}"
                
        # If the LLM determined the column is mostly "Last First" without commas (e.g., "Smith John")
        if decision.dominant_format == "Last First":
            parts = clean_name.split()
            if len(parts) == 2:
                # Flip it to "First Last"
                return f"{parts[1]} {parts[0]}"
                
        # Default fallback: return as-is (already title-cased)
        return clean_name

    try:
        df[col_name] = df[col_name].apply(parse_name)
        print(f"       [Tool Success] Column '{col_name}' standardized.")
    except Exception as e:
        print(f"       [Tool Error] Failed to process names: {e}")
        
    return df


def execute_int_formatting(df: pd.DataFrame, col_name: str) -> pd.DataFrame:
    print(f"       [Tool Executing] Cleaning and truncating '{col_name}' to integers...")
    
    def parse_int(val):
        if pd.isna(val):
            return pd.NA
            
        val_str = str(val).lower().replace(',', '').strip()
        
        try:
            num = float(val_str)
            return int(num)
        except ValueError:
            return pd.NA

    try:
        df[col_name] = df[col_name].apply(parse_int)
        df[col_name] = df[col_name].astype('Int64')
        
        print(f"       [Tool Success] Column '{col_name}' safely truncated to integers.")
    except Exception as e:
        print(f"       [Tool Error] Failed to process integers: {e}")
        
    return df



def execute_missing_data_cleaning(df: pd.DataFrame, decision: MissingDataDecision) -> pd.DataFrame:
    print(f"       [Tool Executing] Cleaning custom NAs, empty rows, and empty columns...")
    try:
        # 1. Wipe custom NA strings
        if decision.custom_na_strings_to_wipe:
            def wipe_custom_na(val):
                if isinstance(val, str) and val.strip() in decision.custom_na_strings_to_wipe:
                    return pd.NA
                return val
            
            df = df.map(wipe_custom_na)
            print(f"       [Tool Success] Wiped strings: {decision.custom_na_strings_to_wipe}")
        
        # 2. Remove completely empty rows
        if decision.remove_completely_empty_rows:
            initial_rows = len(df)
            df = df.dropna(axis=0, how='all')
            rows_removed = initial_rows - len(df)
            if rows_removed > 0:
                print(f"       [Tool Success] Dropped {rows_removed} completely empty rows.")
                
        # 3. Remove completely empty columns
        if decision.remove_completely_empty_columns:
            initial_cols = len(df.columns)
            # axis=1 tells Pandas to drop columns instead of rows
            df = df.dropna(axis=1, how='all')
            cols_removed = initial_cols - len(df.columns)
            if cols_removed > 0:
                print(f"       [Tool Success] Dropped {cols_removed} completely empty columns.")
            
    except Exception as e:
        print(f"       [Tool Error] Failed to clean missing data: {e}")
        
    return df