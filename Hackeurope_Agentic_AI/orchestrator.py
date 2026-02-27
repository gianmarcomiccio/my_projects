import pandas as pd
from agents import (
    time_agent_workflow, money_agent_workflow, int_agent_workflow, 
    float_agent_workflow, name_agent_workflow, dataset_description_agent
)

def orchestrator_router(df: pd.DataFrame, type_vector: list[str], original_filename: str):
    print(f"\n[Orchestrator] Data loaded. Delegating tasks...")
    # REMOVED: df = pd.read_excel(file_path) because df is already loaded and cropped!
    
    for col_name, col_type in zip(df.columns, type_vector):
        if col_type == "time":
            df = time_agent_workflow(df, col_name)
        elif col_type == "money":
            df = money_agent_workflow(df, col_name) 
        elif col_type == "int":
            df = int_agent_workflow(df, col_name)
        elif col_type == "float":
            # Added float logic here just in case you need it!
            df = float_agent_workflow(df, col_name)
        elif col_type == "name":
            df = name_agent_workflow(df, col_name)
        elif col_type in ["string", "unknown"]:
            print(f"  -> [Orchestrator] Bypassing '{col_name}' (Type: {col_type} requires no formatting)")

    desc_df = dataset_description_agent(df)
    
    # Save the file using pd.ExcelWriter to support multiple tabs
    output_path = "cleaned_" + original_filename
    
    # We use engine='openpyxl' to ensure it writes modern .xlsx files properly
    with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
        # Save the actual data to the first sheet
        df.to_excel(writer, sheet_name="Cleaned_Data", index=False)
        
        # Save the LLM's descriptions to the second sheet
        desc_df.to_excel(writer, sheet_name="dataset_description", index=False)
        
    print(f"\n[Orchestrator] All tasks complete. Saved multi-sheet file to: {output_path}")
