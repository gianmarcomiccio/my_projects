import pandas as pd
from agents import header_detection_agent, na_agent_workflow, reader_agent
from orchestrator import orchestrator_router

if __name__ == "__main__":
    test_file = "case_A1_sales_light_dirty_input.xlsx"
    
    print("--- STARTING AGENTIC PIPELINE ---")
    
    # 1. SCOUT
    header_row, header_col = header_detection_agent(test_file)
    
    # 2. LOAD
    print(f"\n[Orchestrator] Loading file starting at row {header_row}...")
    if test_file.endswith('.csv'):
        df = pd.read_csv(test_file, header=header_row)
    else:
        df = pd.read_excel(test_file, header=header_row)
        
    # 3. CROP
    if header_col > 0:
        print(f"[Orchestrator] Cropping {header_col} empty columns from the left...")
        df = df.iloc[:, header_col:]
        
    # 4. SWEEP
    df = na_agent_workflow(df)
    
    # 5. READ
    classified_types = reader_agent(df)
    
    # 6. ROUTE
    orchestrator_router(df, classified_types, test_file)