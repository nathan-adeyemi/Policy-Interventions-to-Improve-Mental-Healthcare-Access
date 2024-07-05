import pandas as pd
import os

file_list = ["Results/validation/acceptance-probs",
             "Results/validation/acceptance-probs-Coord",
             "Results/validation/acceptance-probs-norm"]

valid_files = []
for file in file_list:
    # Define the path to the Excel file
    
    if file == "Results/validation/acceptance-probs":
        column = 'HCCIS-admissions'
    elif file == "Results/validation/acceptance-probs-Coord":
        column = "Median-Coordination-Times"
    elif file == "Results/validation/acceptance-probs-norm":
        column = "Min-Max-Normalized-Median-Coordination-and-Admissions"
        
    file_path = os.path.join(file,'trainable-Incumbent/validation_frames.xlsx')
    # Read the Excel file
    xls = pd.ExcelFile(file_path)

    # Initialize an empty list to store DataFrames
    df_list = []

    # Loop through each sheet name
    for sheet_name in xls.sheet_names:
        # Read the sheet into a DataFrame
        df = pd.read_excel(xls, sheet_name=sheet_name)
        # Add a column for the sheet name
        df['validation-metric-group'] = sheet_name
        # Append the DataFrame to the list
        df_list.append(df)

    # Concatenate all DataFrames into a single DataFrame
    combined_df = pd.concat(df_list, ignore_index=True)
    combined_df['RMSE-Tuned-Metric'] = column

    # Display the combined DataFrame
    valid_files.append(combined_df)
    
valid_files = pd.concat([valid_files])
valid_files.to_csv("Results/validation/combined_incumbents")
