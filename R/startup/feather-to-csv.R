library(reticulate)

# Specify the path to your feather files
feather_dir <- "data/schlegel_2024_fafb_neuron_ranks"

# Python code as a string
python_code <- '
import os
import pandas as pd

# Get the directory path from R
feather_dir = r.feather_dir

# Loop through all files in the directory
for filename in os.listdir(feather_dir):
    if filename.endswith(".feather"):
        # Construct full file paths
        feather_path = os.path.join(feather_dir, filename)
        csv_path = os.path.join(feather_dir, filename.replace(".feather", ".csv"))
        
        # Read the feather file
        df = pd.read_feather(feather_path)
        
        # Save as CSV
        df.to_csv(csv_path, index=False)
        print(f"Converted {filename} to CSV")
'

# Execute the Python code
py_run_string(python_code)

print("All feather files have been converted to CSV.")