"""Minimal example: read columns from the BANC SeaTable base via SQL.

Shows how to authenticate with the SeaTable Python API
(``seatable_api``) using a personal token stored in the
``BANCTABLE_TOKEN`` environment variable, point at the BANC workspace
+ base, and run a SQL ``SELECT`` to pull a column subset. Useful as a
starting point if you need Python-side SeaTable access; most of the
analysis pipeline accesses BANC metadata via the R client
(``bancr::banctable_query()``) instead.

Set up:
    export BANCTABLE_TOKEN=<your personal token>
    (per-user; create at
     https://api.seatable.io/reference/getaccounttokenfromusername)
"""

# Load libraries
import seatable_api
import os
import numpy as np
import pandas as pd

# Configuration seatable variables
server_url = "https://cloud.seatable.io/"  # Replace with your SeaTable server URL
api_token = os.getenv('BANCTABLE_TOKEN')  # Replace with your API token, save it and export it as a system variable called BANCTABLE_TOKEN, get it here: https://api.seatable.io/reference/getaccounttokenfromusername
workspace_id = "57832"                     # Your workspace ID, same or everyone on project
base_name="banc_meta"  # You may want banc_meta instead for WIP BANC neuron annotations

# Login to seatable
ac=seatable_api.Account(login_name=[],password=[],server_url=server_url)
ac.token=api_token

# Initialize the Base object
base=ac.get_base(workspace_id=workspace_id,base_name=base_name)
base.auth()

# Execute the SQL query to retrieve data
query = "SELECT root_id, supervoxel_id, position, proofread, flow, super_class, cell_class, cell_type FROM banc_meta"
query_results = base.query(query)

# Convert results to a pandas DataFrame
df = pd.DataFrame(query_results)  # Adjust 'results' based on the API response

# Filter the DataFrame for rows where 'proofread' is True, for banc_meta really
# filtered_df = df[df['proofread'] == True]
filtered_df = df

# Display the filtered DataFrame
print(filtered_df)

# Save as franken_meta_[date].csv, dated by today's date
from datetime import datetime
today = datetime.today().strftime('%Y%m%d')

# Construct the full file path
output_filename = f"franken_meta_{today}.csv"
output_path = os.path.join("/Users/papers/BANC-project/data/meta", output_filename)

# Create the directory if it doesn't exist
os.makedirs(os.path.dirname(output_path), exist_ok=True)

# Save the DataFrame to CSV
filtered_df.to_csv(output_path, index=False)
print(f"Data saved to: {output_path}")
