"""Minimal example: read columns from the frankenbrain SeaTable base
(``cns_meta``) via SQL.

Same shape as ``read_from_banc_seatable.py`` but targets the
cross-dataset ``cns_meta`` base, which holds the BANC ↔ FAFB ↔ MANC
cell-type bridge ("franken meta"). Useful when you need the
cross-dataset metadata from Python; most of the analysis pipeline gets
it via ``franken_meta()`` in R.

Set up:
    export BANCTABLE_TOKEN=<your personal token>
"""

# Load libraries
import seatable_api
import os
import numpy as np
import pandas as pd

# Configuration seatable variables
server_url = "https://cloud.seatable.io/"  # Replace with your SeaTable server URL
api_token = os.getenv('BANCTABLE_TOKEN')  # Replace with your API token, save it and export it as a system variable called BANCTABLE_TOKEN get it here: https://api.seatable.io/reference/getaccounttokenfromusername
workspace_id = "57832"                     # Your workspace ID, same or everyone on project
base_name="cns_meta"  # You may want banc_meta instead for WIP BANC neuron annotations

# Login to seatable
ac=seatable_api.Account(login_name=[],password=[],server_url=server_url)
ac.token=api_token

# Initialize the Base object
base=ac.get_base(workspace_id=workspace_id,base_name=base_name)
base.auth()

# Execute the SQL query to retrieve data
query = "SELECT id, flow, super_class, cell_class, cell_sub_class, modality, seed, cell_type FROM franken_meta"
query_results = base.query(query)

# Convert results to a pandas DataFrame
df = pd.DataFrame(query_results)  # Adjust 'results' based on the API response
