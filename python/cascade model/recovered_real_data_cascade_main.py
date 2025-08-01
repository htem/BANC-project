
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sqlite3
import sys
import os
os.chdir('/home/jif564/bancpipeline/analysis/python/')
import cascade_model
#from cascade_model import SignalCascade
import json
import argparse
import re
import pickle


def load_sqlite_database(sql_path):
    """
    Load metadata and connectivity data from SQLite database.
    
    Parameters:
    -----------
    sql_path : str
        Path to SQLite database
        
    Returns:
    --------
    tuple
        (metadata_df, edgelist_df)
    """
    conn = sqlite3.connect(sql_path)
    
    # Load all tables
    meta_df = pd.read_sql_query("SELECT * FROM meta", conn)
    try:
        edgelist_df = pd.read_sql_query("SELECT * FROM edgelist_simple", conn)
    except Exception as e:
        print("Failed to load 'edgelist_simple'. Attempting to load 'edgelist' instead.")
        edgelist_df = pd.read_sql_query("SELECT * FROM edgelist", conn)
    conn.close()
    return meta_df, edgelist_df


def filter_edgelist_and_update_meta(
    edgelist_df, 
    meta_df, 
    synaptic_threshold=25
):
    """
    Filter edgelist DataFrame based on synaptic count threshold.
    Update meta_df to remove neurons without any connections and append the 'top_nt' column
    from meta_df to edgelist_df based on 'pre' and 'id'.

    Parameters:
    -----------
    edgelist_df : DataFrame
        DataFrame containing the edgelist with columns ['pre', 'post', 'count', ...].
    meta_df : DataFrame
        Metadata DataFrame containing neuron information with an 'id' column and a 'top_nt' column.
    synaptic_threshold : int, optional
        Minimum total synaptic count to retain a connection. Default is 25.

    Returns:
    --------
    tuple
        (filtered_edgelist, updated_meta, filtered_out_neurons)
    """
    # Group by 'pre' and 'post' and sum synaptic counts
    grouped_edgelist = edgelist_df.groupby(['pre', 'post'], as_index=False).agg(synaptic_count=('count', 'sum'))
    filtered_pairs = grouped_edgelist[grouped_edgelist['synaptic_count'] > synaptic_threshold][['pre', 'post']]

    # Retain only rows in the original edgelist that match the filtered pairs
    filtered_edgelist = edgelist_df.merge(filtered_pairs, on=['pre', 'post'], how='inner')

    # Append 'top_nt' from meta_df to edgelist_df
    nt_mapping = meta_df.set_index('id')['top_nt'].to_dict()
    filtered_edgelist['top_nt'] = filtered_edgelist['pre'].map(nt_mapping)

    # Determine neurons remaining in the filtered edgelist
    remaining_neurons = set(filtered_edgelist['pre']).union(set(filtered_edgelist['post']))

    # Update the meta DataFrame to remove neurons without connections
    updated_meta = meta_df[meta_df['id'].isin(remaining_neurons)].copy()

    # Return the filtered edgelist, updated metadata, and the neurons that were filtered out
    filtered_out_neurons = set(meta_df['id']) - remaining_neurons

    return filtered_edgelist, updated_meta, filtered_out_neurons



def run_cascade_analysis(meta_df, edgelist_df, mask_A, mask_B, is_empty_B, max_timesteps, activation_threshold, n_iterations, version="v2"):
    """
    Run the cascade analysis using the new class implementation.
    
    Parameters:
        meta_df (pd.DataFrame): Metadata containing neuron information.
        edgelist_df (pd.DataFrame): Edge list for neuron connectivity.
        mask_A (pd.Series): Boolean mask for start neurons.
        mask_B (pd.Series): Boolean mask for end neurons.
        max_timesteps (int): Maximum number of timesteps for the cascade.
        activation_threshold (float): Activation threshold for the cascade.
        n_iterations (int): Number of iterations for the cascade simulation.
        
    Returns:
        dict: Results of the cascade analysis.
    """
    print(f"Running cascade analysis from {mask_A.sum()} neurons to {mask_B.sum()} neurons using the new implementation.")

    # Initialize the cascade simulation object
    cascade_simulation = cascade_model.SignalCascade(activation_threshold=activation_threshold, n_iterations=n_iterations,max_timesteps=max_timesteps)

    # Extract start and end neurons
    start_neurons = set(meta_df[mask_A]['id'])
    #print(len(start_neurons))
    end_neurons = set(meta_df[mask_B]['id'])
    if is_empty_B == "True":
        end_neurons = set()
    if version == "v1":
        raise NotImplementedError("The 'v1' version of the cascade analysis is not yet implemented.")
    # Run the cascade simulation
    elif version == "v2":
        result_dict = cascade_simulation.run_cascade_v2(
            S_A0=start_neurons,
            S_E=end_neurons,
            edgelist=edgelist_df,
        )
    return result_dict


def process_filtered_edgelist_by_nt(filtered_edgelist, invert_nts=None):
    """
    Process the filtered edgelist by adding a new column 'effective_count',
    which adjusts the 'count' values based on specific neurotransmitters in
    the 'pre_top_nt' column.

    Parameters:
    -----------
    filtered_edgelist : DataFrame
        The edgelist DataFrame with columns including 'pre_top_nt' and 'count'.
    invert_nts : list of str, optional
        List of neurotransmitters for which to invert the 'count' sign.
        Default is ['gaba', 'glutamate'].

    Returns:
    --------
    processed_edgelist : DataFrame
        The modified edgelist with a new column 'effective_count'.
    """
    if invert_nts is None:
        invert_nts = ['gaba', 'glutamate']  # Default neurotransmitters to invert

    # Initialize 'effective_count' as a copy of 'count'
    filtered_edgelist['effective_count'] = filtered_edgelist['count']

    # Process each neurotransmitter in the invert list
    for nt in invert_nts:
        filtered_edgelist.loc[
            filtered_edgelist.top_nt.str.contains(nt, case=False, na=False), 
            "effective_count"
        ] *= -1

    return filtered_edgelist



def save_results_to_pickle(result_dict, file_path):
    """
    Save the result dictionary to a pickle file.
    
    Parameters:
    -----------
    result_dict : dict
        Dictionary containing the results to be saved.
    file_path : str
        Path to save the pickle file.
    """
    with open(file_path, 'wb') as f:
        pickle.dump(result_dict, f)

def load_results_from_pickle(file_path):
    """
    Load a result dictionary from a pickle file.

    Parameters:
    -----------
    file_path : str
        Path to the pickle file to be loaded.

    Returns:
    --------
    result_dict : dict
        Dictionary containing the loaded results.
    """
    with open(file_path, 'rb') as f:
        result_dict = pickle.load(f)
    return result_dict

def extract_mask(meta_df, mask_key, mask_keyword):
    """
    Extract a mask (boolean filter) for neurons based on a specified key and keyword.
    
    Parameters:
    -----------
    meta_df : DataFrame
        Metadata DataFrame containing neuron information.
    mask_key : str
        Column name in the DataFrame to apply the mask (e.g., 'type', 'id').
    mask_keyword : str
        String or regular expression to filter rows (e.g., neuron type or body IDs).
    
    Returns:
    --------
    mask : Series
        Boolean mask for the filtered rows.
    """
    if mask_key == "id":
        # Split body IDs by commas and check for inclusion
        mask = meta_df[mask_key].astype(str).isin(mask_keyword.split(","))
    if mask_key == "super_class" or "cell_type":
        # Use regex matching for other string-based columns
        mask = meta_df[mask_key].str.contains(mask_keyword, case=False, na=False)
    
    return mask






def main():
    parser = argparse.ArgumentParser(description="Run a cascade analysis with specified parameters.")
    
    # Input arguments with default options
    parser.add_argument(
        "--sql_path", 
        type=str, 
        default="/n/data1/hms/neurobio/wilson/banc/connectivity/frankenbrain_v.1.1_data.sqlite",
        help="Path to the SQLite database. Default: '/n/data1/hms/neurobio/wilson/banc/connectivity/frankenbrain_v.1.1_data.sqlite'."
    )
    parser.add_argument(
        "--synaptic_threshold", 
        type=int, 
        default=25, 
        help="Synaptic count threshold for filtering. Default: 25."
    )
    parser.add_argument(
        "--mask_A", 
        type=str, 
        default="MBON09", 
        help="String to define mask A (e.g., neuron type). Default: 'MBON09'."
    )
    parser.add_argument(
        "--mask_B", 
        type=str, 
        default=r"FB|hdelta|vdelta", 
        help="String to define mask B (e.g., neuron type). Default: 'FB|hdelta|vdelta'."
    )
    parser.add_argument("--mask_key_A", type=str, default="cell_type", help="Column for Mask A in the metadata DataFrame.")
    parser.add_argument("--mask_key_B", type=str, default="super_class", help="Column for Mask B in the metadata DataFrame.")
    parser.add_argument("--empty_B", type=str, default="True", help="Whether to use an empty mask B.")
    parser.add_argument("--cascade_version", type=str, default="v2", help="Version of cascade model.")
    parser.add_argument("--activation_threshold", type=float, default=0.01, help="Activation threshold for the cascade.")
    parser.add_argument("--n_iterations", type=int, default=100, help="Number of iterations for the cascade simulation.")
    parser.add_argument(
        "--max_timesteps", 
        type=int, 
        default=4, 
        help="Maximum timestep to run in cascade model. Default: 4."
    )
    parser.add_argument(
    "--output_dir", 
    type=str, 
    default=None,  # Set to None to dynamically define later
    help="Directory to save the output JSON file. Default: current directory."
    )

    args = parser.parse_args()
    # Dynamically set the output directory based on cascade_version
    if args.output_dir is None:
        args.output_dir = f"cascade_results"

    # Load the SQLite database
    meta_df, edgelist_df = load_sqlite_database(args.sql_path)
    
    # Filter metadata and edgelist
    filtered_edgelist, filtered_meta, _ = filter_edgelist_and_update_meta(
        edgelist_df, meta_df, args.synaptic_threshold
    )
    filtered_edgelist = process_filtered_edgelist_by_nt(filtered_edgelist)
    # Extract masks for A and B
    mask_A = extract_mask(filtered_meta, args.mask_key_A, args.mask_A)
    mask_B = extract_mask(filtered_meta, args.mask_key_B, args.mask_B)

    # Run cascade analysis
    result_dict = run_cascade_analysis(filtered_meta, filtered_edgelist, mask_A, mask_B, args.empty_B, args.max_timesteps, args.activation_threshold, args.n_iterations, args.cascade_version)

    # Generate output filename
    mask_A_clean = re.sub(r"\W+", "_", args.mask_A)
    mask_B_clean = re.sub(r"\W+", "_", args.mask_B)
    #print(mask_A_clean, mask_B_clean)
    output_filename = f"cascade_results_{mask_A_clean}_to_{mask_B_clean}.pkl"
    if args.empty_B == "True":
        output_filename = f"cascade_results_{mask_A_clean}_to_all.pkl"
    output_path = f"{args.output_dir}/{output_filename}"

    # Save results to JSON
    save_results_to_pickle(result_dict, output_path)
    print(f"Results saved to {output_path}")


if __name__ == "__main__":
    main()
