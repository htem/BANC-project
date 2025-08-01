import pandas as pd
import numpy as np
import sqlite3
import os
import json
import pickle
import argparse
import re
import cascade_model

def load_sqlite_database(sql_path):
    """
    Load metadata and connectivity data from SQLite database.
    """
    conn = sqlite3.connect(sql_path)
    meta_df = pd.read_sql_query("SELECT * FROM meta", conn)
    try:
        edgelist_df = pd.read_sql_query("SELECT * FROM edgelist_simple", conn)
    except Exception:
        edgelist_df = pd.read_sql_query("SELECT * FROM edgelist", conn)
    conn.close()
    return meta_df, edgelist_df


def filter_edgelist_and_update_meta(edgelist_df, meta_df, synaptic_threshold=25):
    """
    Filter the edgelist and update metadata based on the synaptic threshold.
    """
    grouped_edgelist = edgelist_df.groupby(['pre', 'post'], as_index=False).agg(synaptic_count=('count', 'sum'))
    filtered_pairs = grouped_edgelist[grouped_edgelist['synaptic_count'] > synaptic_threshold][['pre', 'post']]
    filtered_edgelist = edgelist_df.merge(filtered_pairs, on=['pre', 'post'], how='inner')
    nt_mapping = meta_df.set_index('id')['top_nt'].to_dict()
    filtered_edgelist['top_nt'] = filtered_edgelist['pre'].map(nt_mapping)
    remaining_neurons = set(filtered_edgelist['pre']).union(set(filtered_edgelist['post']))
    updated_meta = meta_df[meta_df['id'].isin(remaining_neurons)].copy()
    filtered_out_neurons = set(meta_df['id']) - remaining_neurons
    return filtered_edgelist, updated_meta, filtered_out_neurons

def process_filtered_edgelist_by_nt(filtered_edgelist, is_signed, invert_nts=None):
    """
    Process the filtered edgelist by adding a new column 'effective_count',
    which adjusts the 'count' values based on specific neurotransmitters in
    the 'pre_top_nt' column if `is_signed` is True.

    Parameters:
    -----------
    filtered_edgelist : DataFrame
        The edgelist DataFrame with columns including 'pre_top_nt' and 'count'.
    invert_nts : list of str, optional
        List of neurotransmitters for which to invert the 'count' sign.
        Default is ['gaba', 'glutamate'].
    is_signed : bool, optional
        If True, invert the 'count' for specified neurotransmitters. Default is True.

    Returns:
    --------
    processed_edgelist : DataFrame
        The modified edgelist with a new column 'effective_count'.
    """
    if invert_nts is None:
        invert_nts = ['gaba', 'glutamate']  # Default neurotransmitters to invert

    # Initialize 'effective_count' as a copy of 'count'
    filtered_edgelist['effective_count'] = filtered_edgelist['count']

    if is_signed:
        # Process each neurotransmitter in the invert list
        for nt in invert_nts:
            filtered_edgelist.loc[
                filtered_edgelist.top_nt.str.contains(nt, case=False, na=False), 
                "effective_count"
            ] *= -1

    return filtered_edgelist

def extract_mask(meta_df, mask_key, mask_keyword):
    """
    Extract a boolean mask based on the key and keyword.
    """
    if mask_key == "id":
        #mask = meta_df[mask_key].astype(str).isin(mask_keyword.split(","))
        mask = meta_df[mask_key].astype(str).isin([str(value) for value in mask_keyword])
    else:
        mask = meta_df[mask_key].str.contains(mask_keyword, case=False, na=False)
    return mask


def run_cascade_analysis(meta_df, edgelist_df, mask_A, mask_B, empty_B, max_timesteps, activation_threshold, n_iterations):
    """
    Run the cascade analysis.
    """
    cascade_simulation = cascade_model.SignalCascade(
        activation_threshold=activation_threshold, 
        n_iterations=n_iterations, 
        max_timesteps=max_timesteps
    )
    start_neurons = set(meta_df[mask_A]['id'])
    print(start_neurons)
    end_neurons = set(meta_df[mask_B]['id'])
    if empty_B:
        end_neurons = set()
    result_dict = cascade_simulation.run_cascade_v2(
        S_A0=start_neurons,
        S_E=end_neurons,
        edgelist=edgelist_df,
    )
    return result_dict


def save_results_to_pickle(result_dict, file_path):
    """
    Save the result dictionary to a pickle file.
    """
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    with open(file_path, 'wb') as f:
        pickle.dump(result_dict, f)

def reverse_index_to_neuron(index_to_neuron):
    """
    Create a reverse mapping from neuron to index.

    Parameters:
        index_to_neuron (dict): A dictionary mapping indices to neuron identifiers.

    Returns:
        dict: A dictionary mapping neuron identifiers to indices.
    """
    return {neuron: index for index, neuron in index_to_neuron.items()}

def create_activation_distance_df(metadata_df, cell_matrix, neuron_to_index):
    """
    Creates a DataFrame with neuron IDs and the time step at which each neuron is activated.

    Parameters:
        metadata_df (pd.DataFrame): DataFrame containing metadata for cells indexed by cell_id.
        cell_matrix (np.ndarray): A 2D array (cell by time step) representing activity levels.
        neuron_to_index (dict): A dictionary mapping neuron names to row indices in cell_matrix.

    Returns:
        pd.DataFrame: A DataFrame with columns 'id' (neuron IDs) and 'distance' (time step of activation or NaN).
    """
    activation_data = []
    metadata_df['id'] = metadata_df['id'].astype('string')

    for neuron_id in metadata_df['id']:
        #print(type(neuron_id))
        index = neuron_to_index.get(neuron_id, None)
        if index is not None:
            # Find the first time step where the neuron is activated
            activation_steps = np.where(cell_matrix[index, :] > 0)[0]
            distance = activation_steps[0] if activation_steps.size > 0 else np.nan
        else:
            distance = np.nan

        activation_data.append({'id': neuron_id, 'distance': distance})
    result_df = pd.DataFrame(activation_data)
    result_df['id'] = result_df['id'].fillna('NaN').astype('string')
    result_df['id'] = result_df['id'].fillna('missing').astype('string')
    return result_df.sort_values(by='distance', na_position='last')

def main():
    parser = argparse.ArgumentParser(description="Run cascade analysis with batched queries.")
    parser.add_argument("--sql_path", type=str, 
                        default = '/n/data1/hms/neurobio/wilson/banc/connectivity/frankenbrain_v.1.6_data.sqlite',
                        #default="/n/data1/hms/neurobio/wilson/banc/connectivity/banc_496_data.sqlite",
                        #default="/Users/jingxuanfan/Documents/GitHub/sql_local/frankenbrain_v.1.1_data.sqlite", # local path
                        help="Path to the SQLite database.")
    parser.add_argument("--synaptic_threshold", type=int, default=1, help="Synaptic threshold for filtering.")
    parser.add_argument("--is_signed", type=bool, default=True, help="If True, adjust 'count' based on neurotransmitters.")
    parser.add_argument("--json_path", type=str, default='criteria_neuron_ids_seed_03.json', help="Path to the JSON file with batch parameters.")
    parser.add_argument("--from_seatable", type=bool, default=True, help="If True, load batch parameters from seatable.")
    parser.add_argument("--output_dir", type=str, default="/n/groups/wilson/Jingxuan/BANC-project/data/cascade/frankenbrain_v1.6/seed_03", help="Directory to save results.")
    #parser.add_argument("--seed_mode", type=str, default='seed', help="Whether to use seed or modality for starting seeds.")
    args = parser.parse_args()

    # Load SQLite database
    meta_df, edgelist_df = load_sqlite_database(args.sql_path)
    # local only
    #meta_df = pd.read_csv('/Users/jingxuanfan/Documents/GitHub/BANC-project/data/meta/frankenbrain_v.1.4_meta.csv',low_memory=False)
    #meta_df['id'] = meta_df['id'].apply(lambda x: '{:.0f}'.format(x))
    print("Loaded SQLite database.")
    # Filter metadata and edgelist
    filtered_edgelist, filtered_meta, _ = filter_edgelist_and_update_meta(
        edgelist_df, meta_df, args.synaptic_threshold
    )
    #if synaptic_threshold > 0:
    filtered_edgelist = process_filtered_edgelist_by_nt(filtered_edgelist, args.is_signed)
    signed = "signed" if args.is_signed else "unsigned"
    # Load batch parameters from JSON
    with open(args.json_path, 'r') as f:
        batch_params = json.load(f)
    from_seatable = args.from_seatable
    for batch in batch_params:
        #synaptic_threshold = batch["synaptic_threshold"]
        if from_seatable:
            mask_A = batch_params[batch]
            mask_B = batch_params[batch]
            empty_B = True
            mask_key_A = "id"
            mask_key_B = "id"
            max_timesteps = 15
            activation_threshold = 0.01
            n_iterations = 100
        else:
            mask_A = batch["mask_A"]
            mask_B = batch["mask_B"]
            empty_B = bool(batch["empty_B"])
            mask_key_A = batch["mask_key_A"]
            mask_key_B = batch["mask_key_B"]
            max_timesteps = batch["max_timesteps"]
            activation_threshold = batch["activation_threshold"]
            n_iterations = batch["n_iterations"]

        # Extract masks for start and end neurons
        mask_A_filter = extract_mask(filtered_meta, mask_key_A, mask_A)
        mask_B_filter = extract_mask(filtered_meta, mask_key_B, mask_B)

        # Run cascade analysis
        result_dict = run_cascade_analysis(
            filtered_meta, 
            filtered_edgelist, 
            mask_A_filter, 
            mask_B_filter, 
            empty_B,
            max_timesteps, 
            activation_threshold, 
            n_iterations
        )
        if from_seatable:
            mask_A_clean = batch
            mask_B_clean = batch
            if empty_B:
                output_filename = f"{mask_A_clean}_to_all_{signed}.pkl"
            else:
                output_filename = f"{mask_A_clean}_to_{mask_B_clean}_{signed}.pkl"
        else:
            # Generate output file path
            mask_A_clean = re.sub(r"\W+", "_", mask_A)
            mask_B_clean = re.sub(r"\W+", "_", mask_B)
            if empty_B:
                output_filename = f"{mask_A_clean}_to_all_{signed}"
            else:
                output_filename = f"{mask_A_clean}_to_{mask_B_clean}_{signed}"
        cleaned_name = re.sub(r'[^a-zA-Z0-9_]', '', output_filename)
        result_pkl_name = cleaned_name + '_distance.pkl'
        activation_probabilities = result_dict['probabilities']
        neuron_to_index = reverse_index_to_neuron(result_dict['index_to_neuron'])
        
        # Create the distance DataFrame
        distance_df = create_activation_distance_df(meta_df, activation_probabilities.T, neuron_to_index)
        output_path = os.path.join(args.output_dir, result_pkl_name)

        # Save results
        save_results_to_pickle(distance_df, output_path)
        #if empty_B:
            #print(f"Saved results for {mask_A} to all at {output_path}")
        #else:
            #print(f"Saved results for {mask_A} to {mask_B} at {output_path}")


if __name__ == "__main__":
    main()
