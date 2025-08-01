import sqlite3
import numpy as np
import pandas as pd
from scipy.sparse import coo_matrix

dataset_version = 626
sql_path = f'banc_{dataset_version}_data.sqlite'

def table_to_sparse_matrix(table, row_key, col_key, row_labels=None, col_labels=None):
    if row_labels is None:
        row_labels = table[row_key].unique()
    if col_labels is None:
        col_labels = table[col_key].unique()

    # Create mappings for rows and columns
    row_map = {label: i for i, label in enumerate(row_labels)}
    col_map = {label: i for i, label in enumerate(col_labels)}
    
    # Map rows and columns to integer indices
    row_indices = table[row_key].map(row_map).to_numpy()
    col_indices = table[col_key].map(col_map).to_numpy()
    
    # Create a sparse matrix
    sparse_matrix = coo_matrix((table['weight'], (row_indices, col_indices)), 
                               shape=(len(row_labels), len(col_labels)))
    
    # Convert to a pandas SparseDataFrame
    sparse_df = pd.DataFrame.sparse.from_spmatrix(sparse_matrix, 
                                                  index=row_labels, 
                                                  columns=col_labels)

    return sparse_df

def elist_to_grouped_matrix_sparse(elist, pre_col=None, post_col=None, pre_vals=None, post_vals=None,
                            weight_col='count', laterality_annotation=None, simplify_column_names=True):
    if pre_col is None:
        pre_key = 'pre'
    else:
        pre_key = f'pre_{pre_col}'

    if post_col is None:
        post_key = 'post'
    else:
        post_key = f'post_{post_col}'
    
    if laterality_annotation is not None:
        laterality = elist['laterality']
        key = pre_key if laterality_annotation == 'pre' else post_key
        values = elist[key]
        elist[key] = elist[key] + '_' + laterality

    elist = elist.groupby([pre_key, post_key], as_index=False).agg(weight=(weight_col, 'sum'))
    matrix = table_to_sparse_matrix(elist, pre_key, post_key, pre_vals, post_vals)
    return matrix

def load_meta(sql_path, index_col='id'):
    conn = sqlite3.connect(sql_path)
    
    # # Query to get table names
    # cursor = conn.cursor()
    # cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    # tables = cursor.fetchall()
    # # Print table names
    # print('fetching tables', len(tables))
    # for table in tables:
    #     print(table[0])

    query = f"SELECT * FROM meta"
    meta_table = pd.read_sql_query(query, conn)
    conn.close()
    meta_table['id'] = meta_table['root_id']
    meta_table = meta_table[meta_table['id'] != '']
    meta_table = meta_table[~meta_table['id'].isnull()]
    meta_table[index_col] = meta_table[index_col].values.astype(int)
    meta_table = meta_table.set_index(index_col, drop=False)
    meta_table = meta_table.replace('', np.nan)
    meta_table = meta_table[meta_table['side'] != 'na']
    meta_table['side'] = meta_table['side'].replace('midline', 'center')

    meta_table = meta_table[~meta_table.index.duplicated(keep='first')]

    indices = meta_table.index.values

    # meta_table.loc[meta_table['cell_class'] == 'ascending_neuron', 'super_class'] = 'ascending'
    # meta_table.loc[meta_table['cell_class'] == 'descending_neuron', 'super_class'] = 'descending'
    # meta_table.loc[meta_table['cell_class'] == 'sensory_ascending', 'super_class'] = 'sensory_ascending'

    # excitatory_mask = (meta_table['top_nt'] == 'acetylcholine').astype(int)
    # inhibitory_mask = meta_table['top_nt'].isin(('glutamate', 'gaba')).astype(int)

    # nt_valence = excitatory_mask - inhibitory_mask
    # meta_table['nt_valence'] = nt_valence

    # for neuromere in ['LB', 'MX', 'MD']:
    #     mask = meta_table['hemilineage'].str.startswith(neuromere) == True
    #     meta_table.loc[mask, 'neuromere'] = neuromere

    # for column in meta_table.columns:
    #     try:
    #         meta_table[column] = meta_table[column].str.replace('auto:', '')
    #     except AttributeError:
    #         continue

    return meta_table

META_TABLE = load_meta(sql_path)

def add_laterality_to_elist(elist):
    pre_side = META_TABLE.loc[elist['pre'], 'side'].values
    post_side = META_TABLE.loc[elist['post'], 'side'].values
    
    laterality = np.full(pre_side.shape, 'contralateral')
    laterality[(pre_side == post_side) & (pre_side == 'center')] = 'central'
    laterality[(pre_side == post_side) & (pre_side != 'center')] = 'ipsilateral'
    laterality[(pre_side != post_side) & (post_side == 'center')] = 'inward'
    laterality[(pre_side != post_side) & (pre_side == 'center')] = 'outward'

    laterality_num = (laterality == 'ipsilateral').astype(int) - (laterality == 'contralateral').astype(int)
    elist = elist.assign(laterality=laterality, laterality_num=laterality_num)
    return elist

def add_meta_to_elist(elist, columns=[]):
    annotations = {}
    for col in columns:
        for partner in ['pre', 'post']:
            key = f'{partner}_{col}'
            value = META_TABLE.loc[elist[partner], col].values
            annotations[key] = value
    elist = elist.assign(**annotations)
    
    elist = add_laterality_to_elist(elist)
    
    return elist

def get_elist(conn, source_ids=None, target_ids=None, weight_col='count', min_weight=0, exclude_self_conn=True):
    if (source_ids is None) and (target_ids is None):
        assert False, 'must provide sources or targets'
    
    conditions = []
    all_ids = []
    if source_ids is not None:
        question_marks = ','.join(['?'] * len(source_ids))
        sub_query = f'(pre IN ({question_marks}))'
        conditions.append(sub_query)
        all_ids = all_ids + source_ids

    if target_ids is not None:
        question_marks = ','.join(['?'] * len(target_ids))
        sub_query = f'(post IN ({question_marks}))'
        conditions.append(sub_query)
        all_ids = all_ids + target_ids

    conditions = ' AND '.join(conditions)

    query = f'SELECT * FROM edgelist_simple WHERE (({weight_col} >= {min_weight}) AND ({conditions}))'
    elist = pd.read_sql_query(query, conn, params=all_ids)
    elist['pre'] = elist['pre'].astype(int)
    elist['post'] = elist['post'].astype(int)

    if exclude_self_conn:
        elist = elist[elist['pre'] != elist['post']]

    # remove connections that don't correspond to a neuron in the dataset
    elist = elist[elist['pre'].isin(META_TABLE.index) & elist['post'].isin(META_TABLE.index)]
    return elist


def get_full_elist(conn, weight_col='count', min_weight=5, exclude_self_conn=True, return_ids=True):
    query = f'SELECT * FROM edgelist_simple WHERE ({weight_col} >= {min_weight})'
    elist = pd.read_sql_query(query, conn)
    elist['pre'] = elist['pre'].astype(int)
    elist['post'] = elist['post'].astype(int)

    # remove connections that don't correspond to a neuron in the dataset
    mask = ~META_TABLE['super_class'].isin(('sensory', 'motor', 'endocrine', 'sensory_ascending'))
    ids = set(META_TABLE.loc[mask, 'id'].values)
    elist = elist[elist['pre'].isin(ids) & elist['post'].isin(ids)]

    if exclude_self_conn:
        elist = elist[elist['pre'] != elist['post']]

    N = -1
    while True:
        intersection = set(elist['pre'].values).intersection(elist['post'].values)
        N_new = len(intersection)
        if N_new == N:
            break
        N = N_new
        print(N_new)
        elist = elist[elist['pre'].isin(intersection) & elist['post'].isin(intersection)]

    if return_ids:
        intersection = sorted(intersection)
        return elist, intersection
    else:
        return elist

def get_full_connectivity():
    conn = sqlite3.connect(sql_path)
    elist, intersection = get_full_elist(conn)
    conn.close()

    matrix = elist_to_grouped_matrix_sparse(elist, None, None, intersection, intersection,
                                            weight_col='count')
    return matrix, elist

def get_full_connectivity_filtered(col, values=None, min_weight=5, prune=True):
    conn = sqlite3.connect(sql_path)
    elist, _ = get_full_elist(conn, min_weight=min_weight)
    conn.close()

    elist = add_meta_to_elist(elist, [col])
    if values is not None:
        elist = elist[elist[f'pre_{col}'].isin(values) & elist[f'post_{col}'].isin(values)]

    intersection = set(elist['pre'].values).union(elist['post'].values)
    N = -1
    while prune:
        intersection = set(elist['pre'].values).intersection(elist['post'].values)
        N_new = len(intersection)
        if N_new == N:
            break
        N = N_new
        print(N_new)
        elist = elist[elist['pre'].isin(intersection) & elist['post'].isin(intersection)]

    matrix = elist_to_grouped_matrix_sparse(elist, None, None, intersection, intersection,
                                            weight_col='count')
    return matrix, elist

def get_full_connectivity_2(col, values=None):
    conn = sqlite3.connect(sql_path)
    elist, intersection = get_full_elist(conn)
    conn.close()

    elist = add_meta_to_elist(elist, [col])
    if values is not None:
        elist = elist[elist[f'pre_{col}'].isin(values) & elist[f'post_{col}'].isin(values)]

    N = -1
    while True:
        intersection = set(elist[f'pre_{col}'].values).intersection(elist[f'post_{col}'].values)
        N_new = len(intersection)
        if N_new == N:
            break
        N = N_new
        print(N_new)
        elist = elist[elist[f'pre_{col}'].isin(intersection) & elist[f'post_{col}'].isin(intersection)]
    intersection = sorted(intersection)

    matrix = elist_to_grouped_matrix_sparse(elist, col, col, intersection, intersection,
                                            weight_col='count')
    return matrix, elist

def get_ids(permit=None, col=None):
    if permit is None:
        return None
    mask = META_TABLE[col].isin(permit) # & META_TABLE['is_vnc']
    ids = META_TABLE[mask].index.to_list()
    return ids

# def elist_to_grouped_matrix(elist, pre_col=None, post_col=None, pre_vals=None, post_vals=None,
#                             weight_col='count', laterality_annotation=None, simplify_column_names=True):
#     if pre_col is None:
#         pre_key = 'pre'
#     else:
#         pre_key = f'pre_{pre_col}'

#     if post_col is None:
#         post_key = 'post'
#     else:
#         post_key = f'post_{post_col}'
    
#     if laterality_annotation is not None:
#         laterality = elist['laterality']
#         key = pre_key if laterality_annotation == 'pre' else post_key
#         values = elist[key]
#         elist[key] = elist[key] + '_' + laterality

#     elist = elist.groupby([pre_key, post_key], as_index=False).agg(weight=(weight_col, 'sum'))
#     matrix = elist.pivot(index=pre_key, columns=post_key, values='weight')

#     if simplify_column_names:
#         matrix = matrix.rename(index={pre_key: 'pre'}, columns={post_key: 'post'})
#     if pre_vals is None:
#         pre_vals = sorted(matrix.index.to_list())
#     if post_vals is None:
#         post_vals = sorted(matrix.columns.to_list())
#     matrix = matrix.reindex(index=pre_vals, columns=post_vals).fillna(0)
#     return matrix

def filter_by_norm(matrix, min_norm=0):
    normalized = matrix.values / matrix.values.sum(-1, keepdims=True)
    mask = np.any(normalized >= min_norm, axis=0)
    matrix = matrix.iloc[:, mask]
    return matrix

def get_connectivity(pre_vals=None, post_vals=None,
                     pre_col='cell_type', post_col='cell_type',
                     extras=[],
                     laterality_annotation=None, exclude_empty=False,
                     min_weight=0, weight_col='count',
                     enforce_recurrent=False):
    pre_ids = get_ids(pre_vals, pre_col)
    post_ids = get_ids(post_vals, post_col)

    conn = sqlite3.connect(sql_path)
    elist = get_elist(conn, pre_ids, post_ids, weight_col, min_weight)
    conn.close()
    elist = add_meta_to_elist(elist, [pre_col, post_col] + extras)
    matrix = elist_to_grouped_matrix_sparse(elist, pre_col, post_col, pre_vals, post_vals,
                                            laterality_annotation=laterality_annotation,
                                            weight_col=weight_col)
    
    if exclude_empty:
        while True:
            pre_mask = matrix.values.sum(1) > 0
            post_mask = matrix.values.sum(0) > 0
            if pre_mask.all() and post_mask.all():
                break
            matrix = matrix.iloc[pre_mask, post_mask]

    return matrix, elist
