"""
GCS Parquet Reader - Efficient row-group based filtering
"""
import gcsfs
import pyarrow.parquet as pq
import pyarrow.compute as pc
import pyarrow as pa
import pandas as pd


def query_parquet_gcs(gcs_path, columns=None, filter_column=None, filter_prefix=None,
                      show_progress=True):
    """
    Read Parquet file from GCS with filtering, processing row groups iteratively.

    Args:
        gcs_path: Full GCS path (gs://bucket/path/file.parquet)
        columns: List of column names to select (None = all)
        filter_column: Column name to filter on
        filter_prefix: String prefix to match (e.g., 'MB' for 'MB%' LIKE query)
        show_progress: Whether to print progress updates

    Returns:
        pandas DataFrame with filtered results
    """
    # Remove gs:// prefix
    if gcs_path.startswith('gs://'):
        gcs_path = gcs_path[5:]

    # Setup GCS filesystem
    fs = gcsfs.GCSFileSystem(token='google_default')

    # Open Parquet file
    with fs.open(gcs_path, 'rb') as f:
        parquet_file = pq.ParquetFile(f)
        total_groups = parquet_file.num_row_groups

        if show_progress:
            print(f"File has {total_groups} row groups, {parquet_file.metadata.num_rows:,} total rows")
            if columns:
                print(f"Selecting columns: {', '.join(columns)}")
            if filter_column and filter_prefix:
                print(f"Filtering: {filter_column} starts with '{filter_prefix}'")
            print()

        result_tables = []

        # Process each row group
        for i in range(total_groups):
            # Read one row group
            table = parquet_file.read_row_group(i, columns=columns)

            # Apply filter if specified
            if filter_column and filter_prefix:
                mask = pc.starts_with(table[filter_column], filter_prefix)
                table = table.filter(mask)

            # Store non-empty results
            if table.num_rows > 0:
                result_tables.append(table)

            # Progress update
            if show_progress and (i + 1) % 100 == 0:
                found = sum(t.num_rows for t in result_tables)
                progress = (i + 1) / total_groups * 100
                print(f"\rProgress: {progress:5.1f}% | {i+1}/{total_groups} groups | Found: {found:,}   ",
                      end='', flush=True)

        if show_progress:
            print()  # New line after progress

        # Combine results
        if not result_tables:
            return pd.DataFrame()

        combined_table = pa.concat_tables(result_tables)
        result_df = combined_table.to_pandas()

        if show_progress:
            print(f"✓ Found {len(result_df):,} rows, {len(result_df.columns)} columns")

        return result_df


def query_influence_chunks_isin(gcs_dir, upstream_ids, columns=None,
                                  upstream_col="upstream_id", show_progress=True):
    """
    Read a directory of parquet chunks (e.g. all_to_all/) from GCS, returning
    only rows whose `upstream_col` value is in `upstream_ids`.

    Uses pyarrow.dataset for predicate pushdown across all chunks; the filter
    is applied chunk-by-chunk on the server side so only the matching slice
    is materialised.

    Args:
        gcs_dir: GCS directory path (gs://bucket/path/) containing parquet chunks.
        upstream_ids: Iterable of upstream_id values to keep.
        columns: List of column names to select (None = all).
        upstream_col: Name of the column to filter (default 'upstream_id').
        show_progress: Print progress + final summary.

    Returns:
        pandas DataFrame with filtered results.
    """
    import pyarrow.dataset as pads
    import gcsfs

    if gcs_dir.startswith('gs://'):
        gcs_dir = gcs_dir[5:]
    gcs_dir = gcs_dir.rstrip('/')

    fs = gcsfs.GCSFileSystem(token='google_default')

    if show_progress:
        print(f"Opening pyarrow dataset at gs://{gcs_dir}/ ...")
    dataset = pads.dataset(gcs_dir, format="parquet", filesystem=fs)
    if show_progress:
        n_chunks = len(list(dataset.get_fragments()))
        print(f"Dataset: {n_chunks} chunks; filtering {upstream_col} IN ({len(upstream_ids)} ids)")

    # Build filter (column IS IN list).
    filter_expr = pc.field(upstream_col).isin(
        pa.array(list(upstream_ids), type=pa.string())
    )
    if show_progress:
        print("Running predicate pushdown across chunks ...")
    table = dataset.to_table(filter=filter_expr, columns=columns)
    df = table.to_pandas()
    if show_progress:
        print(f"✓ Returned {len(df):,} rows, {len(df.columns)} columns")
    return df
