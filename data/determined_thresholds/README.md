# `data/determined_thresholds/` — analysis-derived threshold values

Two small CSV one-liners that record threshold values *fitted elsewhere
in the pipeline* but read at session startup so every script sees the
same numbers.

## Layout

| File | What it records | Set by |
| --- | --- | --- |
| `influence_norm_log_elbow_threshold.csv` | The "high-influence" cutoff (currently `17.28`) fitted as the elbow of the cumulative AN/DN-to-effector adjusted-influence distribution. ED Fig. 5e shows the elbow detection. | `R/figures/panels_body_parts.R` (calls `find_angle_change_in_range()` on the cumulative distribution and writes the elbow rank+value here). |
| `pairwise_modal_influence.csv` | The modal cluster–cluster adjusted-influence value used to normalise the Fig. 3e/f heatmaps so colour scales are comparable across panels. | `R/figures/panels_an_dn_influence.R`. |

## Why a directory of its own

These thresholds are computed once and then re-read across many panels.
Persisting them as tiny CSVs (rather than hard-coding the numbers in
every panel) lets us re-fit on a new dataset version without
threading constants through dozens of files. `R/startup/banc-startup.R`
reads both at session start; downstream panels reference the loaded
variables (`influence_norm_thresh`, etc.).

## Don't edit by hand

Re-run the producing panel script to refresh either CSV; both files are
single-line and overwritten in their entirety on each run.
