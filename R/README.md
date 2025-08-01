# BANC Analysis Scripts

This directory contains R scripts for analysing BANC data and reproducing all paper figures. The scripts are organised into functional categories to support different types of analysis.

💡 **For the most current connectome data, visit [FlyWire Codex](https://codex.flywire.ai/banc) before running analyses.**

## Directory Organisation

### `figures/` - Figure Generation Scripts
Scripts that directly generate panels for publication figures. Each script corresponds to specific figure components:

- `panel_inventory.R` - Dataset overview and quality metrics (Figure 1)
- `panel_proofread_matching.R` - Neuron reconstruction statistics (Figure 1) 
- `panel_all_to_all_influence.R` - Influence analysis heatmaps (Figure 2)
- `panel_efferent_umap.R` - Efferent neuron clustering (Figure 2)
- `panel_an_dn_umap.R` - Ascending/descending neuron analysis (Figure 3)
- `panel_super_clusters.R` - Behavioural module identification (Figure 3)
- `panels_body_parts.R` - Body part specific circuits (Figure 4)
- `panel_sensory_motor.R` - Sensory-motor integration analysis (Figure 5)
- `panel_cluster_sensory_correlations.R` - CNS network integration (Figure 6)

### `startup/` - Configuration and Data Loading
Essential scripts that set up the analysis environment:

- `banc-startup.R` - Main configuration file (set data paths here)
- `banc-meta.R` - Load BANC neuron metadata
- `banc-functions.R` - Custom analysis functions
- `banc-edgelist.R` - Load connectivity data
- `banc-influence.R` - Load influence metrics
- `franken-meta.R` - Load integrated connectome data

### `exploration/` - Additional Analysis Scripts
Extended analyses and custom investigations:

- `banc_vignettes.R` - Circuit vignette analyses
- `banc_influence_dapp.R` - Influence metric validation
- `banc_connectome_clustered.R` - Network clustering approaches
- Various body part and modality specific analyses

### `text/` - Paper Content Generation
Scripts that generate numerical results and supplemental information:

- `numbers.R` - Statistical summaries for paper text
- `supplemental_data.R` - Generate supplemental data tables
- `generate_author_list.R` - Author information processing

## Getting Started

### 1. Setup Data Access

Edit `startup/banc-startup.R` to configure data locations:

```r
# For users with Harvard Dataverse download
banc.connectivity.save.path <- "/path/to/downloaded/sqlite/files"
banc.influence.save.path <- "/path/to/influence/data"

# Or use provided paths for HMS users
source("R/startup/banc-startup.R")
```

### 2. Install Required Packages

The scripts require several R packages. Install missing packages as needed:

```r
# Core packages
install.packages(c("dplyr", "ggplot2", "igraph", "DBI", "RSQLite"))

# Specialized packages
install.packages(c("umap", "cluster", "RColorBrewer", "viridis"))

# Optional: natverse for neuroanatomy
devtools::install_github("natverse/natverse")
```

### 3. Load BANC Environment

```r
# Load core BANC environment
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
```

### 4. Generate Figures

Run individual figure scripts to reproduce paper panels:

```r
# Generate Figure 1 panels
source("R/figures/panel_inventory.R")

# Generate Figure 2 panels  
source("R/figures/panel_all_to_all_influence.R")

# Generate all figures
figure_scripts <- list.files("R/figures", pattern = "panel_.*\\.R", full.names = TRUE)
lapply(figure_scripts, source)
```

## Data Dependencies

The scripts expect access to:

1. **Main SQLite databases:**
   - `banc_data.sqlite` - Complete BANC connectome
   - `frankenbrain_v1.1_data.sqlite` - Integrated brain+VNC dataset
   - `fafb_783_data.sqlite` - FlyWire brain reference

2. **Influence metrics:**
   - `influence_banc_626.sqlite` - Pre-computed influence scores

3. **Metadata and processed datasets** in `data/` directory

Download locations are specified in the main README.

## Customizing Analyses

### Colour Schemes
Modify `settings/paper_colours_lacroix.csv` to customise visualisation colours.

### Analysis Parameters
Key parameters can be adjusted in the startup scripts:
- `banc.version` - Dataset version identifier
- Influence thresholds and normalization methods
- Clustering parameters

### Adding New Analyses
1. Create new script in appropriate subdirectory
2. Source required startup scripts
3. Follow existing code patterns for data access
4. Use standardised colour schemes for consistency

## Output Locations

Generated figures are saved to:
- `figures/figure_X/links/` - Individual figure panels
- Working directory - Intermediate analysis files

## Troubleshooting

**Common Issues:**
- **Path errors:** Check data paths in `banc-startup.R`
- **Missing packages:** Install required dependencies
- **Memory issues:** Large datasets may require 16+ GB RAM
- **SQLite access:** Ensure proper file permissions

**Getting Help:**
- Check existing scripts for similar analyses
- Review data column documentation in `data/README.md`
- Use BANC community resources (see main README)

## Performance Notes

- **Large datasets:** Scripts may take several minutes to hours for full analyses
- **Memory usage:** Some analyses require substantial RAM (8-32 GB recommended)
- **Parallel processing:** Some scripts support parallelization for speed improvements
- **Caching:** Intermediate results are cached when possible to speed re-runs

## Citation

If you use or modify these analysis scripts, please cite the BANC paper and acknowledge the specific scripts used in your analysis.