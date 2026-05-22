# `data/shiu_et_al_2024/` — external reference data (Shiu et al., 2024, *Nature*)

External published reference data from **Shiu et al., 2024, *Nature***
(DOI [`10.1038/s41586-024-07763`](https://doi.org/10.1038/s41586-024-07763)) —
optogenetic-activation results and circuit-model predictions for SEZ
(suboesophageal zone) neurons in *Drosophila*.

## Layout

| File | What it is |
| --- | --- |
| `41586_2024_7763_MOESM2_ESM.xlsx` | Supplementary Data 2 from the Shiu et al. paper, untouched. |
| `sez_neurons.csv` / `sez_neurons.pickle` | The Shiu et al. SEZ neuron roster, re-exported as CSV + pickle for convenient R / Python loading. |
| `shiu_et_al_2024_opto_and_model_results.csv` | Per-neuron optogenetic-activation results + model predictions, in Shiu et al.'s original neuron-ID space. |
| `shiu_et_al_2025_fafb_mapped_opto_and_model_results.csv` | The same results table after re-mapping IDs into the FAFB-FlyWire space via the BANC ↔ FAFB bridge — usable as a direct cross-reference for BANC neurons. |

## Provenance

External — supplementary materials from Shiu et al. 2024. The
FAFB-mapped re-export was produced in-project as a one-off cross-reference
sheet; the mapping uses the same FAFB v783 ↔ BANC bridge that
`franken.meta` records.

## Consumers

Not currently read by any paper-bound script. Kept as archival
cross-reference for the feeding / proboscis-extension circuit comparisons
in the discussion section.
