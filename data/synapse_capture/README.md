The files in this folder describe what fraction of synapses are part of proofread neurons in various connectome datasets. Here is how each file was acquired/generated:
- `BANCv{version}_synapse_capture_by_neuropil.csv`: Jasper generated these from BANC table exports ([cloud bucket](https://console.cloud.google.com/storage/browser/brain-and-nerve-cord_exports/brain_and_nerve_cord/)) using the script in [this comment](https://github.com/jasper-tms/BANC-project/issues/37#issuecomment-2594182812)
- `FAFBv783_synapse_attachment_rates.csv`: Downloaded from the codex.flywire.ai downloads page: https://codex.flywire.ai/api/download
- `MANC_synapse_capture.csv`: Values here were transcribed from Table 1 of the MANC paper (https://elifesciences.org/reviewed-preprints/97769). I suspect this is MANC v1.0 data and not MANC v1.2.1 data, but they don't say in the paper so I'm not sure.
- `FANCv1237_synapse_capture_by_neuropil.csv`: Jasper generated this from FANC v1237 (March 7 2025) synapse table + CAVE tables export, which are available on google cloud: https://console.cloud.google.com/storage/browser/female-adult-nerve-cord_exports/v1237

For additional details, see the issue for this task: https://github.com/jasper-tms/BANC-project/issues/37
