#!/bin/bash
#SBATCH -p short
#SBATCH -c 1                         # 1 CPU core
#SBATCH --mem=20G                    # 20GB RAM
#SBATCH -t 10:00:00                   # 8hr
#SBATCH --job-name="vscodetunnel"    
#SBATCH -o sbatch_logs/%j.out    # File to which STDOUT will be written, %j inserts jobid
#SBATCH -e sbatch_logs/%j.err    # File to which STDERR will be written, %j inserts jobid
#SBATCH --mail-user=jfan@g.harvard.edu  # Email to which notifications will be sent

# Load required modules
module load gcc/9.2.0
module load miniconda3/23.1.0

# Activate your conda environment
source activate banc
#python batch_cascade_franken.py
#python ../real_data_cascade_main.py --mask_A "MBON09" --mask_B "descending" --max_timestep 10
#python ../real_data_cascade_main.py --mask_A "mechanosensory" --mask_B "MBON" --mask_key_A "modality" --mask_key_B "cell_type" --max_timestep 10
# Keep the job running
sleep 2h