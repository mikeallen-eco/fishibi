#!/bin/bash

#SBATCH --partition=main
#SBATCH --requeue
#SBATCH --job-name=mitofish
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=24
#SBATCH --mem=100000
#SBATCH --time=1-10:00:00
#SBATCH -o %N_%j.out
#SBATCH -e %N_%j.err

crabs db_download --source mitofish --output mitofish.fasta --keep_original no