#!/bin/bash
#SBATCH -J Test
#SBATCH -N 1
#SBATCH --mem=32000
#SBATCH -o Test-%j.out
#SBATCH -e Test-%j.err
#SBATCH -t 250:00:00
#SBATCH --mail-type=END


for i in {1..50}; do
  echo $i > numbers.txt
  module load R/4.3
  Rscript model1.R
  module load python/3.10
  python3 model2.py
  module load R/4.3
  Rscript model3.R
done