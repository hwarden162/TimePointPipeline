#!/bin/bash
#$ -N GOFML
#$ -pe sharedmem 8
#$ -cwd
#$ -l h_vmem=12G
#$ -l h_rt=36:00:00
#$ -o ./submission_logs/
#$ -e ./submission_logs/

. /etc/profile.d/modules.sh

module load anaconda/2024.02

source activate /exports/igmm/eddie/khamseh-lab/hwarden/GOFMLPipeline/env

Rscript ./R/01PrepData.R
Rscript ./R/02TrainPreprocessors.R
Rscript ./R/03TrainModels.R
Rscript ./R/04EvaluateModels.R
Rscript ./R/05ConformalPrediction.R
Rscript ./R/06SHAPValues.R