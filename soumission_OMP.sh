#!/bin/bash

#SBATCH --job-name=myjob_omp
#SBATCH --output=job.%j.out # output messages go here 
#SBATCH --error=job.%j.err    # error messages go here 
#SBATCH --mail-user=thomas.lechat@ec-lyon.fr
#SBATCH --mail-type=ALL
#
#SBATCH --partition=thin # partition name 
#SBATCH --nodes=1   
#SBATCH --cpus-per-task=16
#SBATCH --mem=64000 # amount of RAM memory required per node, in Mega Bytes
#SBATCH --time=03:00:00 



export OMP_NUM_THREADS=5
./programme parametres_simulation.m |& tee log.txt

#for i in `seq 15 16`
#do
#	cmd="./programme_OMP parametres_simulation.m |& tee log$i.txt"
#	echo $cmd
#	eval $cmd
#done
