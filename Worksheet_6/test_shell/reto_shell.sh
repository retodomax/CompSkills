#!/bin/sh -v
for seed in 1 2 3 4 
do 
   echo $seed   
   Rscript -e "set.seed($seed); source('reto_script.R');save(rmse_mean, file='reto_script_$seed.RData')" &
done
