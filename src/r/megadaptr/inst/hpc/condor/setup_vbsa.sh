#!/bin/bash

#Removing all the files created by dagman and rfile, while saving rsp file:
mv run_vbsa.dag.rsp SAVErun_vbsa.dag.rsp
rm -f run.d*
mv SAVErun_vbsa.dag.rsp run_vbsa.dag.rsp

singularity run ../../../../../../deploy/megadapt.sif --db-config ../../experiment/db-postgres.json vbsa setup --experiment-config config.json --experiment-number $1 --maximum-exponent $2 --simulate-function $3
