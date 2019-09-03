#!/bin/bash

#Removing all the files created by dagman and rfile, while saving rsp file:
mv run.dag.rsp SAVErun.dag.rsp
rm -f run.d*
mv SAVErun.dag.rsp run.dag.rsp

Rscript create_config.R $1

singularity run ../../../deploy/megadapt.sif --db-config db-postgres.json vbsa setup --experiment-config config.json
