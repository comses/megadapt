#!/bin/bash

singularity run ../../../deploy/megadapt.sif --db-config db-postgres.json vbsa run --experiment-config config.json --id $4 --params $1 --sample_n $2 --ABMat $3
