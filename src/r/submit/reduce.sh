#!/bin/bash

singularity run ../../../deploy/megadapt.sif --db-config db-postgres.json vbsa reduce --experiment-config config.json
