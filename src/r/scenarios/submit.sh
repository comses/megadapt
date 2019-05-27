#!/usr/bin/env bash

set -o nounset


R CMD build ../megadaptr
R CMD INSTALL ../megadaptr_0.1.0.tar.gz

SCENARIO_NAME=$1
MENTAL_MODEL=$2
FLOODING_MODEL=$3
PONDING_MODEL=$4

./create_submit_model.py --experiment_name $SCENARIO_NAME --mental_models $MENTAL_MODEL --flooding_model $FLOODING_MODEL --ponding_model $PONDING_MODEL  

condor_submit ${SCENARIO_NAME}.sub
