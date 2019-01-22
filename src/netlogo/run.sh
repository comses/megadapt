#!/bin/bash

# the headless experiment to run is the first argument to the script, e.g.,
# % ./run.sh spatial_patterns 
# % ./run.sh sensitivity
EXPERIMENT=${1:-"test"}

/opt/netlogo/netlogo-headless.sh --model megadapt-theoretical.nlogo --experiment $EXPERIMENT
