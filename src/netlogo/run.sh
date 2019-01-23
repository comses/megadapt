#!/bin/bash

# the headless experiment to run is the first argument to the script, e.g.,
# % ./run.sh Comparing 
EXPERIMENT=${1:-"investment_on_vuln"}
DATE=$(date '+%Y-%m-%d.%H.%M.%S')
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
IMAGE_NAME=comses/megadaptn

docker build -t ${IMAGE_NAME} ${DIR}

docker run --rm -it \
    --volume="${DIR}:/code/output:rw" \
    ${IMAGE_NAME}:latest /opt/netlogo/netlogo-headless.sh --model megadapt-theoretical.nlogo --experiment $EXPERIMENT --table output/output-${DATE}.csv
