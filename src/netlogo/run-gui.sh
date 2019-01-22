#!/bin/bash

# Use this script to load the NetLogo GUI on a Linux machine using Docker

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

IMAGE_NAME=comses/megadaptn

docker build -t ${IMAGE_NAME} ${DIR}

xhost +local:root

docker run --rm -it \
    --env="DISPLAY" \
    --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
    ${IMAGE_NAME}:latest /opt/netlogo/NetLogo /code/megadapt-theoretical.nlogo

xhost -local:root
