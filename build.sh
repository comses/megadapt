#!/usr/bin/env bash

docker build -f deploy/Dockerfile.base -t megadaptr .
docker save megadaptr > megadaptr.tar.gz
singularity build megadaptr.sif docker-archive://megadaptr.tar.gz
