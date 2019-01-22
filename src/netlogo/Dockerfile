FROM comses/netlogo:6.0.4

ARG EXPERIMENT=timeseries
LABEL maintainer="Allen Lee <allen.lee@asu.edu>"

WORKDIR /code
COPY . /code

RUN /opt/netlogo/netlogo-headless.sh --model megadapt-theoretical.nlogo --experiment $EXPERIMENT
