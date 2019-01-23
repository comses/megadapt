FROM rocker/rstudio:3.5.2

RUN apt-get update \
  && apt-get install -y zlib1g-dev libpng-dev \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /home/rstudio/code/src/r

# Install R packages
COPY ./src/r .
RUN R --vanilla --slave -f .Rprofile --args --bootstrap-packrat
RUN chown -R 1000:1000 /home/rstudio

