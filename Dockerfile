FROM rocker/rstudio:3.5.2

ARG R_PACKRAT_CACHE_DIR

RUN apt-get update \
  && apt-get install -y zlib1g-dev libpng-dev libgeos-dev libgdal-dev \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /home/rstudio/code/src

# Install R packages
COPY ./src/.Rprofile .
COPY ./src/install.R .
RUN R -e 'install.packages("devtools")'
RUN Rscript install.R
COPY --chown=rstudio:rstudio ./src/r r
RUN chown -R rstudio:rstudio /home/rstudio
