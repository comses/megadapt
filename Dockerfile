FROM rocker/rstudio:3.5.2

RUN apt-get update \
  && apt-get install -y zlib1g-dev libpng-dev libgeos-dev libgdal-dev \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /home/rstudio/code/src

# Install R packages
COPY --chown=rstudio:rstudio ./src/packrat packrat
COPY --chown=rstudio:rstudio ./src/.Rprofile .Rprofile
RUN R --vanilla --slave -f .Rprofile --args --bootstrap-packrat
COPY --chown=rstudio:rstudio ./src/r r 
COPY --chown=rstudio:rstudio ./src/ABM_Rversion.Rproj .
COPY --chown=rstudio:rstudio ./src/.lintr .lintr
RUN chown -R rstudio:rstudio /home/rstudio
