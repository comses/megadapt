FROM rocker/rstudio:3.5.3

RUN apt-get update \
  && apt-get install -y zlib1g-dev libpng-dev libgeos-dev libgdal-dev \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY ./src/.R /home/rstudio

WORKDIR /home/rstudio/code/src/r

# Install R packages
COPY ./src/r/Rprofile.site /usr/local/lib/R/etc/
RUN R -e "install.packages('devtools')"
COPY ./src/r/install.R ./src/r/package.csv ./
RUN Rscript install.R
COPY --chown=rstudio:rstudio ./src/r r
RUN chown -R rstudio:rstudio /home/rstudio
