FROM rocker/rstudio:3.5.3

COPY deploy/apt_install.sh .
RUN apt-get update \
  && ./apt_install.sh \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY ./src/r /home/rstudio/code/src/r

WORKDIR /home/rstudio/code/src/r

# Install R packages
COPY ./deploy/Rprofile.site /usr/local/lib/R/etc/
RUN R -e "install.packages('devtools')"
COPY ./deploy/r_package_install.R ./deploy/r_package_manifest.csv ./
RUN Rscript r_package_install.R

COPY --chown=rstudio:rstudio ./src/r r
RUN chown -R rstudio:rstudio /home/rstudio
