FROM rocker/shiny-verse:3.5.3

WORKDIR /tmp

COPY ./deploy/Rprofile.site /usr/local/lib/R/etc/
COPY ./deploy/apt_install.sh ./deploy/r_package_install.R ./deploy/r_package_manifest.csv /tmp/
RUN apt-get update \
  && ./apt_install.sh \
  && rm -rf /var/lib/apt/lists/* /var/tmp/*
RUN Rscript r_package_install.R

COPY ./src/r/megadaptr /tmp/
RUN R CMD INSTALL .

USER shiny
COPY ./src/r/webapp/src /srv/shiny-server

WORKDIR /srv/shiny-server
