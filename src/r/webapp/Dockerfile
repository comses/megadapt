FROM rocker/shiny-verse:3.5.3

WORKDIR /tmp

COPY ./deploy/Rprofile.site /usr/local/lib/R/etc/
COPY ./deploy/apt_install.sh ./deploy/r_package_install.R ./deploy/r_package_manifest.csv /tmp/
RUN apt-get update \
  && ./apt_install.sh \
  && rm -rf /var/lib/apt/lists/* /var/tmp/*
RUN Rscript r_package_install.R

COPY /data /srv/data
COPY ./src/r/megadaptr /tmp/
RUN R CMD INSTALL .

RUN touch /in_docker && rm -rf /srv/shiny-server/* && chown -R shiny:shiny /srv/
USER shiny
COPY --chown=shiny:shiny ./src/r/webapp/src /srv/shiny-server
RUN cd /srv/shiny-server && Rscript bootstrap.R

WORKDIR /srv/shiny-server
