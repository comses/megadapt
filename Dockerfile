FROM rocker/rstudio:3.5.2

RUN apt-get update && apt-get install -y zlib1g-dev libpng-dev

# Install R packages
COPY ./src/r /home/rstudio/code/src/r
#RUN cd /home/rstudio/code && R --vanilla --slave -f .Rprofile --args --bootstrap-packrat
RUN chown -R 1000:1000 /home/rstudio

WORKDIR /home/rstudio/code/src/r
