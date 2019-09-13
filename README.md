# megadapt

[![Build Status](https://travis-ci.com/comses/megadapt.svg?branch=master)](https://travis-ci.com/comses/megadapt)

CoMSES MEGADAPT repository for containerized megadapt models

## Setup

### Development

To create an interactive environment to work on developing megadapt run

```
docker-compose up -d
```

on the command line

Then go to `localhost:8787` in your web browser. Enter the default username
and password (rstudio and test respectively).

You should now see an rstudio environment.

Now source the megadapt project in RStudio (File > Open Project) and go to
`code > src > r > megadaptr` in the explorer and click `megadaptr.Rproj`.

Your R environment is now setup for you.

Press `Ctnl+Shift+b` to build and reload from `megadaptr` package from rstudio.

### High Performance Computing

Build the singularity images

```
cd deploy
singularity build --fakeroot megadapt-base.sif megadapt-base.def
singularity build --fakeroot megadapt.sif megadapt.def

```

Then copy the final image to your hpc environment and run

```
scp megadapt.sif <server>:~/.
```

Detailed instructions are in the packages vignette.

## Code Organization

- Scenarios folder (`src/r/scenarios`)

  Contains scenario specific setup information
- `megadaptr` folder (`src/r/megadaptr`)

  Contains generic code which should apply to many scenarios.

- `deploy` contains configuration to build the development environment and deploy to hpc with singularity

## Resources

- see https://github.com/comses/megadapt/wiki/Developer-Resources for tips on developing R packages and manipulating data
