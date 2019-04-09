# megadapt

[![Build Status](https://travis-ci.org/comses/megadapt.svg?branch=master)](https://travis-ci.org/comses/megadapt)

CoMSES MEGADAPT repository for containerized megadapt models

## Setup

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

## Code Organization

- Scenarios folder (`src/r/scenarios`)

  Contains scenario specific setup information
- `megadaptr` folder (`src/r/megadaptr`)

  Contains generic code which should apply to many scenarios.

## Resources

- see https://github.com/comses/megadapt/wiki/Developer-Resources for tips on developing R packages and manipulating data
