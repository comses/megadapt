#!/bin/bash

#Removing all the files created by dagman and rfile, while saving rsp file:
mv run.dag.rsp SAVErun.dag.rsp
rm -f run.d*
mv SAVErun.dag.rsp run.dag.rsp

rm -f ABMats
rm -f Ya

cd ../
R CMD INSTALL --no-multiarch --with-keep.source megadaptr

cd submit
Rscript setup_megadapt.R
