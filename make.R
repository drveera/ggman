#!/bin/env Rscript

this.dir <- dirname("make.R")
setwd(this.dir)



library(devtools)

document()
build()
install()
