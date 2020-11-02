library(sf)
library(sp)
library(reproducible)

cacheDir <- checkPath("cache", create = TRUE)
inputDir <- checkPath("inputs", create = TRUE)
outputDir <- checkPath("outputs", create = TRUE)
