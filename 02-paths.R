################################################################################
## Set paths for each part of the simulation
################################################################################

## preamble
paths1 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrepGIS", "preamble"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## species layers
paths2 <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrepGIS", "speciesLayers"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## boreal data prep
paths2a <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrepGIS", "borealDataPrep"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## fireSense data prep
paths2b <- list(
  ## use same cachePath for all data-prep steps before dynamic simulation
  cachePath = file.path(cacheDir, "dataPrepGIS", "fireSenseDataPrep"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

ignitionFitPaths <- list(
  cachePath = file.path(cacheDir, "fireSenseIgnitionFit"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

escapeFitPaths <- list(
  cachePath = file.path(cacheDir, "fireSenseEscapeFit"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

spreadFitPaths <- list(
  cachePath = file.path(cacheDir, "fireSenseSpreadFit"),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## main simulation
paths3 <- list(
  ## NOTE: use separate cachePath for each dynamic simulation
  cachePath = file.path(cacheDir, runName),
  modulePath = "modules",
  inputPath = "inputs",
  outputPath = file.path("outputs", runName)
)

## scratch
scratchDir <- checkPath(scratchDir, create = TRUE) ## from config

## tile path (same for all)
tilePath <- checkPath(file.path(paths1$outputPath, "tiles"), create = TRUE)

