do.call(setPaths, paths3)

times <- list(start = 2011, end = 2100)

dynamicModules <- list("fireSense_dataPrepPredict",
                       "fireSense",
                       "fireSense_IgnitionPredict",
                       "fireSense_EscapePredict",
                       "fireSense_SpreadPredict",
                       "Biomass_core",
                       "Biomass_regeneration")

dynamicObjects <- list(
  biomassMap = simOutBiomassMaps2011$biomassMap, ## unclear why Biomass_core needs this atm
  climateComponentsTouse = fSsimDataPrep$climateComponentsToUse,
  cohortData = fSsimDataPrep$cohortData2011,
  ecoregion = simOutBiomassMaps2011$ecoregion,
  ecoregionMap = simOutBiomassMaps2011$ecoregionMap,
  flammableRTM = fSsimDataPrep$flammableRTM,
  fireSense_IgnitionFitted = ignitionOut$fireSense_IgnitionFitted,
  fireSense_EscapeFitted = escapeOut$fireSense_EscapeFitted,
  fireSense_SpreadFitted = spreadOut$fireSense_SpreadFitted,
  covMinMax = spreadOut$covMinMax,
  landcoverDT = fSsimDataPrep$landcoverDT,
  nonForest_timeSinceDisturbance = fSsimDataPrep$nonForest_timeSinceDisturbance,
  ## this is the 2011 TSD - perhaps I should rename it in dataPrepFit to make it explicit?
  minRelativeB = simOutBiomassMaps2011$minRelativeB,
  PCAveg = fSsimDataPrep$PCAveg,
  pixelGroupMap = fSsimDataPrep$pixelGroupMap2011,
  projectedClimateLayers = simOutPreamble$projectedClimateRasters,
  rasterToMatch = simOutBiomassMaps2011$rasterToMatch,
  rasterToMatchLarge = simOutBiomassMaps2011$rasterToMatchLarge,
  species = simOutBiomassMaps2011$species,
  speciesEcoregion = simOutBiomassMaps2011$speciesEcoregion,
  speciesLayers = simOutBiomassMaps2011$speciesLayers, ## does Biomass_core actually need this?
  sppColorVect = simOutBiomassMaps2011$sppColorVect,
  sppEquiv = fSsimDataPrep$sppEquiv,
  studyArea = simOutBiomassMaps2011$studyArea,
  studyAreaLarge = simOutBiomassMaps2011$studyAreaLarge,
  studyAreaReporting = simOutBiomassMaps2011$studyAreaReporting,
  sufficientLight = simOutBiomassMaps2011$sufficientLight,
  terrainDT = fSsimDataPrep$terrainDT,
  vegComponentsToUse = fSsimDataPrep$vegComponentsToUse
)

rastersToSaveAnnually <- c(
  "ANPPMap",
  "burnMap",
  "fireSense_EscapePredicted",
  "fireSense_IgnitionPredicted",
  "fireSense_SpreadPredicted",
  "mortalityMap",
  "pixelGroupMap",
  "rstCurrentBurn",
  "simulatedBiomassMap"
)

annualRasters <- data.frame(
  expand.grid(
    objectName = rastersToSaveAnnually,
    saveTime = seq(times$start, times$end, 1),
    fun = "writeRaster",
    package = "raster"
  ),
  stringsAsFactors = FALSE
)
annualRasters$file <- paste0(annualRasters$objectName, "_", annualRasters$saveTime, ".tif")

objectsToSaveAnnually <- c(
  #"activePixelIndex", ## integer vector ## TODO: not an output -- it's in mod?
  "cohortData"       ## data.table
)

annualObjects <- data.frame(
  expand.grid(
    objectName = objectsToSaveAnnually,
    saveTime = seq(times$start, times$end, 1),
    fun = "qsave",
    package = "qs"
  ),
  stringsAsFactors = FALSE
)
annualObjects$file <- paste0(annualObjects$objectName, "_", annualObjects$saveTime, ".qs")

objectNamesToSaveAtEnd <- c("speciesEcoregion",
                            "species",
                            #"gcsModel", ## TODO: from LandR.CS
                            #"mcsModel", ## TODO: from LandR.CS
                            "simulationOutput",
                            "burnSummary")

finalYearOutputs <- data.frame(
  objectName = objectNamesToSaveAtEnd,
  saveTime = times$end,
  fun = "qsave",
  package = "qs",
  file = paste0(objectNamesToSaveAtEnd, ".qs"),
  stringsAsFactors = FALSE
)

dynamicOutputs <- rbind(annualRasters, annualObjects, finalYearOutputs)

dynamicParams <- list(
  Biomass_core = list(
    "sppEquivCol" = fSsimDataPrep@params$fireSense_dataPrepFit$sppEquivCol,
    "vegLeadingProportion" = 0, ## apparently sppColorVect has no mixed color
    ".plotInitialTime" = NA
  ),
  Biomass_regeneration = list(
    "fireInitialTime" = times$start + 1 #regeneration is scheduled earlier, so it starts in 2012
  ),
  fireSense_dataPrepPredict = list(
    "fireTimeStep" = 1,
    "sppEquivCol" = simOutPreamble$sppEquivCol,
    "whichModulesToPrepare" = c("fireSense_IgnitionPredict",
                                "fireSense_EscapePredict",
                                "fireSense_SpreadPredict"),
    "missingLCCgroup" = fSsimDataPrep@params$fireSense_dataPrepFit$missingLCCgroup
  ),
  fireSense_ignitionPredict = list(
    "rescaleFactor" = 1 / fSsimDataPrep@params$fireSense_dataPrepFit$igAggFactor^2
  ),
  fireSense = list(
    "whichModulesToPrepare" = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
    ".plotInterval" = NA,
    ".plotInitialTime" = NA,
    "plotIgnitions" = FALSE
  )
)

fsim <- file.path(Paths$outputPath, paste0(runName, ".qs"))
mainSim <- simInitAndSpades(
  times = times,
  modules = dynamicModules,
  objects = dynamicObjects,
  outputs = dynamicOutputs,
  params = dynamicParams,
  paths = paths3
)

saveSimList(
  sim = mainSim,
  filename = fsim,
  #filebackedDir = dfSsimDataPrep,
  fileBackend = 2
)
#archive::archive_write_dir(archive = afSsimDataPrep, dir = dfSsimDataPrep)

resultsDir <- file.path("outputs", runName)
#archive::archive_write_dir(archive = paste0(resultsDir, ".tar.gz"), dir = resultsDir) ## does'nt work
utils::tar(paste0(resultsDir, ".tar.gz"), resultsDir, compression = "gzip")
retry(quote(drive_upload(paste0(resultsDir, ".tar.gz"), as_id(gdriveSims[["results"]]), overwrite = TRUE)))

if (requireNamespace("slackr") & file.exists("~/.slackr")) {
  slackr::slackr_setup()
  slackr::slackr_msg(
    paste0("Simulation `", runName, "` completed on host `", Sys.info()[["nodename"]], "`."),
    channel = config::get("slackchannel"), preformatted = FALSE
  )
}
