do.call(setPaths, dataPrepPaths)

gid_biomassMaps2001 <- gdriveSims[studyArea == studyAreaName & simObject == "biomassMaps2001", gid]
upload_biomassMaps2001 <- reupload | length(gid_biomassMaps2001) == 0

year <- 2001

dataPrep <- list(
  subsetDataBiomassModel = 50,
  pixelGroupAgeClass = 20,
  successionTimeStep = 10,
  useCache = TRUE
)

dataPrepModules <- list(
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_borealDataPrep",
  "Biomass_speciesParameters"
)

dataPrepParams2001 <- list(
  .globals = list("dataYear" = 2001),
  Biomass_borealDataPrep = list(
    # biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup))),
    biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover | ecoregionGroup))),
    ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
    exportModels = "all",
    fixModelBiomass = TRUE,
    forestedLCCClasses = simOutPreamble[["LandRforestedLCC"]],
    LCCClassesToReplaceNN = numeric(0),
    pixelGroupAgeClass = dataPrep[["pixelGroupAgeClass"]],
    speciesTableAreas = c("WestON"),
    speciesUpdateFunction = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    subsetDataBiomassModel = dataPrep[["subsetDataBiomassModel"]],
    useCloudCacheForStats = useCloudCache,
    .plots = c("object", "png", "raw"),
    .studyAreaName = paste0(studyAreaName, 2001),
    .useCache = FALSE #c(".inputObjects", "init")
  ),
  Biomass_speciesData = list(
    #dataYear = 2001, ## passed globally
    sppEquivCol = simOutPreamble[["sppEquivCol"]],
    types = if (studyAreaName == "AOU") c("KNN", "ONFRI") else "KNN",
    .plotInitialTime = .plotInitialTime,
    .studyAreaName = paste0(studyAreaName, 2001)
  ),
  Biomass_speciesFactorial = list(
    factorialSize = "small" ## TODO: use medium?
  ),
  Biomass_speciesParameters = list(
    constrainGrowthCurve = c(0, 1),
    constrainMaxANPP = c(3.0, 3.5),
    constrainMortalityShape = c(10, 25),
    GAMMiterations = 2,
    #GAMMknots[names(GAMMknots) %in% sppEquiv$LandR]
    GAMMknots = 3,
    minimumPlotsPerGamm = 65,
    quantileAgeSubset = 98,
    speciesFittingApproach = "focal",
    sppEquivCol = simOutPreamble$sppEquivCol
  )
)

dataPrepOutputs2001 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(studyAreaName, "_",
                c("cohortData2001_fireSense.rds",
                  "pixelGroupMap2001_fireSense.rds",
                  "speciesLayers2001_fireSense.rds",
                  "standAgeMap2001_borealDataPrep.rds",
                  "rawBiomassMap2001_borealDataPrep.rds"))
)

dataPrepObjects <- list(
  .runName = runName,
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  rstLCC = simOutPreamble[["LCC"]],
  standAgeMap = simOutPreamble[["standAgeMap2001"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

fbiomassMaps2001 <- simFile(paste0("biomassMaps2001_", studyAreaName), Paths$outputPath, ext = simFileFormat)
if (isTRUE(usePrerun) & isFALSE(upload_biomassMaps2001)) {
  if (!file.exists(fbiomassMaps2001)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2001), path = fbiomassMaps2001)
  }
  biomassMaps2001 <- loadSimList(fbiomassMaps2001)

  ## TODO: fix these upstream
  biomassMaps2001[["sufficientLight"]] <- as.data.frame(biomassMaps2001[["sufficientLight"]])
} else {
  biomassMaps2001 <- Cache(
    simInitAndSpades,
    times = list(start = year, end = year),
    params = dataPrepParams2001,
    modules = dataPrepModules,
    objects = dataPrepObjects,
    paths = getPaths(),
    loadOrder = unlist(dataPrepModules),
    # outputs = dataPrepOutputs2001,
    .plots = NA,
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2001", studyAreaName)
  )
  saveSimList(biomassMaps2001, fbiomassMaps2001, fileBackend = 2)

  if (isTRUE(upload_biomassMaps2001)) {
    fdf <- googledrive::drive_put(media = fbiomassMaps2001, path = as_id(gdriveURL), name = basename(fbiomassMaps2001))
    gid_biomassMaps2001 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "biomassMaps2001", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2001),
      gdriveSims
    )
  }
}

## PLOTTING
if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers2001$speciesLayers)
}
