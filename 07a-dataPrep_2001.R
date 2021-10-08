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

dataPrepModules <- if (isTRUE(useLandR.CS)) {
  list(
    "Biomass_speciesData",
    "Biomass_borealDataPrep",
    "Biomass_speciesParameters"
  )
} else {
  list(
    "Biomass_speciesData",
    "Biomass_borealDataPrep"
  )
}

dataPrepParams2001 <- list(
  .globals = list("dataYear" = 2001),
  Biomass_borealDataPrep = list(
    # "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup))),
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover | ecoregionGroup))),
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "exportModels" = "all",
    "fixModelBiomass" = TRUE,
    "forestedLCCClasses" = 1:6, ## using LCC2010
    "LCCClassesToReplaceNN" = numeric(0), ## using LCC2010
    "pixelGroupAgeClass" = dataPrep$pixelGroupAgeClass,
    "speciesTableAreas" = c("WestON"),
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "sppEquivCol" = sppEquivCol,
    "subsetDataBiomassModel" = dataPrep$subsetDataBiomassModel,
    "useCloudCacheForStats" = useCloudCache,
    ".plots" = c("object", "png", "raw"),
    ".studyAreaName" = paste0(studyAreaName, 2001),
    ".useCache" = c(".inputObjects", "init")
  ),
  Biomass_speciesData = list(
    #"dataYear" = 2001, ## passed globally
    "sppEquivCol" = simOutPreamble$sppEquivCol,
    "types" = c("KNN", "ONFRI"), ## TODO: use CASFRIv5?
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = paste0(studyAreaName, 2001),
    #".useCache" = FALSE
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
  cloudFolderID = cloudCacheFolderID,
  #nonTreePixels = simOutPreamble[["nonTreePixels"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  rstLCC = simOutPreamble[["LCC"]],
  speciesTable = simOutPreamble[["speciesTable"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap = simOutPreamble[["standAgeMap2001"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

fbiomassMaps2001 <- file.path(Paths$outputPath, paste0("biomassMaps2001_", studyAreaName, ".qs"))
if (isTRUE(usePrerun) & isFALSE(upload_biomassMaps2001)) {
  if (!file.exists(fbiomassMaps2001)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2001), path = fbiomassMaps2001)
  }
  biomassMaps2001 <- loadSimList(fbiomassMaps2001)

  ## TODO: temp until bug in qs resolved
  simOutBiomassMaps2001$cohortData <- as.data.table(simOutBiomassMaps2001$cohortData)
  simOutBiomassMaps2001$minRelativeB <- as.data.table(simOutBiomassMaps2001$minRelativeB)
  simOutBiomassMaps2001$pixelFateDT <- as.data.table(simOutBiomassMaps2001$pixelFateDT)
  simOutBiomassMaps2001$species <- as.data.table(simOutBiomassMaps2001$species)
  simOutBiomassMaps2001$speciesEcoregion <- as.data.table(simOutBiomassMaps2001$speciesEcoregion)
  simOutBiomassMaps2001$sppEquiv <- as.data.table(simOutBiomassMaps2001$sppEquiv)
  simOutBiomassMaps2001$sufficientLight <- as.data.frame(simOutBiomassMaps2001$sufficientLight)
  ## end TODO
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
  saveSimList(simOutBiomassMaps2001, fBiomassMaps2001, fileBackend = 2)

  if (isTRUE(upload_biomassMaps2001)) {
    fdf <- googledrive::drive_put(media = fbiomassMaps2001, path = gdriveURL, name = basename(fbiomassMaps2001))
    gid_biomassMaps2001 <- fdf$id
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "biomassMaps2001", run = NA, gid = gid_biomassMaps2001),
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
