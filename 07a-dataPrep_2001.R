gid_biomassMaps2001 <- gdriveSims[studyArea == studyAreaName & simObject == "biomassMaps2001", gid]
upload_biomassMaps2001 <- config$args[["reupload"]] | length(gid_biomassMaps2001) == 0

year <- 2001


dataPrepModules <- list(
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_borealDataPrep",
  "Biomass_speciesParameters"
) ## TODO: use config$modules

dataPrepParams2001 <- list(
  .globals = config$params[[".globals"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]]
)
dataPrepParams2001[[".globals"]][["dataYear"]] <- year
dataPrepParams2001[[".globals"]][[".plotInitialTime"]] <- year
dataPrepParams2001[[".globals"]][[".studyAreaName"]] <- paste0(studyAreaName, year)

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

fbiomassMaps2001 <- simFile(paste0("biomassMaps2001_", studyAreaName), config$paths[["outputPath"]], ext = "qs")
if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_biomassMaps2001)) {
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
    # outputs = dataPrepOutputs2001,
    .plots = NA,
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2001", studyAreaName)
  )

  if (isTRUE(attr(biomassMaps2001, ".Cache")[["newCache"]])) {
    biomassMaps2001@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(biomassMaps2001, fbiomassMaps2001, fileBackend = 2)
  }

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
