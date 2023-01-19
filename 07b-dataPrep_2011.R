gid_biomassMaps2011 <- gdriveSims[studyArea == studyAreaName & simObject == "biomassMaps2011", gid]
upload_biomassMaps2011 <- config$args[["reupload"]] | length(gid_biomassMaps2011) == 0

year <- 2011

dataPrepParams2011 <- dataPrepParams2001
dataPrepParams2011[[".globals"]][["dataYear"]] <- year
dataPrepParams2011[[".globals"]][[".studyAreaName"]] <- paste0(studyAreaName, year)
dataPrepParams2011[["Biomass_speciesData"]][["types"]] <- "KNN" ## TODO: is this correct? what year for ONFRI?

dataPrepOutputs2011 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(studyAreaName, "_",
                c("cohortData2011_fireSense.rds",
                  "pixelGroupMap2011_fireSense.rds",
                  "speciesLayers2011_fireSense.rds",
                  "standAgeMap2011_borealDataPrep.rds",
                  "rawBiomassMap2011_borealDataPrep.rds"))
)

dataPrepObjects[["standAgeMap"]] <- simOutPreamble[["standAgeMap2011"]]

fbiomassMaps2011 <- simFile(paste0("biomassMaps2011_", studyAreaName), config$paths[["outputPath"]], ext = "qs")
if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_biomassMaps2011)) {
  if (!file.exists(fbiomassMaps2011)) {
    googledrive::drive_download(file = as_id(gid_biomassMaps2011), path = fbiomassMaps2011)
  }
  biomassMaps2011 <- loadSimList(fbiomassMaps2011)

  ## TODO: fix these upstream
  biomassMaps2011[["sufficientLight"]] <- as.data.frame(biomassMaps2011[["sufficientLight"]])
} else {
  biomassMaps2011 <- Cache(
    simInitAndSpades,
    times = list(start = year, end = year),
    params = dataPrepParams2011,
    modules = dataPrepModules,
    objects = dataPrepObjects,
    paths = getPaths(),
    loadOrder = unlist(dataPrepModules),
    clearSimEnv = TRUE,
    # outputs = dataPrepOutputs2011,
    .plots = "png",
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    userTags = c("dataPrep2011", studyAreaName)
  )

  if (isTRUE(attr(biomassMaps2011, ".Cache")[["newCache"]])) {
    biomassMaps2011@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(biomassMaps2011, fbiomassMaps2011, fileBackend = 2)
  }

  if (isTRUE(upload_biomassMaps2011)) {
    fdf <- googledrive::drive_put(media = fbiomassMaps2011, path = as_id(gdriveURL), name = basename(fbiomassMaps2011))
    gid_biomassMaps2011 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "biomassMaps2011", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2011),
      gdriveSims
    )
  }
}

rm(dataPrepObjects, dataPrepOutputs2001, dataPrepParams2001, dataPrepOutputs2011, dataPrepParams2011)
