## 07a-dataPrep_2001 -------------------------------------------------------------------------------

source("05-google-ids.R")

gid_biomassMaps2001 <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "biomassMaps2001", gid]
upload_biomassMaps2001 <- config$args[["reupload"]] # | length(gid_biomassMaps2001) == 0

year <- 2001

dataPrepModules <- list(
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_borealDataPrep",
  "Biomass_speciesParameters"
) ## TODO: use config$modules

## TODO: why isn't updating .globals sufficient to update indiv mmodule values???
config$params <- list(
  .globals = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesData = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesFactorial = list(
    .plotInitialTime = year
  ),
  Biomass_borealDataPrep = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesParameters = list(
    .plotInitialTime = year
  )
)

dataPrepParams2001 <- list(
  .globals = config$params[[".globals"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]],
  Biomass_speciesFactorial = config$params[["Biomass_speciesFactorial"]],
  Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]],
  Biomass_speciesParameters = config$params[["Biomass_speciesParameters"]]
)

dataPrepObjects <- list(
  .runName = config$context[["runName"]],
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

dataPrepOutputs2001 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(config$context[["studyAreaName"]], "_",
                c("cohortData2001_fireSense.rds",
                  "pixelGroupMap2001_fireSense.rds",
                  "speciesLayers2001_fireSense.rds",
                  "standAgeMap2001_borealDataPrep.rds",
                  "rawBiomassMap2001_borealDataPrep.rds"))
)

fbiomassMaps2001 <- simFile(paste0("biomassMaps2001_", config$context[["studyAreaName"]]),
                            config$paths[["outputPath"]], ext = config$args[["fsimext"]])

if (isTRUE(config$args[["usePrerun"]]) && isFALSE(upload_biomassMaps2001)) {
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
    loadOrder = unlist(dataPrepModules),
    .plots = NA,
    useCloud = config$args[["cloud"]][["useCloud"]],
    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
    userTags = c("dataPrep2001", config$context[["studyAreaName"]])
  )

  if (isUpdated(biomassMaps2001) || isFALSE(config$args[["useCache"]])) {
    biomassMaps2001@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      biomassMaps2001,
      fbiomassMaps2001,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
  }

  if (isTRUE(upload_biomassMaps2001)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fbiomassMaps2001, path = as_id(gdriveURL), name = basename(fbiomassMaps2001))
    gid_biomassMaps2001 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "biomassMaps2001", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2001),
      gdriveSims
    )
  }
}

## restore original studyAreaName
config$params[[".globals"]][[".studyAreaName"]] <- config$context[["studyAreaName"]]
config$update()

## PLOTTING
if ("screen" %in% config$params[[".globals"]][[".plots"]]) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers2001[["speciesLayers"]])
}

## 07b-dataPrep_2011 -------------------------------------------------------------------------------

source("05-google-ids.R")

gid_biomassMaps2011 <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "biomassMaps2011", gid]
upload_biomassMaps2011 <- config$args[["reupload"]] # | length(gid_biomassMaps2011) == 0

year <- 2011

## TODO: why isn't updating .globals sufficient to update indiv mmodule values???
config$params <- list(
  .globals = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesData = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesFactorial = list(
    .plotInitialTime = year
  ),
  Biomass_borealDataPrep = list(
    dataYear = year,
    .plotInitialTime = year,
    .studyAreaName = paste0(config$context[["studyAreaName"]], "_", year)
  ),
  Biomass_speciesParameters = list(
    .plotInitialTime = year
  )
)

dataPrepParams2011 <- dataPrepParams2001

## begin param updates
dataPrepParams2011[[".globals"]][["dataYear"]] <- year
dataPrepParams2011[[".globals"]][[".plotInitialTime"]] <- year
dataPrepParams2011[[".globals"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesData"]][["dataYear"]] <- year
dataPrepParams2011[["Biomass_speciesData"]][[".plotInitialTime"]] <- year
dataPrepParams2011[["Biomass_speciesData"]][["types"]] <- "KNN" ## TODO: is this correct? what year for ONFRI?
dataPrepParams2011[["Biomass_speciesData"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesFactorial"]][[".plotInitialTime"]] <- year

dataPrepParams2011[["Biomass_borealDataPrep"]][["dataYear"]] <- year
dataPrepParams2011[["Biomass_borealDataPrep"]][[".plotInitialTime"]] <- year
dataPrepParams2011[["Biomass_borealDataPrep"]][[".studyAreaName"]] <- paste0(config$context[["studyAreaName"]], "_", year)

dataPrepParams2011[["Biomass_speciesParameters"]][[".plotInitialTime"]] <- year
## end pram updates

dataPrepObjects[["standAgeMap"]] <- simOutPreamble[["standAgeMap2011"]]

dataPrepOutputs2011 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(config$context[["studyAreaName"]], "_",
                c("cohortData2011_fireSense.rds",
                  "pixelGroupMap2011_fireSense.rds",
                  "speciesLayers2011_fireSense.rds",
                  "standAgeMap2011_borealDataPrep.rds",
                  "rawBiomassMap2011_borealDataPrep.rds"))
)

fbiomassMaps2011 <- simFile(paste0("biomassMaps2011_", config$context[["studyAreaName"]]),
                            config$paths[["outputPath"]], ext = config$args[["fsimext"]])

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
    loadOrder = unlist(dataPrepModules),
    useCloud = config$args[["cloud"]][["useCloud"]],
    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
    userTags = c("dataPrep2011", config$context[["studyAreaName"]])
  )

  if (isUpdated(biomassMaps2011) || isFALSE(config$args[["useCache"]])) {
    biomassMaps2011@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      biomassMaps2011,
      fbiomassMaps2011,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
  }

  if (isTRUE(upload_biomassMaps2011)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fbiomassMaps2011, path = as_id(gdriveURL), name = basename(fbiomassMaps2011))
    gid_biomassMaps2011 <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = config$context[["studyAreaName"]], simObject = "biomassMaps2011", runID = NA,
                 gcm = NA, ssp = NA, gid = gid_biomassMaps2011),
      gdriveSims
    )
  }
}

rm(dataPrepObjects, dataPrepOutputs2001, dataPrepParams2001, dataPrepOutputs2011, dataPrepParams2011)

## restore original studyAreaName
config$params[[".globals"]][[".studyAreaName"]] <- config$context[["studyAreaName"]]
config$update()

source("05-google-ids.R")

gid_fSsimDataPrep <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "fSsimDataPrep", gid]
upload_fSsimDataPrep <- config$args[["reupload"]] # | length(gid_fSsimDataPrep) == 0

## 07c-dataPrep_fS ---------------------------------------------------------------------------------

## WildFire raster - for now we will supply this data as WWH gang has not made it publicly available
# wildfire2020 <- prepInputs(
#   url = "https://drive.google.com/file/d/1Vc4cOY1jOS1y8P20S14nYBJWwkRj_SPL/",
#   targetFile = "Fire_1985-2020_ROF.dat",
#   fun = "raster::raster",
#   rasterToMatch = simOutPreamble$rasterToMatch,
#   alsoExtract = c("Fire_1985-2020_ROF.dat.ovr",
#                   "Fire_1985-2020_ROF.dat.aux.xml",
#                   "Fire_1985-2020_ROF.dat.vat.cpg",
#                   "Fire_1985-2020_ROF.hdr"),
#   destinationPath = config$paths[["inputPath"]]
# )

# wildfire2020 <- raster::setMinMax(wildfire2020)

fSdataPrepParams <- list(
  fireSense_dataPrepFit = config$params[["fireSense_dataPrepFit"]]
)

fSdataPrepParams[["fireSense_dataPrepFit"]][["forestedLCC"]] <- simOutPreamble[["fireSenseForestedLCC"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["missingLCCgroup"]] <- simOutPreamble[["missingLCCGroup"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["nonflammableLCC"]] <- simOutPreamble[["nonflammableLCC"]]
fSdataPrepParams[["fireSense_dataPrepFit"]][["sppEquivCol"]] <- simOutPreamble[["sppEquivCol"]]

simOutPreamble[["rasterToMatch"]] <- terra::mask(simOutPreamble[["rasterToMatch"]], simOutPreamble[["studyArea"]])
standAgeMap2001 <- postProcess(biomassMaps2001[["standAgeMap"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
standAgeMap2011 <- postProcess(biomassMaps2011[["standAgeMap"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
rstLCC <- postProcess(biomassMaps2011[["rstLCC"]], rasterToMatch = simOutPreamble[["rasterToMatch"]])
rstLCC[] <- as.integer(rstLCC[])

fSdataPrepObjects <- list(
  .runName = config$context[["runName"]],
  cohortData2001 = biomassMaps2001[["cohortData"]],
  cohortData2011 = biomassMaps2011[["cohortData"]],
  # fireRaster = wildfire2020,
  nonForestedLCCGroups = simOutPreamble[["nonForestLCCGroups"]],
  historicalClimateRasters = simOutPreamble[["historicalClimateRasters"]],
  pixelGroupMap2001 = biomassMaps2001[["pixelGroupMap"]],
  pixelGroupMap2011 = biomassMaps2011[["pixelGroupMap"]],
  historicalClimateRasters = simOutPreamble[["historicalClimateRasters"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rstLCC = rstLCC,
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap2001 = standAgeMap2001,
  standAgeMap2011 = standAgeMap2011,
  studyArea = simOutPreamble[["studyArea"]]
)

gc()

ffSsimDataPrep <- simFile(paste0("fSsimDataPrep_", config$context[["studyAreaName"]]),
                          config$paths[["outputPath"]], ext = config$args[["fsimext"]])

if (isTRUE(config$args[["usePrerun"]])) {
  if (!file.exists(ffSsimDataPrep)) {
    googledrive::drive_download(file = as_id(gid_fSsimDataPrep), path = ffSsimDataPrep)
  }
  fSsimDataPrep <- loadSimList(ffSsimDataPrep)
} else {
  fSsimDataPrep <- Cache(
    simInitAndSpades,
    times =  list(start = 2011, end = 2011),
    params = fSdataPrepParams,
    objects = fSdataPrepObjects,
    modules = "fireSense_dataPrepFit", ## TODO: use config$modules
    useCache = FALSE, ## TODO: error in .robustDigest: [subset] invalid name(s)
    useCloud = config$args[["cloud"]][["useCloud"]],
    cloudFolderID = config$args[["cloud"]][["cacheDir"]],
    userTags = c("fireSense_dataPrepFit", config$context[["studyAreaName"]])
  )

  if (isUpdated(fSsimDataPrep) || isFALSE(config$args[["useCache"]])) {
    fSsimDataPrep@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    ## TODO: saveSimList() now failing after resampling rasters for hindcast:
    ## error in evaluating the argument 'object' in selecting a method for function '.robustDigest':
    ##   [subset] invalid name(s)
    tryCatch({
      saveSimList(
        fSsimDataPrep,
        ffSsimDataPrep,
        inputs = FALSE,
        outputs = FALSE,
        cache = FALSE,
        files = FALSE
      )
    }, error = function(e) {
      message(crayon::red(e)) ## TODO: .robustDigest failure per above
    })
  }
}

if (isTRUE(upload_fSsimDataPrep)) {
  source("05-google-ids.R")

  fdf <- googledrive::drive_put(media = ffSsimDataPrep, path = as_id(gdriveURL), name = basename(ffSsimDataPrep))
  gid_fSsimDataPrep <- as.character(fdf$id)
  rm(fdf)
  gdriveSims <- update_googleids(
    data.table(studyArea = config$context[["studyAreaName"]], simObject = "fSsimDataPrep",  runID = NA,
               gcm = NA, ssp = NA, gid = gid_fSsimDataPrep),
    gdriveSims
  )

  source("R/upload_fSDatPrepFit_vegCoeffs.R") ## TODO: add to the module
}

rm(rstLCC, standAgeMap2001, standAgeMap2011)
