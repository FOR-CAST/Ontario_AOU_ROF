do.call(SpaDES.core::setPaths, paths2a)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["biomassMaps2011"]] == ""

year <- 2011

objects2a_2011 <- objects2a_2001
objects2a_2011[["standAgeMap"]] <- simOutPreamble[["ageMap2011"]]

outputs2a_2011 <- data.frame(
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

parameters2a_2011 <- parameters2a_2001
parameters2a_2011$Biomass_speciesData$types <- "KNN2011"
parameters2a_2011$Biomass_speciesData$.studyAreaName <- paste0(studyAreaName, year)
parameters2a_2011$Biomass_borealDataPrep$.studyAreaName <- paste0(studyAreaName, year)

fBiomassMaps2011 <- file.path(Paths$inputPath, paste0("simOutDataPrep_", studyAreaName, "_", year, ".qs"))
if (isTRUE(usePrerun)) {
  simOutBiomassMaps2011 <- loadSimList(fBiomassMaps2011)

  ## TODO: temp until bug in qs resolved
  simOutBiomassMaps2011$cohortData <- as.data.table(simOutBiomassMaps2011$cohortData)
  simOutBiomassMaps2011$minRelativeB <- as.data.table(simOutBiomassMaps2011$minRelativeB)
  simOutBiomassMaps2011$pixelFateDT <- as.data.table(simOutBiomassMaps2011$pixelFateDT)
  simOutBiomassMaps2011$species <- as.data.table(biomassMaps2001$species)
  simOutBiomassMaps2011$speciesEcoregion <- as.data.table(simOutBiomassMaps2011$speciesEcoregion)
  simOutBiomassMaps2011$sppEquiv <- as.data.table(simOutBiomassMaps2011$sppEquiv)
  simOutBiomassMaps2011$sufficientLight <- as.data.frame(simOutBiomassMaps2011$sufficientLight)
  ## end TODO
} else {
  simOutBiomassMaps2011 <- Cache(
    simInitAndSpades,
    times = list(start = year, end = year),
    params = parameters2a_2011,
    modules = modules2a,
    objects = objects2a_2011,
    loadOrder = unlist(modules2a),
    omitArgs = c("debug", "paths", ".plotInitialTime"),
    useCache = TRUE,
    useCloud = useCloudCache,
    cloudFolderID = cloudCacheFolderID,
    .plotInitialTime = year + .plotInitialTime,
    paths = paths2a,
    debug = 1
  )
  saveSimList(simOutBiomassMaps2011, fBiomassMaps2011, fileBackend = 2)
}

if (isTRUE(uplaod2GDrive)) {
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fBiomassMaps2011, path = gdriveURL, name = basename(fBiomassMaps2011), verbose = TRUE)
    #googledrive::drive_put(media = aBiomassMaps2011, path = gdriveURL, name = basename(aBiomassMaps2011), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["biomassMaps2011"]]), media = fBiomassMaps2011)
    #googledrive::drive_update(file = as_id(gdriveSims[["biomassMaps2011Archive"]]), media = aBiomassMaps2011)
  }
}
## PLOTTING
if (!is.na(.plotInitialTime)) {
  Plot(simOutSpeciesLayers2011$speciesLayers)
}
