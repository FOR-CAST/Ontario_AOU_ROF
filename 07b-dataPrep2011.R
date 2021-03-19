do.call(SpaDES.core::setPaths, paths2a)

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

## TODO: update this for both 2001 and 2011
if (isTRUE(newGoogleIDs)) {
  googledrive::drive_put(media = fBiomassMaps2011, path = gdriveURL, name = basename(fBiomassMaps2011), verbose = TRUE)
  #googledrive::drive_put(media = aBiomassMaps2011, path = gdriveURL, name = basename(aBiomassMaps2011), verbose = TRUE)
} else {
  googledrive::drive_update(file = as_id(gdriveSims[["BiomassMaps2011"]]), media = fBiomassMaps2011)
  #googledrive::drive_update(file = as_id(gdriveSims[["BiomassMaps2011Archive"]]), media = aBiomassMaps2011)
}
