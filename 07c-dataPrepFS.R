do.call(SpaDES.core::setPaths, paths2b)

source("05-prerun.R")
newGoogleIDs <- gdriveSims[["fSsimDataPrep"]] == ""

parameters2b <- list(
  fireSense_dataPrepFit = list(
    ".studyAreaName" = studyAreaName,
    ".useCache" = ".inputObjects",
    "fireYears" = 2001:2019, ## this will be fixed to post kNN only
    "sppEquivCol" = "ON",
    "useCentroids" = TRUE,
    "whichModulesToPrepare" = c("fireSense_SpreadFit", "fireSense_IgnitionFit", "fireSense_EscapeFit")
  )
)

objects2b <- list(
  ".runName" = runName,
  "cohortData2001" = simOutBiomassMaps2001[["cohortData"]],
  "cohortData2011" = simOutBiomassMaps2011[["cohortData"]],
  "historicalClimateRasters" = simOutPreamble[["historicalClimateRasters"]],
  "pixelGroupMap2001" = simOutBiomassMaps2001[["pixelGroupMap"]],
  "pixelGroupMap2011" = simOutBiomassMaps2011[["pixelGroupMap"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rstLCC" = crop(simOutBiomassMaps2001[["rstLCC"]], simOutBiomassMaps2001[["rasterToMatch"]]), ## NOTE: rstLCC based on rtml ??
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "standAgeMap2001" = simOutBiomassMaps2001[["standAgeMap"]],
  "standAgeMap2011" = simOutBiomassMaps2011[["standAgeMap"]],
  "studyArea" = simOutPreamble[["studyArea"]]
)

amc::.gc()

fFSdataPrep <- file.path(Paths$inputPath, paste0("simOutBiomassMaps_", studyAreaName, ".qs"))
simOutFireSenseDataPrep <- Cache(
  simInitAndSpades,
  times =  list(start = 2011, end = 2011),
  params = parameters2b,
  objects = objects2b,
  paths = paths2b,
  modules = "fireSense_dataPrepFit",
  userTags = c("fireSense_dataPrepFit", studyAreaName)
)
saveSimList(simOutFireSenseDataPrep, fFSdataPrep, fileBackend = 2)

if (isTRUE(newGoogleIDs)) {
  googledrive::drive_put(media = fFSdataPrep, path = gdriveURL, name = basename(fFSdataPrep), verbose = TRUE)
  #googledrive::drive_put(media = aFSsimDataPrep, path = gdriveURL, name = basename(aFSsimDataPrep), verbose = TRUE)
} else {
  googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrep"]]), media = fFSdataPrep)
  #googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrepArchive"]]), media = aFSsimDataPrep)
}
