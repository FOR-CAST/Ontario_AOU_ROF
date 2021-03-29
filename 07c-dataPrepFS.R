do.call(SpaDES.core::setPaths, paths2b)

source("05-google-ids.R")
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
  "rstLCC" = raster::crop(simOutBiomassMaps2001[["rstLCC"]], simOutBiomassMaps2001[["rasterToMatch"]]), ## NOTE: rstLCC based on rtml ??
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "standAgeMap2001" = simOutBiomassMaps2001[["standAgeMap"]],
  "standAgeMap2011" = simOutBiomassMaps2011[["standAgeMap"]],
  "studyArea" = simOutPreamble[["studyArea"]]
)

amc::.gc()

fSsimDataPrep <- file.path(Paths$outputPath, paste0("simOutFireSenseDataPrep_", studyAreaName, ".qs"))
simOutFireSenseDataPrep <- Cache(
  simInitAndSpades,
  times =  list(start = 2011, end = 2011),
  modules = "fireSense_dataPrepFit",
  params = parameters2b,
  objects = objects2b,
  paths = paths2b,
  .plots = NA,
  userTags = c("fireSense_dataPrepFit", studyAreaName)
)
saveSimList(simOutFireSenseDataPrep, fSsimDataPrep, fileBackend = 2)

if (isTRUE(uplaod2GDrive)) {
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fSsimDataPrep, path = gdriveURL, name = basename(fSsimDataPrep), verbose = TRUE)
    #googledrive::drive_put(media = aSsimDataPrep, path = gdriveURL, name = basename(aSsimDataPrep), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrep"]]), media = fSsimDataPrep)
    #googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrepArchive"]]), media = aSsimDataPrep)
  }
}

source("R/compareMDC.R") ## defines the compareMDC() function
ggMDC <- compareMDC(historicalMDC = simOutPreamble$historicalClimateRasters$MDC,
                    projectedMDC = simOutPreamble$projectedClimateRasters$MDC,
                    flammableRTM = fSsimDataPrep$flammableRTM)
fggMDC <- file.path(dataPrepPaths$outputPath, "figures", paste0("compareMDC_", studyAreaName, ".png"))
checkPath(dirname(fggMDC), create = TRUE)

ggsave(plot = ggMDC, filename = fggMDC)

if (isTRUE(firstRunMDCplots)) {
  googledrive::drive_upload(media = fggMDC, path = as_id(gdriveSims[["results"]]), name = basename(fggMDC), overwrite = TRUE)
}
