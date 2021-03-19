do.call(SpaDES.core::setPaths, paths2b)

parameters2b <- list(
  fireSense_dataPrepFit = list(
    ".studyAreaName" = studyAreaName,
    "fireYears" = 2001:2019, # this will be fixed to post kNN only
    "sppEquivCol" = "ON",
    "useCentroids" = TRUE,
    ".useCache" = ".inputObjects",
    "whichModulesToPrepare" = c("fireSense_SpreadFit", "fireSense_IgnitionFit", "fireSense_EscapeFit")
  )
)

objects2b <- list(
  ".runName" = runName,
  "cohortData2001" = simOutDataPrep2001[["cohortData"]],
  "cohortData2011" = simOutDataPrep2011[["cohortData"]],
  "historicalClimateRasters" = simOutPreamble[["historicalClimateRasters"]],
  "pixelGroupMap2001" = simOutDataPrep2001[["pixelGroupMap"]],
  "pixelGroupMap2011" = simOutDataPrep2011[["pixelGroupMap"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rstLCC" = crop(simOutDataPrep2001[["rstLCC"]], simOutDataPrep2001[["rasterToMatch"]]), ## NOTE: rstLCC based on rtml ??
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "standAgeMap2001" = simOutDataPrep2001[["standAgeMap"]],
  "standAgeMap2011" = simOutDataPrep2011[["standAgeMap"]],
  "studyArea" = simOutPreamble[["studyArea"]]
)

amc::.gc()

dataPrepFileFS <- file.path(Paths$inputPath, paste0("simOutDataPrep_", studyAreaName, ".qs"))
simOutFireSenseDataPrep <- Cache(simInitAndSpades,
                                 times =  list(start = 2011, end = 2011),
                                 params = parameters2b,
                                 objects = objects2b,
                                 paths = paths2b,
                                 modules = "fireSense_dataPrepFit",
                                 userTags = c("fireSense_dataPrepFit", studyAreaName)
)
saveSimList(simOutFireSenseDataPrep, dataPrepFileFS, fileBackend = 2)

## TODO: update this
if (isTRUE(newGoogleIDs)) {
  googledrive::drive_put(media = fsimOutPreamble, path = gdriveURL, name = basename(fsimOutPreamble), verbose = TRUE)
  #googledrive::drive_put(media = asimOutPreamble, path = gdriveURL, name = basename(asimOutPreamble), verbose = TRUE)
} else {
  googledrive::drive_update(file = as_id(gdriveSims[["simOutPreamble"]]), media = fsimOutPreamble)
  #googledrive::drive_update(file = as_id(gdriveSims[["simOutPreambleArchive"]]), media = asimOutPreamble)
}
