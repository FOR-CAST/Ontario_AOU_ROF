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

ffSsimDataPrep <- file.path(Paths$outputPath, paste0("simOutFireSenseDataPrep_", studyAreaName, "_", climateScenario, ".qs"))
if (isTRUE(usePrerun)  && file.exists(ffSsimDataPrep)) {
  fSsimDataPrep <- loadSimList(ffSsimDataPrep)

  ## TODO: temporary until bug in qs is fixed
  fSsimDataPrep$fireSense_escapeCovariates <- as.data.table(fSsimDataPrep$fireSense_escapeCovariates)
  fSsimDataPrep$fireSense_annualSpreadFitCovariates <- lapply(fSsimDataPrep$fireSense_annualSpreadFitCovariates, as.data.table)
  fSsimDataPrep$fireBufferedListDT <- lapply(fSsimDataPrep$fireBufferedListDT, as.data.table)
  fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]] <- as.data.table(fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]])
  fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[2]] <- as.data.table(fSsimDataPrep$fireSense_nonAnnualSpreadFitCovariates[[2]])
  fSsimDataPrep$cohortData2011 <- as.data.table(fSsimDataPrep$cohortData2011)
  fSsimDataPrep$cohortData2001 <- as.data.table(fSsimDataPrep$cohortData2001)
  fSsimDataPrep$fireSense_ignitionCovariates <- as.data.table(fSsimDataPrep$fireSense_ignitionCovariates)
  fSsimDataPrep$landcoverDT <- as.data.table(fSsimDataPrep$landcoverDT)
  fSsimDataPrep$terrainDT <- as.data.table(fSsimDataPrep$terrainDT)
  fSsimDataPrep$sppEquiv <- as.data.table(fSsimDataPrep$sppEquiv)
  ## end TODO
} else {
  fSsimDataPrep <- Cache(
    simInitAndSpades,
    times =  list(start = 2011, end = 2011),
    modules = "fireSense_dataPrepFit",
    params = parameters2b,
    objects = objects2b,
    paths = paths2b,
    .plots = NA,
    userTags = c("fireSense_dataPrepFit", studyAreaName)
  )
  saveSimList(fSsimDataPrep, fSsimDataPrep, fileBackend = 2)
}

if (isTRUE(uplaod2GDrive)) {
  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = ffSsimDataPrep, path = gdriveURL, name = basename(ffSsimDataPrep), verbose = TRUE)
    #googledrive::drive_put(media = afSsimDataPrep, path = gdriveURL, name = basename(afSsimDataPrep), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrep"]]), media = ffSsimDataPrep)
    #googledrive::drive_update(file = as_id(gdriveSims[["fSsimDataPrepArchive"]]), media = aSsimDataPrep)
  }
}

source("R/compareMDC.R") ## defines the compareMDC() function
ggMDC <- compareMDC(historicalMDC = simOutPreamble$historicalClimateRasters$MDC,
                    projectedMDC = simOutPreamble$projectedClimateRasters$MDC,
                    flammableRTM = fSsimDataPrep$flammableRTM)
fggMDC <- file.path(paths2b$outputPath, "figures", paste0("compareMDC_", studyAreaName, "_", climateScenario, ".png"))
checkPath(dirname(fggMDC), create = TRUE)

ggsave(plot = ggMDC, filename = fggMDC)

if (isTRUE(firstRunMDCplots)) {
  googledrive::drive_upload(media = fggMDC, path = as_id(gdriveSims[["results"]]), name = basename(fggMDC), overwrite = TRUE)
}
