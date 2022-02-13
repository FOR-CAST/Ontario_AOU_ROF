## NOTE: 07a-dataPrep_2001.R and 07b-dataPrep_2011.R need to be run before this script
do.call(setPaths, dataPrepPaths)

gid_fSsimDataPrep <- gdriveSims[studyArea == studyAreaName & simObject == "fSsimDataPrep", gid]
upload_fSsimDataPrep <- reupload | length(gid_fSsimDataPrep) == 0

LCC2005_nonFlam <- c(0, 25, 30, 33, 36:39) ## original fireSense_dataPrepFit defaults
LCC_FN_nonFlam <- c(1:6, 7, 10:11, 21:24) ## TODO: re-eval 7, 12:14, 21:22 per Rmd

LCC2005_groups <- list(
  nonForest_highFlam = c(16:19, 22),
  nonForest_lowFlam = c(21, 23:24, 26:29, 31)
  ## nonForest_nonFlam = c(0, 25, 30, 33, 36:39)
)
LCC_FN_groups <- list(
  nonForest_highFlam = NULL, ## none
  nonForest_lowFlam = c(8, 13)
  ## nonForest_nonFlam = c(1:7, 11, 21:14)
)


#for reference
classes <- c("Clear Open Water" = 1, "Turbid Water" = 2, "Intertidal Mudflat" = 3,
             "Intertidal Marsh" = 4, "Supratidal Marsh" = 5, "Fresh Water Marsh" = 6,
             "Heath" = 7, "Thicket Swamp" = 8, "Coniferous Swamp" = 9, "Deciduous Swamp" = 10,
             "Open Fen" = 11, "Treed Fen" = 12, "Open Bog" = 13, "Treed Bog" = 14,
             "Sparse Treed" = 15, "Deciduous Treed" = 16, "Mixed Treed" = 17, "Coniferous Treed" = 18,
             "Disturbance - Non and sparse-woody" = 19, "Disturbance - Treed or shrub" = 20,
             "Sand/Gravel/Mine Tailings" = 21, "Bedrock" = 22, "Community/Infrastructure" = 23,
             "Agriculture" = 24, "Cloud/Shadow" = -9, "Other" = -99)
plot(biomassMaps2001$rstLCC, rain)
# must reclassify the LCC map


######WildFire raster - for now we will supply this data as WWH gang has not made it publicly available
wildfire2020 <- prepInputs(url = "https://drive.google.com/file/d/1Vc4cOY1jOS1y8P20S14nYBJWwkRj_SPL/view?usp=sharing",
                           targetFile = "Fire_1985-2020_ROF.dat",
                           fun = 'raster',
                           rasterToMatch = simOutPreamble$rasterToMatch,
                           alsoExtract = c("Fire_1985-2020_ROF.dat.ovr",
                                           "Fire_1985-2020_ROF.dat.aux.xml",
                                           "Fire_1985-2020_ROF.dat.vat.cpg",
                                           "Fire_1985-2020_ROF.hdr"),
                           destinationPath = dataPrepPaths$inputPath)

wildfire2020 <- setMinMax(wildfire2020)

fSdataPrepParams <- list(
  fireSense_dataPrepFit = list(
    ".studyAreaName" = studyAreaName,
    ".useCache" = ".inputObjects",
    "climateGCM" = climateGCM,
    "climateSSP" = climateSSP,
    "fireYears" = 2001:2020,
    "missingLCCgroup" = if (grepl("AOU", studyAreaName)) "nonForest_highFlam" else "nonForest_lowFlam",
    "nonflammableLCC" = if (grepl("AOU", studyAreaName)) LCC2005_nonFlam else LCC_FN_nonFlam,
    "nonForestedLCCgroups" = if (grepl("AOU", studyAreaName)) LCC2005_groups else LCC_FN_groups,
    "sppEquivCol" = simOutPreamble[["sppEquivCol"]],
    "useCentroids" = TRUE,
    "useFireRaster" = TRUE,
    "usePCA" = FALSE,
    "whichModulesToPrepare" = c("fireSense_IgnitionFit", "fireSense_EscapeFit", "fireSense_SpreadFit")
  )
)

simOutPreamble$rasterToMatch <- raster::mask(simOutPreamble$rasterToMatch, simOutPreamble$studyArea)
fSdataPrepObjects <- list(
  .runName = runName,
  cohortData2001 = biomassMaps2001[["cohortData"]],
  cohortData2011 = biomassMaps2011[["cohortData"]],
  fireRaster = wildfire2020,
  historicalClimateRasters = simOutPreamble[["historicalClimateRasters"]],
  pixelGroupMap2001 = biomassMaps2001[["pixelGroupMap"]],
  pixelGroupMap2011 = biomassMaps2011[["pixelGroupMap"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]], ## this needs to be masked
  rstLCC = biomassMaps2011[["rstLCC"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  standAgeMap2001 = biomassMaps2001[["standAgeMap"]],
  standAgeMap2011 = biomassMaps2011[["standAgeMap"]],
  studyArea = simOutPreamble[["studyArea"]]
)

invisible(replicate(10, gc()))

ffSsimDataPrep <- simFile(paste0("fSsimDataPrep_", studyAreaName), Paths$outputPath, ext = simFileFormat)
if (isTRUE(usePrerun) & isFALSE(upload_fSsimDataPrep)) {
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
    paths = dataPrepPaths,
    modules = "fireSense_dataPrepFit",
    .plots = NA,
    #useCloud = useCloudCache,
    #cloudFolderID = cloudCacheFolderID,
    userTags = c("fireSense_dataPrepFit", studyAreaName)
  )
  saveSimList(fSsimDataPrep, ffSsimDataPrep, fileBackend = 2)

  if (isTRUE(upload_fSsimDataPrep)) {
    fdf <- googledrive::drive_put(media = ffSsimDataPrep, path = gdriveURL, name = basename(ffSsimDataPrep))
    gid_fSsimDataPrep <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "fSsimDataPrep",  runID = NA,
                 gcm = NA, ssp = NA, gid = gid_fSsimDataPrep),
      gdriveSims
    )
  }

  source("R/upload_spreadFit_coeffs.R")
}

if (isTRUE(firstRunMDCplots)) {
  stopifnot(packageVersion("fireSenseUtils") >= "0.0.4.9082") ## compareMDC() now in fireSenseUtils
  ggMDC <- fireSenseUtils::compareMDC(
    historicalMDC = simOutPreamble$historicalClimateRasters$MDC,
    projectedMDC = simOutPreamble$projectedClimateRasters$MDC,
    flammableRTM = fSsimDataPrep$flammableRTM
  )
  fggMDC <- file.path(dataPrepPaths$outputPath, "figures", paste0("compareMDC_", studyAreaName, "_",
                                                                  climateGCM, "_", climateSSP, ".png"))
  checkPath(dirname(fggMDC), create = TRUE)

  ggplot2::ggsave(plot = ggMDC, filename = fggMDC)

  googledrive::drive_put(
    media = fggMDC,
    path = unique(as_id(gdriveSims[studyArea == studyAreaName & simObject == "results", gid])),
    name = basename(fggMDC)
  )
}
