do.call(setPaths, spreadFitPaths)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["spreadOut"]] == ""

extremeVals <- 4
lowerParamsNonAnnual <- rep(-extremeVals, times = ncol(simOutFireSenseDataPrep$fireSense_nonAnnualSpreadFitCovariates[[1]]) - 1)
lowerParamsAnnual <- c(-extremeVals, -extremeVals)
upperParamsNonAnnual <- rep(extremeVals, times = length(lowerParamsNonAnnual))
upperParamsAnnual <- c(extremeVals, extremeVals)
lowerParams <- c(lowerParamsAnnual, lowerParamsNonAnnual)
upperParams <- c(upperParamsAnnual, upperParamsNonAnnual)

## Spread log function bounds

## for logistic3p
# lower <- c(0.22, 0.001, 0.001, lowerParams)
# upper <- c(0.29, 10, 10, upperParams)

lower <- c(0.25, 0.2, 0.1, lowerParams)
upper <- c(0.286, 2, 4, upperParams)
dfT <- cbind(c("lower", "upper"), t(data.frame(lower, upper)))
message("Upper and Lower parameter bounds are:")
Require:::messageDF(dfT)

cores <- if (peutils::user("achubaty") && Sys.info()["nodename"] == "forcast02") {
  c(rep("localhost", 68), rep("forcast01.local", 32))
} else {
  stop("please specify number of cores to use for spreadFit")
}

stopifnot(length(cores) == length(lower)*10) ## 10 populations per parameter for DEoptim

spreadFitParams <- list(
  fireSense_SpreadFit = list(
    # "cacheId_DE" = paste0("DEOptim_", studyAreaName), # This is NWT DEoptim Cache
    "cloudFolderID_DE" = cloudCacheFolderID,
    "cores" = cores,
    "DEoptimTests" = c("adTest", "snll_fs"), # Can be one or both of c("adTest", "snll_fs")
    "doObjFunAssertions" = FALSE,
    "iterDEoptim" = 150,
    "iterStep" = 150,
    "iterThresh" = 396L,
    "lower" = lower,
    "maxFireSpread" = max(0.28, upper[1]),
    "mode" = if (isTRUE(firstRunSpreadFit)) c("fit", "visualize") else "fit", ##  "debug", or combo of "fit", "visualize"
    "NP" = length(cores),
    "objFunCoresInternal" = 1L,
    "objfunFireReps" = 100,
    #"onlyLoadDEOptim" = FALSE,
    "rescaleAll" = TRUE,
    "trace" = 1,
    "SNLL_FS_thresh" = NULL, # NULL means 'autocalibrate' to find suitable threshold value
    "upper" = upper,
    #"urlDEOptimObject" = "spreadOut_2021-02-10_Limit3_150_SNLL_FS_thresh_cNG42y", ## TODO: by studyArea
    "useCloud_DE" = useCloudCache,
    "verbose" = TRUE,
    "visualizeDEoptim" = FALSE,
    ".plot" = TRUE,
    ".plotSize" = list(height = 1600, width = 2000)
  )
)

spreadFitObjects <- list(
  fireBufferedListDT = simOutFireSenseDataPrep[["fireBufferedListDT"]],
  firePolys = simOutFireSenseDataPrep[["firePolys"]],
  fireSense_annualSpreadFitCovariates = simOutFireSenseDataPrep[["fireSense_annualSpreadFitCovariates"]],
  fireSense_nonAnnualSpreadFitCovariates = simOutFireSenseDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]],
  fireSense_spreadFormula = simOutFireSenseDataPrep[["fireSense_spreadFormula"]],
  flammableRTM = simOutFireSenseDataPrep[["flammableRTM"]],
  #parsKnown = spreadOut$fireSense_SpreadFitted$meanCoef,
  rasterToMatch = simOutFireSenseDataPrep[["rasterToMatch"]],
  spreadFirePoints = simOutFireSenseDataPrep[["spreadFirePoints"]],
  studyArea = simOutFireSenseDataPrep[["studyArea"]]
)

#add tags when it stabilizes
# rm(biomassMaps2001, biomassMaps2011)

#dspreadOut <- file.path(Paths$outputPath, paste0("ignitionOut_", studyAreaName)) %>%
#  checkPath(create = TRUE)
#aspreadOut <- paste0(dspreadOut, ".7z")
fspreadOut <- file.path(Paths$inputPath, paste0("fS_SpreadFit_", studyAreaName, ".qs"))
spreadOut <- simInitAndSpades(times = list(start = 0, end = 1),
                              params = spreadFitParams,
                              modules = "fireSense_SpreadFit",
                              paths = spreadFitPaths,
                              objects = spreadFitObjects)
saveSimList(
  spreadOut,
  fspreadOut,
  #filebackedDir = dspreadOut,
  fileBackend = 2 ## TODO use fileBackend = 1
)
#archive::archive_write_dir(archive = aspreadOut, dir = dspreadOut)

if (isTRUE(uplaod2GDrive)) {
  source("R/upload_spreadFit.R")

  if (isTRUE(newGoogleIDs)) {
    googledrive::drive_put(media = fspreadOut, path = gdriveURL, name = basename(fspreadOut), verbose = TRUE)
    #googledrive::drive_put(media = aspreadOut, path = gdriveURL, name = basename(aspreadOut), verbose = TRUE)
  } else {
    googledrive::drive_update(file = as_id(gdriveSims[["spreadOut"]]), media = fspreadOut)
    #googledrive::drive_update(file = as_id(gdriveSims[["spreadOutArchive"]]), media = aspreadOut)
  }
}

if (requireNamespace("slackr") & file.exists("~/.slackr")) {
  slackr::slackr_setup()
  slackr::slackr_msg(
    paste0("`fireSense_SpreadFit` for `", runName, "` completed on host `", Sys.info()[["nodename"]], "`."),
    channel = config::get("slackchannel"), preformatted = FALSE
  )
}
