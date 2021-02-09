do.call(setPaths, spreadFitPaths)

spreadFitObjects <- list(
  fireBufferedListDT = simOutFireSenseDataPrep[["fireBufferedListDT"]],
  fireSense_annualSpreadFitCovariates = simOutFireSenseDataPrep[["fireSense_annualSpreadFitCovariates"]],
  fireSense_nonAnnualSpreadFitCovariates = simOutFireSenseDataPrep[["fireSense_nonAnnualSpreadFitCovariates"]],
  fireSense_spreadFormula = simOutFireSenseDataPrep[["fireSense_spreadFormula"]],
  firePolys = simOutFireSenseDataPrep[["firePolys"]],
  flammableRTM = simOutFireSenseDataPrep[["flammableRTM"]],
  spreadFirePoints = simOutFireSenseDataPrep[["spreadFirePoints"]],
  studyArea = simOutFireSenseDataPrep[["studyArea"]],
  rasterToMatch = simOutFireSenseDataPrep[["rasterToMatch"]]
)

extremeVals <- 3
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
  c(rep("localhost", 90), rep("forcast01.local", 10))
} else {
  stop("please specify number of cores to use for spreadFit")
}

stopifnot(length(cores) == length(lower)*10) ## 10 populations per parameter for DEoptim

spreadFitParams <- list(
  fireSense_SpreadFit = list(
    # "cacheId_DE" = paste0("DEOptim_", studyAreaName), # This is NWT DEoptim Cache
    "cloudFolderID_DE" = cloudCacheFolderID,
    "cores" = cores,
    "debugMode" = FALSE,
    "iterDEoptim" = 150,
    "iterStep" = 150,
    "iterThresh" = 192L,
    "lower" = lower,
    "maxFireSpread" = max(0.28, upper[1]),
    "NP" = length(cores),
    "objFunCoresInternal" = 1L,
    "objfunFireReps" = 100,
    "rescaleAll" = TRUE,
    "trace" = 1,
    "SNLL_FS_thresh" = NULL, # NULL means 'autocalibrate' to find suitable threshold value
    "upper" = upper,
    "verbose" = TRUE,
    "visualizeDEoptim" = FALSE,
    "useCloud_DE" = useCloudCache,
    ".plotSize" = list(height = 1600, width = 2000)
  )
)

#add tags when it stabilizes
# rm(biomassMaps2001, biomassMaps2011)

fs_SpreadFit_file <- file.path(Paths$outputPath, paste0("fS_SpreadFit_", studyAreaName, ".qs"))
spreadSim <- simInit(times = list(start = 0, end = 1),
                     params = spreadFitParams,
                     modules = "fireSense_SpreadFit",
                     paths = spreadFitPaths,
                     objects = spreadFitObjects)
spreadOut <- spades(spreadSim)
saveSimList(Copy(spreadOut), fs_SpreadFit_file) ## TODO: fix issue loading simList
