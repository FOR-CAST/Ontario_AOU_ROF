.starttime <- Sys.time()

if (file.exists(".Renviron")) readRenviron(".Renviron")

Require::Require("config")

cacheDir <- config::get("paths")[["cachedir"]]
cacheFormat <- config::get("cacheformat")
climateGCM <- config::get("climategcm")
climateSSP <- as.numeric(config::get("climatessp"))
cloudCacheFolderID <- config::get("cloud")[["cachedir"]]
codeChecks <- config::get("codechecks")
delayStart <- config::get("delaystart")
fitUsing <- if (grepl("for-cast[.]ca", Sys.info()[["nodename"]])) 3 else 0
messagingNumCharsModule <- config::get("messagingNumCharsModule")
newGoogleIDs <- FALSE ## gets rechecked/updated for each script (06, 07x, 08x) based on script 05
nReps <- config::get("nreps")
reproducibleAlgorithm <- config::get("reproduciblealgorithm")
resolution <- as.integer(config::get("resolution"))
reupload <- config::get("reupload")
run <- config::get("run")
scratchDir <- config::get("paths")[["scratchdir"]]
sppEquivCol <- "ON"
studyAreaName <- if (grepl("AOU", runName)) {
  if (grepl("test", runName)) {
    "AOU_test"
  } else {
    "AOU"
  }
} else if (grepl("ROF", runName)) {
  if (grepl("test", runName)) {
    "ROF_test" ## TODO: enable test areas in preamble for RoF
  } else {
    "ROF"
  }
} else {
  stop("runName must contain one of 'AOU' or 'ROF'.")
}
useCloudCache <- config::get("cloud")[["usecloud"]]
useLandR.CS <- if (grepl("ROF", studyAreaName)) FALSE else config::get("uselandrcs")
useMemoise <- config::get("usememoise")
usePlot <- config::get("plot")
userInputPaths <- config::get("inputpaths")
usePrerun <- config::get("useprerun")
useRequire <- config::get("userequire")
useTerra <- config::get("useterra")
.plotInitialTime <- if (isTRUE(usePlot)) 2011 else NA

if (!exists("runName")) {
  runName <- sprintf("%s_%s_SSP%03d_run%02d", studyAreaName, climateGCM, climateSSP, run)
} else {
  chunks <- strsplit(runName, "_")[[1]]
  climateSSP <- substr(chunks[length(chunks) - 2], 4, 6) ## TODO: confirm
  climateGCM <- if (grepl("ensemble", runName)) paste0(chunks[2], "_", chunks[3]) else chunks[2]
  studyAreaName <- chunks[1]
  run <- as.numeric(substr(chunks[length(chunks)], 4, 5))
}

firstRunMDCplots <- if (run == 1 && reupload) TRUE else FALSE
firstRunIgnitionFit <- if (run == 1) TRUE else FALSE
