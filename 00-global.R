# project basics ------------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## RENV_PATHS_CACHE etc.
if (file.exists("Ontario_AOU_ROF.Renviron")) readRenviron("Ontario_AOU_ROF.Renviron") ## database credentials

.ncores <- min(parallelly::availableCores(constraints = "connections") / 2, 32L)

.nodename <- SpaDES.config::machine()
.user <- SpaDES.config::user()

###### allow setting run context info from outside this script (e.g., bash script) -----------------
if (exists(".mode", .GlobalEnv)) {
  stopifnot(all(.mode %in% c("development", "fit", "postprocess", "production")))
} else {
  .mode <- c("development")

  if (.user %in% c("achubaty") && grepl("for-cast[.]ca", .nodename)) {
   .mode <- append(.mode, "fit")
  }
}

if (exists(".climateGCM", .GlobalEnv)) {
  stopifnot(.climateGCM %in% c("CanESM5", "CNRM-ESM2-1"))
} else {
  .climateGCM <- "CanESM5"
}

if (exists(".climateSSP", .GlobalEnv)) {
  stopifnot(.climateSSP %in% c(245, 370, 585))
} else {
  .climateSSP <- 370
}

if (exists(".rep", .GlobalEnv)) {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else as.integer(.rep)
} else {
  .rep <- if ("postprocess" %in% .mode) NA_integer_ else 1L
}

if (exists(".res", .GlobalEnv)) {
  stopifnot(.res %in% c(125, 250))
} else {
  .res <- 250
}

if (!exists(".studyAreaName", .GlobalEnv)) {
  .studyAreaName <- "ON_AOU_6.2" ## ecoprovs in AOU: 6.1, 6.2, 6.5, 6.6 (omit 15.2)
  #.studyAreaName <- "ON_ROF_15.2" ## ecoprovs in ROF: 6.1, 6.2, 15.2 (omit 15.1)
  #.studyAreaName <- "ON_ROF_shield" ## ecozones in ROF: Boreal Shield, Hudson Plain
  #.studyAreaName <- "QC_boreal_6.2" ## ecoprovs in QC_boreal: 6.2, 6.3, 6.6
}

#####

prjDir <- SpaDES.config::findProjectPath()

stopifnot(identical(prjDir, getwd()))

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cloud.r-project.org")
)

## set new temp dir in scratch directory (existing /tmp too small for large callr ops in postprocessing)
## see https://github.com/r-lib/callr/issues/172
if (grepl("for-cast[.]ca", .nodename) && !grepl("larix", .nodename)) {
  newTmpDir <- file.path("/mnt/scratch", .user, basename(prjDir), "tmp")
  tmpdir::setTmpDir(newTmpDir, rmOldTempDir = TRUE)
}

## load packages ------------------------------------------------------------------------------

library(data.table)
library(plyr)
library(pryr)
library(SpaDES.config)
library(future.callr)
library(googledrive)
library(httr)
library(SpaDES.core)

# configure project ---------------------------------------------------------------------------

config <- SpaDES.config::useConfig(projectName = "LandRfS", projectPath = prjDir,
                                   climateGCM = .climateGCM, climateSSP = .climateSSP,
                                   mode = .mode, rep = .rep, res = .res,
                                   studyAreaName = .studyAreaName)
config$modules <- modifyList(config$modules, list(historicFires = "historicFires"))  ## TODO: update in SpaDES.config
config$validate()

## apply user and machine context settings here
source("02-studyArea-config.R")
config$args <- config.studyArea$args
config$modules <- modifyList(config$modules, config.studyArea$modules) ## TODO: update in SpaDES.config
config$options <- config.studyArea$options
config$params <- config.studyArea$params
config$paths <- config.studyArea$paths
config$update()
config$validate()

## apply user and machine context settings here
source("02-user-config.R")
config$args <- config.user$args
#config$modules <- config.user$modules ## no modules should differ among users/machines
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
names(config$modules)

# project paths -------------------------------------------------------------------------------
config$paths
stopifnot(identical(checkPath(config$paths[["projectPath"]]), prjDir))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

prjPaths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "eastern-boreal", tryEmail = config$args[["cloud"]][["googleUser"]])

## helper functions
# source("R/cache_helpers.R") ## TODO: remove; now in reproducible

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, prjPaths)

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

if (!"postprocess" %in% config$context[["mode"]]) {
  if ("fit" %in% config$context[["mode"]]) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE
  } else {
    config$args[["usePrerun"]] <- TRUE
    config$args[["reupload"]] <- FALSE
  }

  source("06-studyArea.R")
  source("07a-dataPrep_2001.R")

  if ("fit" %in% config$context[["mode"]]) {
    opt <- options(spades.memoryUseInterval = FALSE) ## TODO: periodically stalls during mem use setup; disable temporarily
  }
  source("07b-dataPrep_2011.R")
  source("07c-dataPrep_fS.R")

  source("08a-ignitionFit.R")
  source("08b-escapeFit.R")

  # if ("fit" %in% config$context[["mode"]]) {
  #   options(opt)
  # }

  if ("fit" %in% config$context[["mode"]]) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE

    for (i in config$params[[".globals"]][["reps"]]) {
      config$context[["rep"]] <- i
      config$update()
      config$validate()

      logPath <- checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below
      prjPaths <- SpaDES.config::paths4spades(config$paths)

      ## prerun all spreadfits, for use with main sim runs on another machine

      if (file.exists("Rplots.pdf")) {
        unlink("Rplots.pdf")
      }

      do.call(SpaDES.core::setPaths, prjPaths)

      source("08c-spreadFit.R")

      if (file.exists("Rplots.pdf")) {
        file.rename("Rplots.pdf", file.path(figPath, sprintf("spreadFit_plots_%s.pdf", config$context[["runName"]])))
      }
    }
  } else {
    source("08c-spreadFit.R")
    source("09-main-sim.R")
  }
} else {
  source("10-post-processing.R")
}

relOutputPath <- SpaDES.config:::.getRelativePath(prjPaths[["outputPath"]], prjDir)
rrFile <- file.path(relOutputPath, "INFO.md")
cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
cat(workflowtools::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

DBI::dbDisconnect(getOption("reproducible.conn"))
unlink(newTmpDir, recursive = TRUE)
