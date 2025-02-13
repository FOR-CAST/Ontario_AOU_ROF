# project basics ------------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT, RENV_PATHS_CACHE, TMPDIR, etc.
if (file.exists("Ontario_AOU_ROF.Renviron")) readRenviron("Ontario_AOU_ROF.Renviron") ## database credentials

source("01-global-vars.R")

## packages, paths and options ----------------------------------------------------------------

library(data.table)
library(plyr)
library(pryr)
library(future.callr)
library(googledrive)
library(httr)

library(SpaDES.config)
library(SpaDES.core)

prjDir <- SpaDES.config::findProjectPath()

stopifnot(identical(prjDir, getwd()))

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cloud.r-project.org")
)

workflowtools::check_project_packages(prjDir)

# configure project ---------------------------------------------------------------------------

## TODO: implement exptTbl stuff to pass values to config

box::use(box/onnrv)

config <- onnrv$onnrvConfig$new(
  projectPath = prjDir,
  climateGCM = .climateGCM, climateSSP = .climateSSP,
  fireCause = .fireCause, fireModel = .fireModel, frpType = .fireRegimePolysType,
  mode = .mode, nrvType = .nrvType, rep = .rep, res = .res,
  studyAreaName = .studyAreaName
)$update()$validate()

## apply study area context settings here
source("02-studyArea-config.R")
config$args <- config.studyArea$args
config$modules <- modifyList(config$modules, config.studyArea$modules) ## TODO: update in SpaDES.config
config$options <- config.studyArea$options
config$params <- config.studyArea$params
config$paths <- config.studyArea$paths
config$update()$validate()

## apply user and machine context settings here
source("02-user-config.R")
config$args <- config.user$args
config$options <- config.user$options
config$params <- config.user$params
config$paths <- config.user$paths
config$update()$validate()

# print run info ------------------------------------------------------------------------------
SpaDES.config::printRunInfo(config$context)
names(config$modules)

# project paths -------------------------------------------------------------------------------

stopifnot(identical(checkPath(config$paths[["projectPath"]]), prjDir))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

# project options -----------------------------------------------------------------------------

## TODO: why is SpaDES.core / reproducible not setting this correctly?
data.table::setDTthreads(config$params[[".globals"]][[".useParallel"]])

opts <- SpaDES.config::setProjectOptions(config)

sf::sf_proj_network(TRUE)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "eastern-boreal", tryEmail = config$args[["cloud"]][["googleUser"]])

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, SpaDES.config::paths4spades(config$paths))

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

switch(
  config$context[["mode"]],
  development = {
    config$args[["usePrerun"]] <- FALSE
  },
  postprocess = {
    config$args[["usePrerun"]] <- TRUE
  },
  production = {
    if (config$context[["rep"]] == 1) {
      config$args[["usePrerun"]] <- FALSE
    } else {
      config$args[["usePrerun"]] <- TRUE
    }
  },
  config$args[["usePrerun"]] <- TRUE
)

## TODO: pull this into the block above
# if ("fit" %in% config$context[["mode"]]) {
#   config$args[["usePrerun"]] <- FALSE
#   config$args[["reupload"]] <- TRUE
# } else {
#   config$args[["usePrerun"]] <- TRUE
#   config$args[["reupload"]] <- FALSE
# }

if (!"postprocess" %in% config$context[["mode"]]) {
  source("06-studyArea.R")
  source(paste0("07-dataPrep-", config$context[["fireModel"]], ".R"))

  if ("fit" %in% config$context[["mode"]]) {
    config$params[[".globals"]][["reps"]] <- 1 ## TODO: testing only
    for (i in config$params[[".globals"]][["reps"]]) {
      config$context[["rep"]] <- i
      config$update()$validate()

      config$params[["fireSense_SpreadFit"]][["rep"]] <- i

      logPath <- checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below
      prjPaths <- SpaDES.config::paths4spades(config$paths)

      ## prerun all spreadfits, for use with main sim runs on another machine

      if (file.exists("Rplots.pdf")) {
        unlink("Rplots.pdf")
      }

      do.call(SpaDES.core::setPaths, SpaDES.config::paths4spades(config$paths))

      if (config$context[["fireModel"]] == "firesense") {
        source("08-fireSense_fit.R")
      }

      if (file.exists("Rplots.pdf")) {
        file.rename("Rplots.pdf", file.path(figPath, sprintf("spreadFit_plots_%s.pdf", config$context[["runName"]])))
      }
    }
  } else {
    source(paste0("09-main-", config$context[["fireModel"]], ".R"))
  }
} else {
  source("06-studyArea.R")
  source("10-post-processing.R") ## TODO: run B_sppData here
}

relOutputPath <- SpaDES.config:::.getRelativePath(config$paths[["outputPath"]], prjDir)
rrFile <- file.path(relOutputPath, "INFO.md")
cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
cat(workflowtools::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

## cleanup
DBI::dbDisconnect(getOption("reproducible.conn"))
