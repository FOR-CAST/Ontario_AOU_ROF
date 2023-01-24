# project basics ------------------------------------------------------------------------------

if (file.exists("~/.Renviron")) readRenviron("~/.Renviron") ## GITHUB_PAT
if (file.exists("Ontario_AOU_ROF.Renviron")) readRenviron("Ontario_AOU_ROF.Renviron") ## database credentials

.ncores <- min(parallel::detectCores() / 2, 24L)
.nodename <- Sys.info()[["nodename"]]
.user <- Sys.info()[["user"]]

###### allow setting run context info from outside this script (e.g., bash script) -----------------
if (exists(".mode", .GlobalEnv)) {
  stopifnot(.mode %in% c("development", "postprocess", "production"))
} else {
  .mode <- c("development")

  ## TODO: allow "fit" in config
  # if (.user %in% c("achubaty") && grepl("for-cast[.]ca", .nodename)) {
  #   .mode <- append(.mode, "fit")
  # }
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
  .rep <- if (.mode == "postprocess") NA_integer_ else as.integer(.rep)
} else {
  .rep <- if (.mode == "postprocess") NA_integer_ else 1L
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
}
#####

prjDir <- "~/GitHub/Ontario_AOU_ROF"

stopifnot(identical(normalizePath(prjDir), normalizePath(getwd())))

## set new temp dir in scratch directory (existing /tmp too small for large callr ops in postprocessing)
## see https://github.com/r-lib/callr/issues/172
if (grepl("for-cast[.]ca", .nodename) && !grepl("larix", .nodename)) {
  oldTmpDir <- tempdir()
  newTmpDir <- file.path("/mnt/scratch", .user, basename(prjDir), "tmp")
  if (!dir.exists(newTmpDir)) dir.create(newTmpDir, recursive = TRUE)
  newTmpDir <- tools::file_path_as_absolute(newTmpDir)
  Sys.setenv(TMPDIR = newTmpDir)
  unlink(oldTmpDir, recursive = TRUE)
  tempdir(check = TRUE)
}

options(
  Ncpus = .ncores,
  repos = c(CRAN = "https://cran.rstudio.com"),
  Require.RPackageCache = "default", ## will use default package cache directory: `RequirePkgCacheDir()`
  Require.usepak = FALSE ## pkg deps too complicated for pak
)

# install and load packages -------------------------------------------------------------------

pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

if (!"remotes" %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
  install.packages("remotes")
}

Require.version <- "PredictiveEcology/Require@v0.2.6" ## use CRAN version
if (!"Require" %in% rownames(installed.packages(lib.loc = .libPaths()[1])) ||
    packageVersion("Require", lib.loc = .libPaths()[1]) < "0.2.6") {
  remotes::install_github(Require.version)
}

library(Require)

setLinuxBinaryRepo()

Require(c(
  "PredictiveEcology/SpaDES.project@transition (>= 0.0.7.9003)", ## TODO: use development once merged
  "PredictiveEcology/SpaDES.config@development (>= 0.0.2.9058)"
), upgrade = FALSE, standAlone = TRUE)

modulePkgs <- unname(unlist(packagesInModules(modulePath = file.path(prjDir, "modules"))))
otherPkgs <- c("archive", "details", "DBI", "s-u/fastshp", "future", "future.callr", "logging",
               "Rcpp (>= 1.0.10)",
               "PredictiveEcology/reproducible@development (>= 1.2.16.9018)",
               "RPostgres", "slackr",
               "PredictiveEcology/SpaDES.core@development (>= 1.1.1)",
               "rspatial/terra@6a3ea837be88de5a66cd0577c6d79c1091562547 (>= 1.7-0)")

Require(unique(c(modulePkgs, otherPkgs)), require = FALSE, standAlone = TRUE, upgrade = FALSE)

## NOTE: always load packages LAST, after installation above;
##       ensure plyr loaded before dplyr or there will be problems
Require(c("data.table", "plyr", "pryr", "SpaDES.core",
          "googledrive", "httr", "LandR", "magrittr", "sessioninfo", "slackr"),
        upgrade = FALSE, standAlone = TRUE)

# configure project ---------------------------------------------------------------------------

config <- SpaDES.config::useConfig(projectName = "LandRfS", projectPath = prjDir,
                                   climateGCM = .climateGCM, climateSSP = .climateSSP,
                                   mode = .mode, rep = .rep, res = .res,
                                   studyAreaName = .studyAreaName)

## apply user and machine context settings here
source("02-studyArea-config.R")
config$args <- config.studyArea$args
config$modules <- modifyList2(config$modules, config.studyArea$modules) ## TODO: update in SpaDES.config
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
stopifnot(identical(checkPath(config$paths[["projectPath"]]), getwd()))

checkPath(config$paths[["logPath"]], create = TRUE) ## others will be created as needed below

prjPaths <- SpaDES.config::paths4spades(config$paths)

# project options -----------------------------------------------------------------------------
opts <- SpaDES.config::setProjectOptions(config)

quickPlot::dev.useRSGD(useRSGD = quickPlot::isRstudioServer())

SpaDES.config::authGoogle(tryToken = "landweb", tryEmail = config$args[["cloud"]][["googleUser"]])

source("05-google-ids.R")

## helper functions
source("R/cache_helpers.R")

# begin simulations ---------------------------------------------------------------------------

do.call(SpaDES.core::setPaths, prjPaths)

if (config$args[["delayStart"]] > 0) {
  message(crayon::green("\nStaggered job start: delaying by", config$args[["delayStart"]], "minutes."))
  Sys.sleep(config$args[["delayStart"]]*60)
}

if (!"postprocess" %in% config$context[["mode"]]) {
  source("06-studyArea.R")

  if ("fit" %in% config$context[["mode"]]) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE
  } else {
    config$args[["usePrerun"]] <- TRUE
    config$args[["reupload"]] <- FALSE
  }

  source("07a-dataPrep_2001.R")
  source("07b-dataPrep_2011.R")
  source("07c-dataPrep_fS.R")  ## TODO: resume (HERE)

  source("08a-ignitionFit.R")
  source("08b-escapeFit.R")

  if (fit) {
    config$args[["usePrerun"]] <- FALSE
    config$args[["reupload"]] <- TRUE

    for (i in 1:nReps) {
      rep <- i
      config$context[["runName"]] <- gsub("rep[0-9][0-9]", sprintf("rep%02d", rep), config$context[["runName"]])

      ## prerun all spreadfits, for use with main sim runs on another machine

      if (file.exists("Rplots.pdf")) {
        unlink("Rplots.pdf")
      }
      source("08c-spreadFit.R")
      if (file.exists("Rplots.pdf")) {
        file.rename("Rplots.pdf", file.path(config$paths[["outputPath"]], "figures",
                                            sprintf("spreadFit_plots_%s.pdf", config$context[["runName"]])))
      }
    }
  } else {
    source("08c-spreadFit.R")
    source("09-main-sim.R")
  }
} else {
  # source("10-post-processing.R") ## TODO: update postprocessing script to work here
}

relOutputPath <- SpaDES.config:::.getRelativePath(prjPaths[["outputPath"]], prjDir)
rrFile <- file.path(relOutputPath, "INFO.md")
cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
cat(SpaDES.project::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

DBI::dbDisconnect(getOption("reproducible.conn"))
