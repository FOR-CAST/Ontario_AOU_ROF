box::use(R6[R6Class])
box::use(SpaDES.config[...])

box::use(DBI[dbConnect,dbDisconnect])
box::use(pemisc[availableMemory])
box::use(scfmutils[fireRegimePolyTypes])

# runName -------------------------------------------------------------------------------------

#' @keywords internal
.onnrvRunName <- function(context, withRep = TRUE) {
  .runName <- paste0(
    context$studyAreaName,
    paste0("_", context$fireModel, "_", paste0(context$fireCause, collapse = "")),
    paste0("_", context$nrvType),
    if (context$fireModel == "scfm") {
      ""
    } else {
      paste0("_", context$climateGCM, "_SSP", context$climateSSP)
    },
    paste0("_", context$frpType),
    if (context$pixelSize == 250) "" else paste0("_res", context$pixelSize),
    if (isTRUE(withRep)) {
      if ("postprocess" %in% context[["mode"]]) "" else sprintf("_rep%02d", context$rep)
    } else {
      ""
    }
  )
  attr(.runName, "auto") <- TRUE

  return(.runName)
}

# context -------------------------------------------------------------------------------------

#' Ontario NRV project context class
#'
#' This extends the `projContext` class by setting various defaults for Ontario NRV
#' and employing custom field validation.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname onnrvContext-class
onnrvContext <- R6::R6Class(
  "onnrvContext",
  inherit = projContext,

  public = list(
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param climateGCM Character strings giving the CMIP6 climate scenario GCM from ClimateNA.
    #'                   E.g., `"CanESM5"` or `"CNRM-ESM2-1"`.
    #'
    #' @param climateSSP Numeric CMIP climate scenario SSP. E.g., `370` or `585`.
    #'
    #' @param fireCause Character string specifying which fires to use for fitting:
    #'                  `'L'` (or `'N'`) for natural (lightning) caused fires only;
    #'                  `c('L', 'H')` for both lightning and human-caused fires.
    #'
    #' @param fireModel Character string specifying the fire model to use.
    #'                  One of 'scfm' or 'firesense'.
    #'
    #' @param frpType Character string denoting the polygons to use for scfm fire regime fitting.
    #'                One of 'BECSUBZONE', 'BECZONE', 'ECODISTRICT', or other types in
    #'                `scfmutils::fireRegimePolyTypes()`.
    #'
    #' @param mode Character string. One of 'production', 'development', or 'postprocess',
    #'             May also include 'fit' (e.g., `c('development', 'fit')`).
    #'
    #' @param nrvType Character string specifying 'hrv' for historic,
    #'                or 'frv' for future range of variability.
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param res Numeric indicating the map resolution (pixel size) to use.
    #'            Must be one of 125 or 250 (default).
    #'
    #' @param studyAreaName Character string identifying a study area (see `Ontario_preamble`
    #'                      module for up-to-date descriptions of each study area label).
    initialize = function(projectPath, mode = "development",
                          climateGCM = NA_character_, climateSSP = NA_integer_,
                          fireCause = "L", fireModel = "scfm", frpType = "FRT", nrvType = "hrv",
                          rep = 1L, res = 250, studyAreaName = NA_character_) {
      stopifnot(
        res %in% c(125, 250)
      )

      private[[".pixelSize"]] <- res
      private[[".projectPath"]] <- normPath(projectPath)

      self$machine <- machine()
      self$user <- user()

      self$mode <- mode
      self$climateGCM <- climateGCM
      self$climateSSP <- climateSSP
      self$fireCause <- fireCause
      self$fireModel <- fireModel
      self$frpType <- frpType
      self$nrvType <- nrvType
      self$rep <- rep
      self$studyAreaName <- studyAreaName ## will set studyAreaHash

      self$runName <- .onnrvRunName(self)

      return(invisible(self))
    },

    #' @description print the context object in markdown table format,
    #'              and invisibly return this formatted table for use
    #'              e.g., when writing the context info to a file for humans.
    print = function() {
      cntxt <- list(
        mode = self$mode,
        machine = self$machine,
        user = self$user,
        studyAreaName = self$studyAreaName,
        rep = self$rep,
        climateGCM = self$climateGCM,
        climateSSP = self$climateSSP,
        fireCause = self$fireCause,
        fireModel = self$fireModel,
        frpType = self$frpType,
        nrvType = self$nrvType,
        pixelSize = self$pixelSize,
        runName = self$runName
      )

      info <- context2md(cntxt)

      message(info)

      return(invisible(info))
    }
  ),

  active = list(
    #' @field mode  Character string giving the project run mode.
    #'              One of 'development', 'postprocess', or 'production'.
    #'              May also include 'fit' (e.g., `c('development', 'fit')`).
    mode = function(value) {
      if (missing(value)) {
        return(private[[".mode"]])
      } else {
        stopifnot(
          all(tolower(value) %in% c("development", "fit", "postprocess", "production"))
        )
        private[[".mode"]] <- tolower(value)

        if ("postprocess" %in% private[[".mode"]]) {
          self$rep <- NA_integer_
        }
      }
    },

    #' @field climateGCM Character strings giving the CMIP6 climate scenario GCM from ClimateNA.
    #'                   E.g., `"CanESM5"` or `"CNRM-ESM2-1"`.
    climateGCM = function(value) {
      if (missing(value)) {
        return(private[[".climateGCM"]])
      } else {
        private[[".climateGCM"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field climateSSP Numeric CMIP climate scenario SSP. E.g., `370` or `585`.
    climateSSP = function(value) {
      if (missing(value)) {
        return(private[[".climateSSP"]])
      } else {
        private[[".climateSSP"]] <- as.integer(value)
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field fireCause Character string specifying which fires to use for fitting:
    #'                  `'L'` (or `'N'`) for natural (lightning) caused fires only;
    #'                  `c('L', 'H')` for both lightning and human-caused fires.
    fireCause = function(value) {
      if (missing(value)) {
        return(private[[".fireCause"]])
      } else {
        stopifnot(value %in% c("L", "H", "N"))

        private[[".fireCause"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field fireModel Character string specifying the fire model to use.
    #'                  One of 'scfm' or 'firesense'.
    fireModel = function(value) {
      if (missing(value)) {
        return(private[[".fireModel"]])
      } else {
        value <- tolower(value)
        stopifnot(value %in% c("scfm", "firesense"))

        private[[".fireModel"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field frpType Character string denoting the polygons to use for scfm fire regime fitting.
    #'                One of the types in `scfmutils::fireRegimePolyTypes()`.
    frpType = function(value) {
      if (missing(value)) {
        return(private[[".frpType"]])
      } else {
        stopifnot(value %in% scfmutils::fireRegimePolyTypes())

        private[[".frpType"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field nrvType Character string specifying 'hrv' for historic
    #'                or 'frv' for future range of variability.
    nrvType = function(value) {
      if (missing(value)) {
        return(private[[".nrvType"]])
      } else {
        value <- tolower(value)
        stopifnot(value %in% c("hrv", "frv"))

        if (value == "frv") {
          if (is.na(self$climateGCM) || is.na(self$climateSSP)) {
            stop("climateGCM and climateSSP must be specified for FRV runs")
          }
        }

        private[[".nrvType"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field pixelSize raster pixel resolution (in metres) to use for simulations
    pixelSize = function(value) {
      if (missing(value)) {
        return(private[[".pixelSize"]])
      } else {
        stopifnot(value %in% c(125, 250))
        private[[".pixelSize"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    },

    #' @field rep  replicate id (integer)
    rep = function(value) {
      if (missing(value)) {
        return(private[[".rep"]])
      } else {
        if ("postprocess" %in% private[[".mode"]] && !is.na(value)) {
          warning("unable to set context$rep because context$mode is 'postprocess'")
        } else {
          private[[".rep"]] <- as.integer(value)
          self$runName <- .onnrvRunName(self)
        }
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        private[[".studyAreaName"]] <- value
        self$runName <- .onnrvRunName(self)
      }
    }
  ),

  private = list(
    .climateGCM = NA_character_,
    .climateSSP = NA_integer_,
    .fireCause = "L",
    .fireModel = "scfm",
    .frpType = "FRT",
    .nrvType = "hrv",
    .pixelSize = 125,
    .studyAreaHash = NA_character_
  )
)

# config --------------------------------------------------------------------------------------

#' Ontario NRV project configuration class
#'
#' This extends the `projConfig` class by setting various Ontario NRV config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @note See note in `?projConfig` describing the list-update mechanism of assignment to
#' certain fields.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname onnrvConfig-class
onnrvConfig <- R6::R6Class(
  "onnrvConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `onnrvConfig` object
    #'
    #' @param projectName character string of length 1 giving the name of the project.
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments
    #'
    initialize = function(projectName, projectPath, ...) {
      dots <- list(...)

      self$context <- onnrvContext$new(projectPath = projectPath, ...)

      ## do paths first as these may be used below
      # paths ---------------------------------------------------------------------------------------
      modPaths <- if (self$context[["fireModel"]] == "scfm") {
        c(projectPaths("module"),
          file.path(projectPaths("modules"), "scfm", "modules"))
      } else {
        projectPaths("module")
      }

      private[[".paths"]] <- list(
        cachePath = projectPaths("cache"),
        inputPath = projectPaths("input"),
        logPath = projectPaths("log"),
        modulePath = modPaths,
        outputPath = projectPaths("output"),
        projectPath = normPath(projectPath),
        scratchPath = file.path(dirname(tempdir()), "scratch", basename(projectPath)),
        tilePath = file.path(projectPaths("output"), "tiles")
      )

      # arguments -----------------------------------------------------------------------------------
      private[[".args"]] <- list(
        cloud = list(
          cacheDir = "",
          googleUser = "",
          useCloud = FALSE ## TODO: cloudCache spams Google Drive folder; doesn't respect drive path
        ),
        delayStart = 0,
        fsimext = "rds", ## TODO: use qs once spades.core is fixed
        simYears = if (self$context[["fireModel"]] == "scfm") {
          list(start = 0, end = 1201) ## use one extra year to get fire summaries
        } else {
          list(start = 2011, end = 2100)
        },
        notifications = list(
          slackChannel = ""
        ),
        reupload = FALSE,
        useCache = FALSE, ## simulation caching
        useLandR.CS = if (self$context[["fireModel"]] == "scfm") FALSE  else TRUE,
        usePrerun = TRUE
      )
      self$args <- list(
        ## these need 'simYears' to already be defined
        summaryInterval = 50,
        summaryPeriod = c(self$args$simYears$start + 800, self$args$simYears$end),
        timeSeriesTimes = self$args[["simYears"]][["start"]] + 801:850,
        transitionPlotTimes = seq(self$args[["simYears"]][["start"]], self$args[["simYears"]][["end"]], 100)
      )
      self$args <- list(
        ## these need 'summaryPeriod' and 'summaryInterval' to already be defined
        analysesOutputsTimes = seq(self$args[["summaryPeriod"]][1], self$args[["summaryPeriod"]][2],
                                   self$args[["summaryInterval"]])
      )

      # modules ------------------------------------------------------------------------------------
      private[[".fireModules"]] <- list() ## updated based on context$fireModel below

      private[[".modules"]] <- list(
        ## NOTE: user needs to provide their own preamble module per project, and add it to the config
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        Biomass_speciesFactorial = "Biomass_speciesFactorial",
        Biomass_speciesParameters = "Biomass_speciesParameters",
        ## Biomass_summary = "Biomass_summary", ## post-processing
        ## birds_BRT = "birds_BRT", ## post-processing
        ## burnSummaries = "burnSummaries", ## post-processing
        ## NRV_summary = "NRV_summary ## post-processing
        timeSinceFire = "timeSinceFire"
      )

      # options ------------------------------------------------------------------------------------
      private[[".options"]] <- list(
        encoding = "UTF-8",
        future.availableCores.fallback = parallelly::availableCores(constraints = "connections", omit = 2L),
        future.globals.maxSize = 1000*1024^2, ## 1000 MiB (0.98 GiB)
        future.plan = "callr",
        LandR.assertions = TRUE,
        LandR.verbose = 1,
        map.dataPath = self$paths$inputPath, # not used yet
        map.maxNumCores = pemisc::optimalClusterNum(20000, parallel::detectCores() / 2),
        map.overwrite = TRUE,
        map.tilePath = FALSE, ## TODO: use self$paths$tilePath once parallel tile creation works
        map.useParallel = TRUE, ## TODO: streamline useParallel: used directly for post-processing
        pemisc.useParallel = TRUE, ## TODO: streamline useParallel: used directly by scfm
        rasterMaxMemory = 5e+9,
        rasterTmpDir = normPath(file.path(self$paths[["scratchPath"]], "raster")),
        reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
        reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
        reproducible.destinationPath = normPath(self$paths[["inputPath"]]),
        reproducible.gdalwarp = TRUE, ## required b/c prepInputs doing it wrong???
        reproducible.inputPaths = NULL,
        reproducible.nThreads = 2,
        reproducible.overwrite = TRUE,
        reproducible.quick = FALSE,
        # reproducible.shapefileRead = "terra::vect",
        reproducible.showSimilar = FALSE,
        reproducible.useCache = TRUE,
        reproducible.useCloud = FALSE, ## TODO: cloudCache spams Google Drive; doesn't respect drive path
        reproducible.useTerra = TRUE,
        Require.install = FALSE, ## don't use Require; assume all pkgs installed
        scfmutils.driver.plot.scam = FALSE,
        spades.allowInitDuringSimInit = FALSE, ## TODO: is TRUE working correctly???
        spades.allowSequentialCaching = FALSE,
        spades.futurePlan = "callr",
        # spades.memoryUseInterval = 10, ## track memory use every 10 seconds
        spades.memoryUseInterval = FALSE, ## TODO: broken with recent SpaDES.core versions; hangs indefinitely
        spades.messagingNumCharsModule = 36,
        spades.moduleCodeChecks = TRUE,
        spades.qsThreads = 4,
        spades.recoveryMode = FALSE,
        spades.scratchPath = normPath(self$paths[["scratchPath"]]),
        spades.useRequire = FALSE ## don't use Require; using renv so assume all pkgs installed
      )

      # parameters ---------------------------------------------------------------------------------
      private[[".params_full"]] <- list(
        .globals = list(
          fireTimestep = 1L, ## TODO: where is this used?
          initialB = NA, ## 10
          reps = 1L:50L,
          sppEquivCol = "LandR",
          successionTimestep = 10,
          summaryInterval = 50,
          summaryPeriod = c(self$args$simYears$start + 800, self$args$simYears$end), ## TODO: confirm
          vegLeadingProportion = 0.8,
          .plotInitialTime = self$args$simYears$start,
          .plots = "png", ## TODO: c("object", "png", "raw", "screen")
          .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
          .studyAreaName = self$context$studyAreaName,
          .useCache = FALSE, ## TODO: event caching is broken
          .useParallel = 2 ## doesn't benefit from more DT threads
        ),
        Biomass_borealDataPrep = list(
          dataYear = 2011,
          ecoregionLayerField = "ECOREGION", ## "ECODISTRIC"
          exportModels = "none", ## use "all" to export for debugging
          fixModelBiomass = TRUE,
          forestedLCCClasses = 1:6, ## LCC2010 default
          LCCClassesToReplaceNN = numeric(0), ## LCC2010 default
          speciesTableAreas = c("BSW", "BP", "MC"),
          speciesUpdateFunction = list(
            quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
            quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
          ),
          subsetDataAgeModel = 100,
          subsetDataBiomassModel = 100,
          useCloudCacheForStats = FALSE, ## TODO: re-enable once errors in species levels resolved
          .plotInitialTime = self$args$simYears$start, ## start(sim)
          .useCache = FALSE # c(".inputObjects", "init") ## TODO
        ),
        Biomass_core = list(
          growthAndMortalityDrivers = ifelse(isTRUE(self$args[["useLandR.CS"]]), "LandR.CS", "LandR"),
          growthInitialTime = self$args$simYears$start, ## start(sim)
          initialBiomassSource = "cohortData",
          mixedType = 2L,
          vegLeadingProportion = 0, ## apparently `sppColorVect` has no mixed colour
          .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
          .plotInitialTime = self$args$simYears$start, ## start(sim)
          .useCache = FALSE # c(".inputObjects", "init") ## TODO
        ),
        Biomass_regeneration = list(
          calibrate = TRUE, ## TODO: use TRUE for debugging regen
          fireInitialTime = self$args$simYears$start + 1, ## start(sim) + 1
          .plotInitialTime = self$args$simYears$start, ## start(sim)
          .useCache = FALSE # c(".inputObjects", "init") ## TODO
        ),
        Biomass_speciesData = list(
          dataYear = 2011,
          types = "KNN",
          .plotInitialTime = self$args$simYears$start, ## start(sim)
          .useCache = FALSE # c(".inputObjects", "init") ## TODO
        ),
        Biomass_speciesFactorial = list(
          factorialSize = "small" ## TODO: use medium?
        ),
        Biomass_speciesParameters = list(
          PSPdataTypes = "all", ## will use all within studyAreaANPP
          quantileAgeSubset = 98,
          speciesFittingApproach = "focal" ## 'pairwise' ?
        ),
        burnSummaries = list(
          simOutPrefix = "simOutMainSim",
          simTimes = unlist(self$args[["simYears"]])
        ),
        canClimateData = list(
          climateGCM = self$context$climateGCM,
          climateSSP = self$context$climateSSP,
          historicalFireYears = 1971:2022, ## TODO: using more years for sampling
          outputDir = file.path(dirname(self$paths[["outputPath"]]), "climate"), ## outputs/studyArea/climate
          projectedType = "forecast",
          .studyAreaName = self$context$studyAreaName,
          .useCache = FALSE ## c(".inputObjects", "init")
        ),
        fireSense = list(
          plotIgnitions = FALSE,
          whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
          .plotInterval = NA,
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_dataPrepFit = list(
          fireYears = 2002:2022,
          forestedLCC = 1:6, ## LCC2010 default
          igAggFactor = 10000 / self$context$pixelSize,
          ignitionFuelClassCol = "FuelClass", ## TODO: use improved classification
          spreadFuelClassCol = "FuelClass", ## TODO: use improved classification
          useCentroids = TRUE,
          usePiecewiseRegression = FALSE, ## pw reg is the old approach
          useRasterizedFireForSpread = FALSE,
          whichModulesToPrepare = c("fireSense_IgnitionFit", "fireSense_EscapeFit", "fireSense_SpreadFit"),
          .studyAreaName = self$context$studyAreaName,
          .useCache = FALSE # ".inputObjects" ## TODO
        ),
        fireSense_dataPrepPredict = list(
          forestedLCC = 1:6, ## LCC2010 default
          ignitionFuelClassCol = "FuelClass", ## TODO: use improved classification
          nonForestCanBeYoungAge = TRUE,
          spreadFuelClassCol = "FuelClass", ## TODO: use improved classification
          whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_EscapeFit = list(
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_EscapePredict = list(
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_IgnitionFit = list(
          # cores = 1L, ## added by user config
          # iterDEoptim = 300, ## default: 500
          # iterNlminb = 500, ## default: 500
          rescalers = NULL,
          rescaleVars = TRUE,
          .runInitialTime = self$args$simYears$start, ## start(sim)
          .studyAreaName = self$context$studyAreaName,
          .useCache = "run" ## TODO
        ),
        fireSense_IgnitionPredict = list(
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_SpreadFit = list(
          cloudFolderID_DE = self$args$cloud$cacheDir,
          # cores = 1L, ## added by user config
          DEoptimTests = c("adTest", "snll_fs"),
          doObjFunAssertions = FALSE,
          iterDEoptim = 150L, ## default 500L
          iterStep = 150L, ## default 25L
          iterThresh = 396L, ## default 96L
          libPathDEoptim = file.path(projectPath, "renv", "library",
                                     paste0("R-", getRversion()[, 1:2]), version$platform),
          mode = c("fit", "visualize"), ## combo of "debug", "fit", "visualize"
          mutuallyExclusiveCols = list("youngAge" = c("class", "nonForest")),
          objFunCoresInternal = 1L,
          objfunFireReps = 100,
          rep = self$config$context$rep,
          rescaleAll = TRUE,
          trace = 1,
          SNLL_FS_thresh = NULL, # NULL means 'autocalibrate' to find suitable threshold value
          useCache_DE = FALSE,
          useCloud_DE = self$args$cloud$useCloud,
          verbose = TRUE,
          visualizeDEoptim = FALSE,
          .plot = FALSE, # TRUE,
          .plotSize = list(height = 1600, width = 2000),
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        fireSense_SpreadPredict = list(
          mutuallyExclusiveCols = list("youngAge" = c("class", "nonForest")),
          .runInitialTime = self$args$simYears$start ## start(sim)
        ),
        gmcsDataPrep = list(
          doPlotting = TRUE,
          yearOfFirstClimateImpact = self$args$simYears$start ## start(sim)
        ),
        NRV_summary = list(
          ageClasses = c("Young1", "Young2", "Immature1", "Immature2", "Mature1", "Mature2", "Old", "Old2"),
          ageClassCutOffs = seq(0, 140, 20),
          ageClassMaxAge = 400L, ## was `maxAge` previously
          reps = 1L:10L, ## TODO: used elsewhere to setup runs (expt table)?
          postprocessEvents = "bc",
          sieveThresh = as.integer(10 / self$args[["pixelSize"]]), ## 10 ha in pixels
          # simOutputPath = self$paths[["outputPath"]],
          studyAreaNamesCol = "LU_NAME",
          summaryInterval = 50,        ## also in .globals
          summaryPeriod = c(800, 1200), ## also in .globals
          timeSeriesTimes = 801:850, ## TODO: from args
          upload = FALSE,
          uploadTo = "", ## TODO: use google-ids.csv to define these per WBI?
          .plotInitialTime = self$args$simYears$start, ## start(sim)
          .useCache = FALSE # c(".inputObjects") ## don't cache 'init' ## TODO
        ),
        scfmDiagnostics = list(
          mode = "single"
        ),
        scfmDriver = list(
          pMax = 0.27,
          targetN = 5000, ## increase targetN for more robust estimates, longer run-time
          scamOptimizer = "efs",
          .useCache = FALSE, # ".inputObjects", ## don't cache 'init' ## TODO
          .useCloud = FALSE,
          .useParallelFireRegimePolys = FALSE ## TODO: fix cluster hang/crash
        ),
        scfmEscape = list(
          startTime = self$args$simYears$start + 1,
          .useCache = FALSE # ".inputObjects", ## don't cache 'init' ## TODO
        ),
        scfmIgnition = list(
          startTime = self$args$simYears$start + 1,
          .useCache = FALSE # ".inputObjects", ## don't cache 'init' ## TODO
        ),
        scfmLandcoverInit = list(
          sliverThreshold = 1e8, ## polygons <100 km2 are merged with closest non-sliver neighbour
          .plotInitialTime = self$args$simYears$start + 1,
          .useCache = FALSE # ".inputObjects", ## don't cache 'init' ## TODO
        ),
        scfmRegime = list(
          fireCause = self$context[["fireCause"]],
          fireEpoch = c(1971, 2010), ## default 1971-2000; using longer epoch for areas too small w/ not enough fire data
          .useCache = FALSE # ".inputObjects", ## don't cache 'init' ## TODO
        ),
        scfmSpread = list(
          startTime = self$args$simYears$start + 1,
          .plotInitialTime = self$args$simYears$start + 1,
          .useCache = FALSE # ".inputObjects", ## don't cache 'init' ## TODO
        ),
        timeSinceFire = list(
          startTime = self$args$simYears$start + 1,
          .useCache = ".inputObjects" ## faster without caching for "init"
        )
      )

      self$params <- private[[".params_full"]]

      invisible(self)
    },

    #' @description Update a `onnrvConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      ## fireModel -------------------------------------------------------------
      private[[".fireModules"]] <- switch(
        self$context[["fireModel"]],
        firesense = list(
          canClimateData = "canClimateData",
          fireSense = "fireSense",
          fireSense_dataPrepFit = "fireSense_dataPrepFit",
          fireSense_dataPrepPredict = "fireSense_dataPrepPredict",
          fireSense_EscapeFit = "fireSense_EscapeFit",
          fireSense_EscapePredict = "fireSense_EscapePredict",
          fireSense_IgnitionFit = "fireSense_IgnitionFit",
          fireSense_IgnitionPredict = "fireSense_IgnitionPredict",
          fireSense_SpreadFit = "fireSense_SpreadFit",
          fireSense_SpreadPredict = "fireSense_SpreadPredict",
          ## fireSense_summary = "fireSense_summary", ## post-processing
          gmcsDataPrep = "gmcsDataPrep"
        ),
        scfm = list(
          scfmDiagnostics = "scfmDiagnostics",
          scfmDriver = "scfmDriver",
          scfmEscape = "scfmEscape",
          scfmIgnition = "scfmIgnition",
          scfmLandcoverInit = "scfmLandcoverInit",
          scfmRegime = "scfmRegime",
          scfmSpread = "scfmSpread"
        )
      )

      self$modules <- modList(self$modules, private[[".fireModules"]])

      if (self$context[["fireModel"]] == "firesense") {
        self$params <- list(
          fireSense_SpreadFit = list(
            NP = length(self$params[["fireSense_SpreadFit"]][["cores"]])
          )
        )
      }

      ## frpType ---------------------------------------------------------------
      self$params <- list(
        Ontario_preamble = list(
          fireRegimePolysType = self$context[["frpType"]]
        )
      )

      ## mode ------------------------------------------------------------------
      if (any(c("development", "production") %in% self$context[["mode"]])) {
        self$args <- list(
          cloud = list(
            useCloud = FALSE ## TODO: cloudCache spams Google Drive folder; doesn't respect drive path
          ),
          delayStart = if ("production" %in% self$context[["mode"]]) delay_rnd(5L:15L) else 0L, # 5-15 minutes
          successionTimestep = 10,
          summaryInterval = 50, ## TODO: remove from args; used in params
          summaryPeriod = c(self$args$simYears$start + 800, self$args$simYears$end) ## TODO: confirm; remove from args; used in params
        )

        self$modules <- modList(self$modules, private[[".fireModules"]])

        self$params <- list(
          .globals = list(
            .plots = c("png") ## don't plot to screen; saving ggplot/raw is very slow
          )
        )
      } else if ("postprocess" %in% self$context[["mode"]]) {
        self$modules <- list(
          ## TODO preamble + speciesData ??
          "Biomass_summary",
          "fireSense_summary",
          "birds_BRT",
          "burnSummaries",
          "NRV_summary"
        )

        if (self$context[["fireModel"]] == "scfm") {
          self$modules <- modList(self$modules, list(scfmDiagnostics = "scfmDiagnostics"))
          self$params <- list(
            scfmDiagnostics = list(
              mode = "multi",
              simOutPrefix = "simOutMainSim",
              simTimes = unlist(self$args[["simYears"]])
            )
          )
        } else if (self$context[["fireModel"]] == "firesense") {
          self$modules <- modList(self$modules, list(fireSense_summary = "fireSense_summary"))
        }

        self$params <- list(
          .globals = list(
            reps = 1L:50L,
            .plots = c("png")
          ),
          Biomass_summary = list(
            ## TODO
          ),
          birds_BRT = list(
            ## TODO
          ),
          fireSense_summary = list(
            ## TODO
          ),
          NRV_summary = list(
            postprocessEvents = "on"
          )
        )
      }

      ## options -- based on mode
      self$options <- list(
        LandR.assertions = if ("production" %in% self$context[["mode"]]) FALSE else TRUE,
        spades.moduleCodeChecks = if ("production" %in% self$context[["mode"]]) FALSE else TRUE
      )

      ## args ------------------------------------------------------------------
      self$params <- list(
        NRV_summary = list(
          summaryPeriod = c(self$args$simYears$start + 800, self$args$simYears$end),
          timeSeriesTimes = self$args$simYears$start + 801:850
        )
      )

      ## NRV type --------------------------------------------------------------
      if (self$context[["nrvType"]] == "hrv") {
        self$params <- list(
          canClimateData = list(
            projectedFireYears = 1501:2000, ## TODO: not real calendar years!
            projectedType = "hindcast"
          ),
          NRV_summary = list(
            ## TODO for HRV
          )
        )
      } else {
        self$params <- list(
          NRV_summary = list(
            ## TODO for FRV
          )
        )
      }

      ## TODO: if using frv, need non-NA climateGCM and SSP

      ## study area + run info -------------------------------------------------
      self$params <- list(
        .globals = list(
          .studyAreaName = self$context[["studyAreaName"]]
        ),
        Biomass_borealDataPrep = list(
          pixelGroupBiomassClass = 1000 / (250 / self$context[["pixelSize"]])^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
        ),
        Biomass_core = list(
          growthAndMortalityDrivers = ifelse(isTRUE(self$args[["useLandR.CS"]]), "LandR.CS", "LandR")
        )
      )

      ## paths -----------------------------------------------------------------
      self$paths <- list(
        logPath = file.path(updateOutputPath(self, .onnrvRunName), "log"),
        outputPath = updateOutputPath(self, .onnrvRunName),
        tilePath = file.path(updateOutputPath(self, .onnrvRunName), "tiles")
      )

      return(invisible(self))
    }
  ),

  private = list(
    .fireModules = list(),

    finalize = function() {
      if (!is.null(self$options[["reproducible.conn"]])) {
        if (requireNamespace("DBI", quietly = TRUE)) {
          DBI::dbDisconnect(self$options[["reproducible.conn"]])
        }
      }
    }
  )
)
