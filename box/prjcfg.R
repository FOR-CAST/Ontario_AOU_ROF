box::use(R6[R6Class])
box::use(SpaDES.config[...])

box::use(DBI[dbConnect,dbDisconnect])
box::use(pemisc[availableMemory])

#' @keywords internal
.landrfsRunName <- function(context, withRep = TRUE) {
  .runName <- paste0(
    context$studyAreaName,
    paste0("_", context$climateGCM),
    paste0("_SSP", context$climateSSP),
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

#' LandR-fireSense project context class
#'
#' This extends the `projContext` class by setting various defaults for LandR-fireSense
#' and employing custom field validation.
#'
#' @export
#' @importFrom R6 R6Class
#' @rdname landrfsContext-class
landrfsContext <- R6::R6Class(
  "landrfsContext",
  inherit = projContext,

  public = list(
    #' @param projectPath Character string giving the path to the project directory.
    #'
    #' @param climateGCM Character strings giving the CMIP6 climate scenario GCM from ClimateNA.
    #'                   E.g., `"CanESM5"` or `"CNRM-ESM2-1"`.
    #'
    #' @param climateSSP Numeric CMIP climate scenario SSP. E.g., `370` or `585`.
    #'
    #' @param mode Character string. One of 'development', 'postprocess', or 'production'.
    #'             If 'development', may also include 'fit' (e.g., `c('development', 'fit)`).
    #'
    #' @param rep Integer denoting the replicate ID for the current run.
    #'
    #' @param res Numeric indicating the map resolution (pixel size) to use.
    #'            Must be one of 125 or 250 (default).
    #'
    #' @param studyAreaName Character string identifying a study area.
    #'
    initialize = function(projectPath, climateGCM = NA_character_, climateSSP = NA_character_,
                          mode = "development", rep = 1L, res = 250, studyAreaName = NA_character_) {
      stopifnot(
        res %in% c(125, 250)
      )

      private[[".pixelSize"]] <- res
      private[[".projectPath"]] <- normPath(projectPath)

      self$machine <- Sys.info()[["nodename"]]
      self$user <- Sys.info()[["user"]]

      self$mode <- mode
      self$climateGCM <- climateGCM
      self$climateSSP <- climateSSP
      self$rep <- rep
      self$studyAreaName <- studyAreaName ## will set studyAreaHash

      self$runName <- .landrfsRunName(self)

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
    #'              If 'development' may also include 'fit' (e.g., `c('development', 'fit')`).
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
        self$runName <- .landrfsRunName(self)
      }
    },

    #' @field climateSSP Numeric CMIP climate scenario SSP. E.g., `370` or `585`.
    climateSSP = function(value) {
      if (missing(value)) {
        return(private[[".climateSSP"]])
      } else {
        private[[".climateSSP"]] <- value
        self$runName <- .landrfsRunName(self)
      }
    },

    #' @field pixelSize raster pixel resolution (in metres) to use for simulations
    pixelSize = function(value) {
      if (missing(value)) {
        return(private[[".pixelSize"]])
      } else {
        stopifnot(value %in% c(125, 250))
        private[[".pixelSize"]] <- value
        self$runName <- .landrfsRunName(self)
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
          self$runName <- .landrfsRunName(self)
        }
      }
    },

    #' @field studyAreaName  Character string giving the name of current study area.
    studyAreaName = function(value) {
      if (missing(value)) {
        return(private[[".studyAreaName"]])
      } else {
        private[[".studyAreaName"]] <- value
        self$runName <- .landrfsRunName(self)
      }
    }
  ),

  private = list(
    .climateGCM = NA_character_,
    .climateSSP = NA_integer_,
    .pixelSize = 125,
    .studyAreaHash = NA_character_
  )
)

#' LandR-fireSense project configuration class
#'
#' This extends the `projConfig` class by setting various LandR-fireSense config defaults,
#' and implements custom validation and finalizer methods.
#'
#' @note See note in `?projConfig` describing the list-update mechanism of assignment to
#' certain fields.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom tools R_user_dir
#' @rdname landrfsConfig-class
landrfsConfig <- R6::R6Class(
  "landrfsConfig",
  inherit = projConfig,
  public = list(
    #' @description Create an new `landrfsConfig` object
    #'
    #' @param projectName character string of length 1 giving the name of the project.
    #'
    #' @param projectPath character string giving the path to the project directory.
    #'
    #' @param ... Additional arguments
    #'
    initialize = function(projectName, projectPath, ...) {
      dots <- list(...)

      self$context <- landrfsContext$new(projectPath = projectPath, ...)

      ## do paths first as these may be used below
      # paths ---------------------------------------------------------------------------------------
      private[[".paths"]] <- list(
        cachePath = projectPaths("cache"),
        inputPath = projectPaths("input"),
        logPath = projectPaths("log"),
        modulePath = projectPaths("module"),
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
        simYears = list(start = 2011, end = 2100),
        notifications = list(
          slackChannel = ""
        ),
        reupload = FALSE,
        useCache = FALSE,
        useLandR.CS = TRUE,
        usePrerun = TRUE
      )

      # modules ------------------------------------------------------------------------------------
      private[[".modules"]] <- list(
        ## NOTE: user needs to provide their own preamble module per project, and add it to the config
        Biomass_borealDataPrep = "Biomass_borealDataPrep",
        Biomass_core = "Biomass_core",
        Biomass_regeneration = "Biomass_regeneration",
        Biomass_speciesData = "Biomass_speciesData",
        Biomass_speciesFactorial = "Biomass_speciesFactorial",
        Biomass_speciesParameters = "Biomass_speciesParameters",
        # Biomass_summary = "Biomass_summary", ## post-processing
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
        # fireSense_summary = "fireSense_summary", ## post-processing
        gmcsDataPrep = "gmcsDataPrep"
      )

      # options ------------------------------------------------------------------------------------
      private[[".options"]] <- list(
        encoding = "UTF-8",
        future.globals.maxSize = 1000*1024^2, ## 1000 MiB (0.98 GiB)
        LandR.assertions = TRUE,
        LandR.verbose = 1,
        rasterMaxMemory = 5e+12,
        rasterTmpDir = normPath(file.path(self$paths[["scratchPath"]], "raster")),
        reproducible.cacheSaveFormat = "rds", ## can be "qs" or "rds"
        reproducible.conn = dbConnCache("sqlite"), ## "sqlite" or "postgresql"
        reproducible.destinationPath = normPath(self$paths[["inputPath"]]),
        reproducible.gdalwarp = TRUE,
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
        spades.allowInitDuringSimInit = TRUE,
        spades.allowSequentialCaching = FALSE,
        spades.futurePlan = "callr",
        spades.memoryUseInterval = 10, ## track memory use every 10 seconds
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
          fireTimestep = 1L,
          initialB = NA,
          reps = 1L:10L,
          sppEquivCol = "LandR",
          successionTimestep = 10,
          .plotInitialTime = self$args$simYears$start,
          .plots = c("object", "png", "raw", "screen"),
          .sslVerify = 0L, ## TODO: temporary to deal with NFI server SSL issues
          .studyAreaName = self$context$studyAreaName,
          .useParallel = 2 ## doesn't benefit from more DT threads
        ),
        Biomass_borealDataPrep = list(
          biomassModel = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                            (logAge + cover | ecoregionGroup))),
          ecoregionLayerField = "ECOREGION", # "ECODISTRIC"
          exportModels = "all",
          fixModelBiomass = TRUE,
          forestedLCCClasses = 1:6, ## LCC2010 default
          LCCClassesToReplaceNN = numeric(0), ## LCC2010 default
          pixelGroupAgeClass = 2 * 10,  ## twice the successionTimestep; can be coarse because initial conditions are irrelevant
          pixelGroupBiomassClass = 1000, ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
          speciesTableAreas = c("BSW", "BP", "MC", "PM"), ## western boreal defaults
          speciesUpdateFunction = list(
            quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
            quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
          ),
          subsetDataAgeModel = 100,
          subsetDataBiomassModel = 100,
          useCloudCacheForStats = FALSE, ## TODO: re-enable once errors in species levels resolved
          .plotInitialTime = self$args$simYears$start, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_core = list(
          growthAndMortalityDrivers = ifelse(isTRUE(self$args[["useLandR.CS"]]), "LandR.CS", "LandR"),
          growthInitialTime = self$args$simYears$start, ## start(sim)
          vegLeadingProportion = 0, ## apparently `sppColorVect` has no mixed colour
          .maxMemory = if (format(pemisc::availableMemory(), units = "GiB") > 130) 5 else 2, ## GB
          .plotInitialTime = self$args$simYears$start, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_regeneration = list(
          fireInitialTime = self$args$simYears$start + 1, ## start(sim, "year") + 1
          .plotInitialTime = self$args$simYears$start, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_speciesData = list(
          dataYear = self$args$simYears$start,
          types = "KNN",
          .plotInitialTime = self$args$simYears$start, ## sim(start)
          .useCache = c(".inputObjects", "init")
        ),
        Biomass_speciesFactorial = list(
          factorialSize = "small", ## TODO: use medium?
          .plots = c("png") ## saving ggplot object as qs is slow; creates massive files
        ),
        Biomass_speciesParameters = list(
          constrainGrowthCurve = c(0, 1),
          constrainMaxANPP = c(3.0, 3.5),
          constrainMortalityShape = c(10, 25),
          GAMMiterations = 2,
          GAMMknots = 3,
          minimumPlotsPerGamm = 65,
          PSPdataTypes = "all", ## will use all within studyAreaANPP
          quantileAgeSubset = 98,
          speciesFittingApproach = "focal"
        ),
        Biomass_summary = list(
          ## TODO
        ),
        canClimateData = list(
          climateGCM = self$context$climateGCM,
          climateSSP = self$context$climateSSP,
          historicalFireYears = 1971:2022, ## TODO: using more years for sampling
          studyAreaName = self$context$studyAreaName,
          .useCache = ".inputObjects"
        ),
        fireSense = list(
          plotIgnitions = FALSE,
          whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
          .plotInterval = NA,
          .runInitialTime = self$args$simYears$start ## sim(start)
        ),
        fireSense_dataPrepFit = list(
          fireYears = 2001:2022, ## TODO:
          igAggFactor = 10000 / self$context$pixelSize,
          useCentroids = TRUE,
          usePiecewiseRegression = FALSE, ## pw reg is the old approach
          whichModulesToPrepare = c("fireSense_IgnitionFit", "fireSense_EscapeFit", "fireSense_SpreadFit"),
          .studyAreaName = self$context$studyAreaName,
          .useCache = ".inputObjects"
        ),
        fireSense_dataPrepPredict = list(
          nonForestCanBeYoungAge = TRUE,
          whichModulesToPrepare = c("fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict"),
          .runInitialTime = self$args$simYears$start ## sim(start)
        ),
        fireSense_EscapeFit = list(
          ##
        ),
        fireSense_EscapePredict = list(
          .runInitialTime = self$args$simYears$start ## sim(start)
        ),
        fireSense_IgnitionFit = list(
          family = quote(MASS::negative.binomial(theta = 1, link = "identity")),
          iterDEoptim = 300,
          rescalers = NULL,
          rescaleVars = FALSE,
          studyAreaName = self$context$studyAreaName
        ),
        fireSense_IgnitionPredict = list(
          .runInitialTime = self$args$simYears$start ## sim(start)
        ),
        fireSense_SpreadFit = list(
          cloudFolderID_DE = self$args$cloud$cacheDir,
          DEoptimTests = c("adTest", "snll_fs"),
          doObjFunAssertions = FALSE,
          iterDEoptim = 150L,
          iterStep = 150L,
          iterThresh = 396L,
          libPathDEoptim = file.path(projectPath, "renv", "library",
                                     paste0("R-", getRversion()[, 1:2]), version$platform),
          mode = c("fit", "visualize"), ## combo of "debug", "fit", "visualize"
          mutuallyExclusive = list("youngAge" = c("class", "nonForest")),
          objFunCoresInternal = 1L,
          objfunFireReps = 100,
          rescaleAll = TRUE,
          trace = 1,
          SNLL_FS_thresh = NULL, # NULL means 'autocalibrate' to find suitable threshold value
          useCache_DE = FALSE,
          useCloud_DE = self$args$cloud$useCloud,
          verbose = TRUE,
          visualizeDEoptim = FALSE,
          .plot = FALSE, # TRUE,
          .plotSize = list(height = 1600, width = 2000)
        ),
        fireSense_SpreadPredict = list(
          .runInitialTime = self$args$simYears$start ## sim(start)
        ),
        fireSense_summary = list(
          ## TODO
        ),
        gmcsDataPrep = list(
          doPlotting = TRUE,
          yearOfFirstClimateImpact = self$args$simYears$start ## sim(start)
        )
      )

      self$params <- private[[".params_full"]]

      invisible(self)
    },

    #' @description Update a `landrfsConfig` object from its context.
    #'              Must be called anytime the context is updated.
    update = function() {
      self$params <- list(
        Biomass_core = list(
          growthAndMortalityDrivers = ifelse(isTRUE(self$args[["useLandR.CS"]]), "LandR.CS", "LandR")
        ),
        fireSense_SpreadFit = list(
          NP = length(self$params$fireSense_SpreadFit$cores)
        )
      )

      ## mode ---------------------------------------
      if (any(c("development", "production") %in% self$context[["mode"]])) {
        self$args <- list(
          cloud = list(
            useCloud = FALSE ## TODO: cloudCache spams Google Drive folder; doesn't respect drive path
          ),
          delayStart = if ("production" %in% self$context[["mode"]]) delay_rnd(5L:15L) else 0L, # 5-15 minutes
          successionTimestep = 10
        )

        self$params <- list(
          .globals = list(
            .plots = c("object", "png", "raw") ## don't plot to screen
          )
        )
      } else if ("postprocess" %in% self$context[["mode"]]) {
        self$modules <- list("Biomass_summary", "fireSense_summary")

        self$params <- list(
          .globals = list(
            reps = 1L:10L
          ),
          Biomass_summary = list(
            ## TODO
          ),
          fireSense_summary = list(
            ## TODO
          )
        )
      }

      ## options -- based on mode
      self$options <- list(
        LandR.assertions = if ("production" %in% self$context[["mode"]]) FALSE else TRUE,
        spades.moduleCodeChecks = if ("production" %in% self$context[["mode"]]) FALSE else TRUE
      )

      ## study area + run info ----------------------
      self$params <- list(
        .globals = list(
          .studyAreaName = self$context[["studyAreaName"]]
        ),
        Biomass_borealDataPrep = list(
          pixelGroupBiomassClass = 1000 / (250 / self$context[["pixelSize"]])^2 ## 1000 / mapResFact^2; can be coarse because initial conditions are irrelevant
        )
      )

      ## paths --------------------------------------
      self$paths <- list(
        logPath = file.path(updateOutputPath(self, .landrfsRunName), "log"),
        outputPath = updateOutputPath(self, .landrfsRunName),
        tilePath = file.path(updateOutputPath(self, .landrfsRunName), "tiles")
      )

      return(invisible(self))
    }
  ),

  private = list(
    finalize = function() {
      if (!is.null(self$options[["reproducible.conn"]])) {
        if (requireNamespace("DBI", quietly = TRUE)) {
          DBI::dbDisconnect(self$options[["reproducible.conn"]])
        }
      }
    }
  )
)
