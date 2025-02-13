# paths ---------------------------------------------------------------------------------------

if (!"postprocess" %in% config$context[["mode"]]) {
  ## don't need replicated copies of dataPrep outputs
  repID <- basename(config$paths[["outputPath"]])
  config$paths[["outputPath"]] <- dirname(config$paths[["outputPath"]]) ## TODO: add to config
  checkPath(config$paths[["logPath"]], create = TRUE)
}

# modules -------------------------------------------------------------------------------------

modules2 <- c(
  "Biomass_speciesData",
  "Biomass_speciesFactorial",
  "Biomass_borealDataPrep",
  "Biomass_speciesParameters"
) ## TODO: use config$modules

# parameters ----------------------------------------------------------------------------------

parameters2 <- list(
  .globals = config$params[[".globals"]],
  Biomass_borealDataPrep = config$params[["Biomass_borealDataPrep"]],
  Biomass_speciesData = config$params[["Biomass_speciesData"]],
  Biomass_speciesFactorial = config$params[["Biomass_speciesFactorial"]],
  Biomass_speciesParameters = config$params[["Biomass_speciesParameters"]]
)

# objects -------------------------------------------------------------------------------------

## ensure all 'objects(simOutPreamble)' accounted for here, with correct names/mappings
if (FALSE) {
  ## verify everything needed for main sim gets into objects_sim / objects_fireModel
  obj4sim <- lapply(objects(simOutPreamble), function(x) simOutPreamble[[x]])
  names(obj4sim) <- objects(simOutPreamble)

  objs2drop <- c()

  for (i in objs2drop) {
    obj4sim[[i]] <- NULL
  }

  names(obj4sim) |> sort()

  rm(objs2drop, obj4sim)
}

objects2 <- list(
  cloudFolderID = config$args[["cloud"]][["cacheDir"]],
  imputedPixID = simOutPreamble[["imputedPixID2011"]],
  rasterToMatch = simOutPreamble[["rasterToMatch"]],
  rasterToMatchLarge = simOutPreamble[["rasterToMatchLarge"]],
  rstLCC = simOutPreamble[["rstLCC2011"]],
  speciesParams = simOutPreamble[["speciesParams"]],
  speciesTable = simOutPreamble[["speciesTable"]],
  sppColorVect = simOutPreamble[["sppColorVect"]],
  sppEquiv = simOutPreamble[["sppEquiv"]],
  studyArea = simOutPreamble[["studyArea"]],
  studyAreaLarge = simOutPreamble[["studyAreaLarge"]],
  studyAreaReporting = simOutPreamble[["studyAreaReporting"]]
)

objects2_fireModel <- list(
  standAgeMap = simOutPreamble[["standAgeMap2011"]]
)

objects2 <- append(objects2, objects2_fireModel)

# outputs -------------------------------------------------------------------------------------

outputs2 <- data.frame(
  objectName = c(
    "ecoregionMap",
    "speciesEcoregion",
    "species" ## adjusted species traits table
  ),
  saveTime = c(1, 1, 1),
  fun = c("writeRaster", "write.csv", "write.csv"),
  package = c("terra", "base", "base"),
  file = c(
    "ecoregionMap_year0000.tif",
    "speciesEcoregion_year0000.csv",
    "speciesTraits_adjusted.csv"
  ),
  stringsAsFactors = FALSE
)
outputs2$arguments <- I(list(
  list(overwrite = TRUE, progress = FALSE),
  list(row.names = FALSE),
  list(row.names = FALSE)
))

# run simulation ------------------------------------------------------------------------------

dataPrepFile <- simFile(
  name = paste0("simOutDataPrep_", config$context[["studyAreaName"]]),
  path = config$paths[["outputPath"]],
  ext = config$args[["fsimext"]]
)

if (file.exists(dataPrepFile) && isTRUE(config$args[["usePrerun"]])) {
  ## TODO: download prerun object
  simOutDataPrep <- loadSimList(dataPrepFile)
} else {
  tryCatch({
    simOutDataPrep <- Cache(
      simInitAndSpades,
      times = list(start = 0, end = 1),
      params = parameters2,
      modules = modules2,
      objects = objects2,
      outputs = outputs2,
      paths = SpaDES.config::paths4spades(config$paths),
      debug = list(
        file = list(file = file.path(config$paths[["logPath"]], "07-dataPrep-scfm.log"), append = FALSE),
        debug = 1
      ),
      omitArgs = c("debug", "paths", ".plotInitialTime"),
      useCache = config$args[["useCache"]],
      useCloud = config$args[["cloud"]][["useCloud"]],
      cloudFolderID = config$args[["cloud"]][["cacheDir"]],
      userTags = c(config$context[["studyAreaName"]], "dataPrep")
    )
  }, error = function(e) {
    if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0("ERROR in 05-dataPrep: `", config$context[["runName"]],
               "` on host `", config$context[["machine"]], "`.\n",
               "```\n", e$message, "\n```")
      )

      stop(e$message)
    }

    if (interactive()) {
      config$paths[["outputPath"]] <- file.path(config$paths[["outputPath"]], repID)
    } else {
      DBI::dbDisconnect(getOption("reproducible.conn"))
    }
  })

  if (isUpdated(simOutDataPrep) || isFALSE(config$args[["useCache"]])) {
    simOutDataPrep@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      simOutDataPrep,
      dataPrepFile,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
    ## TODO: upload
  }
}

## restore paths + cleanup
if (!"postprocess" %in% config$context[["mode"]]) {
  config$paths[["outputPath"]] <- file.path(config$paths[["outputPath"]], repID)
}
terra::tmpFiles(remove = TRUE)
