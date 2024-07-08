paths_sim <- SpaDES.config::paths4spades(config$paths)

times_sim <- config$args[["simYears"]]

prev_modules <- c(
  unlist(preambleModules),    ## 06-studyArea.R
  "Biomass_speciesData",      ## 07-dataPrep-scfm.R
  "Biomass_speciesFactorial", ## 07-dataPrep-scfm.R
  "Biomass_borealDataPrep",   ## 07-dataPrep-scfm.R
  "Biomass_speciesParameters" ## 07-dataPrep-scfm.R
)

modules_sim <- config$modules[-which(names(config$modules) %in% prev_modules)]

loadorder_sim <- c(
  ## TODO: specify load order manually; is this still necessary???
)

parameters_sim <- config$params

# simulation objects --------------------------------------------------------------------------

objects_sim <- list(
  biomassMap = simOutDataPrep[["biomassMap"]],
  cohortData = simOutDataPrep[["cohortData"]],
  ecoregion = simOutDataPrep[["ecoregion"]],
  ecoregionMap = simOutDataPrep[["ecoregionMap"]],
  # fireReturnInterval = simOutPreamble[["fireReturnInterval"]], ## LandWeb_output
  flammableMap = simOutPreamble[["flammableMap"]],
  flammableMapLarge = simOutPreamble[["flammableMapLarge"]],
  minRelativeB = simOutDataPrep[["minRelativeB"]],
  pixelGroupMap = simOutDataPrep[["pixelGroupMap"]],
  rawBiomassMap = simOutDataPrep[["rawBiomassMap"]],
  rasterToMatch = simOutDataPrep[["rasterToMatch"]],
  rasterToMatchLarge = simOutDataPrep[["rasterToMatchLarge"]],
  rstFlammable = simOutPreamble[["rstFlammable"]],
  rstTimeSinceFire = simOutPreamble[["rstTimeSinceFire"]],
  species = simOutDataPrep[["species"]],
  speciesEcoregion = simOutDataPrep[["speciesEcoregion"]],
  speciesLayers = simOutDataPrep[["speciesLayers"]],
  speciesTable = simOutDataPrep[["speciesTable"]],
  sppColorVect = simOutDataPrep[["sppColorVect"]],
  sppEquiv = simOutDataPrep[["sppEquiv"]],
  studyArea = simOutDataPrep[["studyArea"]],
  studyAreaLarge = simOutDataPrep[["studyAreaLarge"]],
  studyAreaReporting = simOutDataPrep[["studyAreaReporting"]],
  sufficientLight = simOutDataPrep[["sufficientLight"]]#,
  # summaryPeriod = config$params[[".globals"]][["summaryPeriod"]] ## LandWeb_output
)

objects_fireModel <- list(
  fireRegimePolys = simOutPreamble[["fireRegimePolys"]], ## scfmDriver, scfmRegime
  fireRegimePolysLarge = simOutPreamble[["fireRegimePolysLarge"]], ## scfmLandCoverInit, scfmRegime
  rstLCC = simOutDataPrep[["rstLCC"]],
  standAgeMap = simOutDataPrep[["standAgeMap"]],
  vegMap = simOutDataPrep[["rstLCC"]] ## scfmLandCoverInit
)

objects_sim <- append(objects_sim, objects_fireModel)

# simulation outputs --------------------------------------------------------------------------

## objects to save during simulation
times2save <- c(
  unlist(config$args[["simYears"]]),
  config$args[["analysesOutputsTimes"]],
  config$args[["timeSeriesTimes"]],
  config$args[["transitionPlotTimes"]],
) |> unique() |> sort()

objs2save_during <- c(
  "cohortData",        ## data.table (.qs); NRV_summary, visualize_LandR_output

  "pixelGroupMap",     ## SpatRaster (.tif); NRV_summary, visualize_LandR_output
  "reproductionMap",   ## SpatRaster (.tif); debugging regen
  "rstCurrentBurn",    ## SpatRaster (.tif); burnSummaries
  "rstTimeSinceFire",  ## SpatRaster (.tif); HSI_PineMarten, visualize_LandR_output
  "standAgeMap",       ## SpatRaster (.tif); LandWeb_output, NRV_summary
  "vegTypeMap"         ## SpatRaster (.tif); LandWeb_output, HSI_PineMarten, NRV_summary, visualize_LandR_output
)

outputs_during <- data.frame(
  expand.grid(objectName = objs2save_during, saveTime = times2save),
  fun = c("qsave", rep("writeRaster", nRsts)),
  package = c("qs", rep("terra", nRsts)),
  file = paste0(objs2save_during, c(".qs", rep(".tif", nRsts))),
  stringsAsFactors = FALSE
)
outputs_during$arguments <- I(list(
  list(nthreads = 1),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT4U"), ## !! need >6e6 pixelGroupIDs
  list(overwrite = TRUE, progress = FALSE),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U"),
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U")
))

## objects to save (only) at end of simulation
objs2save_end <- c(
  "burnMap",              ## SpatRaster (.tif)
  "burnSummary",          ## data.table (.qs)
  "postFireRegenSummary", ## data.table (.qs)
  "simulationOutput",     ## data.table (.qs)
  "species"               ## data.table (.qs)
)

outputs_end <- data.frame(
  objectName = objs2save_end,
  saveTime = times_sim$end,
  fun = c("writeRaster", "qsave", "qsave", "qsave", "qsave"),
  package = c("terra", "qs", "qs", "qs", "qs"),
  file = paste0(objs2save_end, c(".tif", ".qs", ".qs", ".qs", ".qs")),
  stringsAsFactors = FALSE
)
outputs_end$arguments <- I(list(
  list(overwrite = TRUE, progress = FALSE, datatype = "INT2U"),
  list(nthreads = 1),
  list(nthreads = 1),
  list(nthreads = 1),
  list(nthreads = 1)
))

## all outputs
outputs_sim <- rbind(outputs_during, outputs_end)

# run simulation ------------------------------------------------------------------------------

data.table::setDTthreads(config$params[[".globals"]][[".useParallel"]])

## record RNG info (e.g., for debugging)
fseed <- file.path(Paths$outputPath, "seed.rds")
fseed2 <- raster::extension(fseed, "txt")
if (file.exists(fseed)) {
  seed <- readRDS(fseed)
} else {
  seed <- sample(1e4, 1)
  saveRDS(seed, fseed)
}
print(paste("random seed:", seed))
cat(paste("Setting seed in 10-main-sim.R:", seed), file = fseed2, sep = "\n")
set.seed(seed)
writeRNGInfo(fseed2, append = TRUE)

## main simulation
fsim <- simFile(
  name = config$context[["runName"]],
  path = paths_sim[["outputPath"]],
  time = config$args[["simYears"]][["end"]],
  ext = config$args[["fsimext"]]
)

tryCatch({
  mySimOut <- simInitAndSpades(
    times = times_sim,
    params = parameters_sim,
    modules = modules_sim,
    # loadOrder = loadorder_sim, ## TODO: confirm this is no longer necessary
    outputs = outputs_sim,
    objects = objects_sim,
    paths = paths_sim,
    debug = list(
      file = list(file = file.path(config$paths[["logPath"]], "09-main-scfm.log"), append = FALSE),
      debug = 1
    )
  )

  capture.output(warnings(), file = file.path(config$paths[["logPath"]], "warnings.txt"), split = TRUE)

  # end-of-sim notifications --------------------------------------------------------------------
  if (requireNamespace("notifications") && file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0("Simulation `", config$context[["runName"]],
             "` completed on host `", config$context[["machine"]], "`",
             if (nzchar(Sys.getenv("STY"))) paste0(" (screen `", Sys.getenv("STY"), "`)"), ".")
    )
  }
}, error = function(e) {
  capture.output(traceback(), file = file.path(config$paths[["logPath"]], "traceback_09-main-scfm.txt"), split = TRUE)

  if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
    notifications::notify_google(
      paste0("ERROR in 09-main-scfm: `", config$context[["runName"]],
             "` on host `", config$context[["machine"]], "`.\n",
             "```\n", e$message, "\n```")
    )

    stop(e$message)
  }

  if (!interactive()) {
    DBI::dbDisconnect(getOption("reproducible.conn"))
  }
})

if (isUpdated(mySimOut) || isFALSE(config$args[["useCache"]])) {
  mySimOut@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)

  message("Saving simulation to: ", fsim)
  tryCatch({
    saveSimList(mySimOut, fsim, inputs = FALSE, outputs = FALSE, cache = FALSE, files = FALSE)
  }, error = function(e) warning(e))

  ## TODO: upload

  # save simulation info ------------------------------------------------------------------------
  relOutputPath <- SpaDES.config:::.getRelativePath(paths_sim[["outputPath"]], prjDir)
  rrFile <- file.path(relOutputPath, "INFO.md")
  cat(SpaDES.config::printRunInfo(config$context), file = rrFile, sep = "")
  cat(workflowtools::reproducibilityReceipt(), file = rrFile, sep = "\n", append = TRUE)

  # save simulation stats -----------------------------------------------------------------------
  elapsed <- elapsedTime(mySimOut)
  data.table::fwrite(elapsed, file.path(paths_sim[["outputPath"]], "elapsedTime.csv"))
  qs::qsave(elapsed, file.path(paths_sim[["outputPath"]], "elapsedTime.qs"))

  if (!isFALSE(getOption("spades.memoryUseInterval"))) {
    memory <- memoryUse(mySimOut, max = TRUE)
    data.table::fwrite(memory, file.path(paths_sim[["outputPath"]], "memoryUsed.csv"))
    qs::qsave(memory, file.path(paths_sim[["outputPath"]], "memoryUsed.qs"))
  }

  # create vegetation transition plots ----------------------------------------------------------

  NDTBEC <- simOutPreamble[["ml"]][["ecoregionLayer (NDTxBEC)"]] |>
    sf::st_crop(simOutPreamble[["studyAreaReporting"]])
  rstNDTBEC <- terra::rasterize(NDTBEC, simOutPreamble[["rasterToMatch"]], field = "NDTBEC") |>
    terra::crop(simOutPreamble[["studyAreaReporting"]]) |>
    terra::mask(simOutPreamble[["studyAreaReporting"]])

  years <- seq(0, 1200, 100) ## TODO: use years from config
  fvtm <- file.path(paths_sim[["outputPath"]], sprintf("vegTypeMap_year%04d.tif", years))

  transitions_df <- vegTransitions(
    vtm = fvtm,
    ecoregion = rstNDTBEC,
    field = "NDTBEC",
    studyArea = simOutPreamble[["studyAreaReporting"]],
    times = years,
    na.rm = TRUE
  )

  transition_ggs <- plotVegTransitions(transitions_df)

  lapply(names(transition_ggs), function(i) {
    ggsave(file.path(paths_sim[["outputPath"]], "figures", paste0("transition_vegTypeMap_", i, ".png")),
           transition_ggs[[i]], width = 12, height = 6)
  })

  # archive files + upload ----------------------------------------------------------------------
  if (isTRUE(config$args[["reupload"]])) {
    resultsDir <- config$paths[["outputPath"]]

    tarball <- paste0(resultsDir, ".tar.gz")

    # withr::with_dir(resultsDir, archive::archive_write_dir(archive = tarball, dir = resultsDir)) ## TODO: verify
    utils::tar(tarball, resultsDir, compression = "gzip") ## TODO: use archive pkg

    ## upload at the end to prevent timeouts from delaying subsequent sims
  }
}

## cleanup
terra::tmpFiles(remove = TRUE)
