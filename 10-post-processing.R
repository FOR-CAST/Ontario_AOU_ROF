years <- unlist(config$args[["simYears"]])
Nreps <- config$params[[".globals"]][["reps"]]
studyAreaNames <- c("ON_AOU_6.2", "ON_AOU_6.5", "ON_AOU_6.6", "QC_boreal_6.2", "QC_boreal_6.3", "QC_boreal_6.6")
climateScenarios <- c("CanESM5_SSP370", "CanESM5_SSP585", "CNRM-ESM2-1_SSP370", "CNRM-ESM2-1_SSP585")[1] ## TODO: other scenarios

options(mc.cores = max(Nreps))

config$args[["usePrerun"]] <- TRUE
doUpload <- FALSE ## TODO: fix uploads; TRUE

source("05-google-ids.R")

gid_results <- lapply(studyAreaNames, function(sAN) {
  gdriveSims[studyArea == sAN & simObject == "results", gid]
})
names(gid_results) <- studyAreaNames

## postprocessing paths
posthocPaths <- prjPaths
posthocPaths[["outputPath"]] <- dirname(config$paths[["outputPath"]])

do.call(setPaths, posthocPaths)

## TODO: ensure files exist in the place the summary modules expect - fix in preamble + scripts
lapply(studyAreaNames, function(sAN) {
  outPathAbs <- checkPath(file.path(posthocPaths[["outputPath"]], sAN), create = TRUE)
  figPathAbs <- checkPath(file.path(posthocPaths[["outputPath"]], sAN, "figures"), create = TRUE)

  ## preamble
  fromFile <- file.path(posthocPaths[["outputPath"]], paste0(sAN, "_", climateScenarios[1]), sprintf("rep%02d", 1),
                        paste0("simOutPreamble_", sAN, "_", gsub("SSP", "", climateScenarios[1]), ".", config$args[["fsimext"]]))
  toFile <- file.path(outPathAbs,  paste0("simOutPreamble_", sAN, "_", gsub("SSP", "", climateScenarios[1]), ".", config$args[["fsimext"]]))

  if (!file.exists(toFile))
    file.copy(fromFile, toFile)

  ## fsDAtaPrep
  fromFile <- file.path(posthocPaths[["outputPath"]], paste0(sAN, "_", climateScenarios[1]), sprintf("rep%02d", 1),
                        paste0("fSsimDataPrep_", sAN, ".", config$args[["fsimext"]]))
  toFile <- file.path(outPathAbs,  paste0("fSsimDataPrep_", sAN, ".", config$args[["fsimext"]]))

  if (!file.exists(toFile))
    file.copy(fromFile, toFile)
})

# posthocModules <- list("Biomass_summary", "fireSense_summary")
posthocModules <- config$modules

posthocParams <- list(
  Biomass_summary = list(
    climateScenarios = climateScenarios,
    simOutputPath = posthocPaths[["outputPath"]], ## "outputs"
    studyAreaNames = studyAreaNames,
    reps = Nreps,
    upload = doUpload,
    year = years
  ),
  fireSense_summary = list(
    climateScenarios = climateScenarios,
    simOutputPath = posthocPaths[["outputPath"]], ## "outputs"
    studyAreaNames = studyAreaNames,
    reps = Nreps,
    upload = doUpload
  )
)

# tree species used
source("modules/Ontario_preamble/R/sppEquiv.R") ## makeSppEquivON()
sppEquiv <- makeSppEquivON()

treeSpecies <- unique(sppEquiv[, c("ON", "Type")])
setnames(treeSpecies, "ON", "Species")

## same RTM for all sims with given study area, so it doesn't matter which one we load
# sim_SA <- loadSimList(file.path(posthocPaths[["outputPath"]],
#                                 studyAreaNames[[1]],
#                                 paste0("simOutPreamble_", studyAreaNames[[1]], "_",
#                                        gsub("SSP", "", climateScenarios[[1]]), ".", config$args[["fsimext"]])))
# rasterToMatch <- sim_SA$rasterToMatchReporting
# rm(sim_SA)

# if (grepl("ROF", .studyAreaName)) {
#   if (unique(raster::res(rasterToMatch)) == 250) {
#     ## ROF uses 125m pixels
#     rasterToMatch <- raster::disaggregate(rasterToMatch, fact = 2)
#   }
# }

posthocObjects <- list(
  #rasterToMatch = rasterToMatch,
  treeSpecies = treeSpecies,
  uploadTo = gid_results
)

posthocSim <- simInitAndSpades(
  times = list(start = 0, end = 1),
  params = posthocParams,
  modules = posthocModules,
  loadOrder = unlist(posthocModules),
  objects = posthocObjects,
  paths = posthocPaths
)

# fires ---------------------------------------------------------------------------------------

if (FALSE) {
  tmp <- loadSimList(file.path("outputs", "ROF_shield_CanESM5_SSP370_run01",
                               paste0("ROF_shield_CanESM5_SSP370_run01.", config$args[["fsimext"]])))

  r <- raster::raster(tmp$rasterToMatch)
  burnSummary = copy(tmp$burnSummary)

  bs <- burnSummary[ , .N, .(igLoc)]

  r[bs$igLoc] <- bs$N

  raster::plot(r)


  myXYs <- raster::xyFromCell(object = r, cell = bs$igLoc)
  mySPs <- sp::SpatialPoints(coords = myXYs, proj4string = raster::crs(r))

  sp::plot(mySPs)


  bm <- raster::raster("outputs/ROF_shield_CanESM5_SSP370_run01/burnMap_2100_year2100.tif")
  raster::plot(bm)

  raster::setMinMax(bm)
  hist(bm[])
}

# simulation summaries ------------------------------------------------------------------------

## TODO: sim summary module?

#sim <- loadSimList("outputs/AOU_CCSM4_RCP85_res250_rep02/AOU_CCSM4_RCP85_res250_rep02.qs")
#et <- elapsedTime(sim, units = "hours")

#sim_rof <- loadSimList("outputs/ROF_CCSM4_RCP85_res125_rep02/ROF_CCSM4_RCP85_res125_rep02.qs")
#et_rof <- elapsedTime(sim_rof)
