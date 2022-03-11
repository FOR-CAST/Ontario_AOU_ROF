moduleDir <- "modules"

years <- c(2011, 2100)
Nreps <- 5 ## adjust as needed
studyAreaNames <- c("ROF", "ROF-kNN")[1] ## TODO #c("ROF_plain", "ROF_shield") #c("AOU", "ROF")
climateScenarios <- c("CanESM5_SSP370", "CanESM5_SSP585", "CNRM-ESM2-1_SSP370", "CNRM-ESM2-1_SSP585")[1] ## TODO

runName <- sprintf("%s_%s_run01", studyAreaNames[1], climateScenarios[1]) ## need a runName for gids

source("01-packages.R")
source("02-init.R")
source("03-paths.R")
source("04-options.R"); options(mc.cores = nReps);
source("05-google-ids.R")

usePrerun <- TRUE
doUpload <- TRUE

gid_results <- lapply(studyAreaNames, function(sAN) {
  gdriveSims[studyArea == sAN & simObject == "results", gid]
})
names(gid_results) <- studyAreaNames

do.call(setPaths, posthocPaths)

posthocModules <- list("Biomass_summary", "fireSense_summary")

posthocParams <- list(
  Biomass_summary = list(
    climateScenarios = climateScenarios,
    simOutputPath = dirname(defaultPaths$outputPath), ## "outputs"
    studyAreaNames = studyAreaNames,
    reps = Nreps,
    upload = doUpload,
    year = years
  ),
  fireSense_summary = list(
    climateScenarios = climateScenarios,
    simOutputPath = dirname(defaultPaths$outputPath), ## "outputs"
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

## same RTM for all sims, so it doesn't matter which one we load
sim_SA <- loadSimList(file.path("outputs", studyAreaNames[[1]],
                                paste0("simOutPreamble_", studyAreaNames[[1]], "_",
                                       gsub("SSP", "", climateScenarios[[1]]), ".qs")))
rasterToMatch <- sim_SA$rasterToMatchReporting
rm(sim_SA)

if (grepl("ROF", studyAreaName)) {
  if (unique(raster::res(rasterToMatch)) == 250) {
    ## ROF uses 125m pixels
    rasterToMatch <- raster::disaggregate(rasterToMatch, fact = 2)
  }
}

posthocObjects <- list(
  rasterToMatch = rasterToMatch,
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

# simulation summaries ------------------------------------------------------------------------

## TODO: sim summary module?

#sim <- loadSimList("outputs/AOU_CCSM4_RCP85_res250_rep02/AOU_CCSM4_RCP85_res250_rep02.qs")
#et <- elapsedTime(sim, units = "hours")

#sim_rof <- loadSimList("outputs/ROF_CCSM4_RCP85_res125_rep02/ROF_CCSM4_RCP85_res125_rep02.qs")
#et_rof <- elapsedTime(sim_rof)
