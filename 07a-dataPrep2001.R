do.call(SpaDES.core::setPaths, paths2a)

source("05-google-ids.R")
newGoogleIDs <- gdriveSims[["biomassMaps2001"]] == ""

year <- 2001

modules2a <- list("Biomass_speciesData", "Biomass_borealDataPrep")

objects2a_2001 <- list(
  ".runName" = runName,
  "cloudFolderID" = cloudCacheFolderID,
  "rstLCC" = simOutPreamble[["LCC"]],
  #"nonTreePixels" = simOutPreamble[["nonTreePixels"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "speciesTable" = simOutPreamble[["speciesTable"]],
  "sppColorVect" = simOutPreamble[["sppColorVect"]],
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "standAgeMap" = simOutPreamble[["standAgeMap2001"]],
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]],
  "studyAreaReporting" = simOutPreamble[["studyArea"]]
)

outputs2a_2001 <- data.frame(
  objectName = c("cohortData",
                 "pixelGroupMap",
                 "speciesLayers",
                 "standAgeMap",
                 "rawBiomassMap"),
  saveTime = year,
  file = paste0(studyAreaName, "_",
                c("cohortData2001_fireSense.rds",
                  "pixelGroupMap2001_fireSense.rds",
                  "speciesLayers2001_fireSense.rds",
                  "standAgeMap2001_borealDataPrep.rds",
                  "rawBiomassMap2001_borealDataPrep.rds"))
)

parameters2a_2001 <- list(
  Biomass_speciesData = list(
    "omitNonVegPixels" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "types" = c("KNN", "ONFRI"), ## TODO: use CASFRIv5?
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = paste0(studyAreaName, 2001),
    ".useCache" = FALSE
  ),
  Biomass_borealDataPrep = list(
    # "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup))),
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                                        (logAge + cover | ecoregionGroup))),
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "forestedLCCClasses" = c(1:15, 20, 32, 34:36),
    "LCCClassesToReplaceNN" = 34:35,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "runName" = runName,
    "pixelGroupAgeClass" = successionTimestep * 2,  ## can be coarse because initial conditions are irrelevant
    "pixelGroupBiomassClass" = 1000 / (250/resolution)^2, ## can be coarse because initial conditions are irrelevant
    "speciesTableAreas" = c("WestON"),
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "sppEquivCol" = sppEquivCol,
    "subsetDataAgeModel" = 50,
    "subsetDataBiomassModel" = 50,
    "useCloudCacheForStats" = useCloudCache,
    ".studyAreaName" = paste0(studyAreaName, 2001),
    ".useCache" = eventCaching
  )
)

fBiomassMaps2001 <- file.path(Paths$outputPath, paste0("simOutDataPrep_", studyAreaName, "_", year, ".qs"))
simOutBiomassMaps2001 <- Cache(
  simInitAndSpades,
  times = list(start = year, end = year),
  params = parameters2a_2001,
  modules = modules2a,
  objects = objects2a_2001,
  loadOrder = unlist(modules2a),
  omitArgs = c("debug", "paths", ".plotInitialTime"),
  useCache = TRUE,
  useCloud = useCloudCache,
  cloudFolderID = cloudCacheFolderID,
  .plotInitialTime = year + .plotInitialTime,
  paths = paths2a,
  debug = 1
)
saveSimList(simOutBiomassMaps2001, fBiomassMaps2001, fileBackend = 2) ## TODO: use fileBackend = 1

## UPLOAD TO GOOGLE DRIVE
if (isTRUE(newGoogleIDs)) {
  googledrive::drive_put(media = fBiomassMaps2001, path = gdriveURL, name = basename(fBiomassMaps2001), verbose = TRUE)
  #googledrive::drive_put(media = aBiomassMaps2001, path = gdriveURL, name = basename(aBiomassMaps2001), verbose = TRUE)
} else {
  googledrive::drive_update(file = as_id(gdriveSims[["BiomassMaps2001"]]), media = fBiomassMaps2001)
  #googledrive::drive_update(file = as_id(gdriveSims[["BiomassMaps2001Archive"]]), media = aBiomassMaps2001)
}

## PLOTTING
if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers2001$speciesLayers)
}
