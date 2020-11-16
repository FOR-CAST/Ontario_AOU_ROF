################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2)

objects2 <- list(
  #"nonTreePixels" = simOutPreamble[["nonTreePixels"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "sppColorVect" = simOutPreamble[["sppColorVect"]],
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]],
  "studyAreaReporting" = simOutPreamble[["studyArea"]]
)

outputs2_2001 <- data.frame(
  objectName = "speciesLayers",
  saveTime = 2001,
  file = paste0(studyAreaName, "_speciesLayers2001_fireSense.rds")
)

outputs2_2011 <- data.frame(
  objectName = "speciesLayers",
  saveTime = 2011,
  file = paste0(studyAreaName, "_speciesLayers2011_fireSense.rds")
)

parameters2 <- list(
  Biomass_speciesData = list(
    "omitNonVegPixels" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "types" = c("KNN", "ONFRI"), ## TODO: use CASFRIv5?
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = paste0(studyAreaName, 2001),
    ".useCache" = FALSE
  )
)

## create species layers for 2001 and 2011
year <- 2001
sppLayersFile2001 <- file.path(Paths$inputPath, paste0("simOutSpeciesLayers_", studyAreaName, "_", year, ".qs"))
simOutSpeciesLayers2001 <- Cache(simInitAndSpades,
                                 times = list(start = year, end = year),
                                 params = parameters2,
                                 modules = c("Biomass_speciesData"),
                                 objects = objects2,
                                 omitArgs = c("debug", "paths", ".plotInitialTime"),
                                 outputs = outputs2_2001,
                                 paths = paths2,
                                 useCache = TRUE,
                                 useCloud = useCloudCache,
                                 cloudFolderID = cloudCacheFolderID,
                                 .plotInitialTime = year + .plotInitialTime,
                                 debug = 1)
saveSimList(Copy(simOutSpeciesLayers2001), sppLayersFile2001) ## TODO: fix issue loading simList

simOutSpeciesLayers2011 <- simOutSpeciesLayers2001
simOutSpeciesLayers2011$Biomass_speciesData$types <- c("KNN2011", "ONFRI")
simOutSpeciesLayers2011$Biomass_speciesData <- paste0(studyAreaName, 2011)
simOutSpeciesLayers2011$Biomass_speciesData$.studyAreaName <- paste0(studyAreaName, 2011)

year <- 2011
sppLayersFile2011 <- file.path(Paths$inputPath, paste0("simOutSpeciesLayers_", studyAreaName, "_", year, ".qs"))
simOutSpeciesLayers2011 <- Cache(simInitAndSpades,
                                 times = list(start = year, end = year),
                                 params = parameters2,
                                 modules = c("Biomass_speciesData"),
                                 objects = objects2,
                                 omitArgs = c("debug", "paths", ".plotInitialTime"),
                                 outputs = outputs2_2011,
                                 paths = paths2,
                                 useCache = TRUE,
                                 useCloud = useCloudCache,
                                 cloudFolderID = cloudCacheFolderID,
                                 .plotInitialTime = year + .plotInitialTime,
                                 debug = 1)
saveSimList(Copy(simOutSpeciesLayers2011), sppLayersFile2011) ## TODO: fix issue loading simList

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers2001$speciesLayers)
  Plot(simOutSpeciesLayers2011$speciesLayers)
}

################################################################################
## boreal data prep
################################################################################

do.call(SpaDES.core::setPaths, paths2a)

objects2a_2001 <- list(
  "cloudFolderID" = cloudCacheFolderID,
  "rstLCC" = simOutPreamble[["LCC"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "speciesLayers" = simOutSpeciesLayers2001[["speciesLayers"]],
  #"speciesParams" = speciesParams,
  "speciesTable" = simOutPreamble[["speciesTable"]],
  "sppColorVect" = simOutPreamble[["sppColorVect"]],
  "sppEquiv" = simOutPreamble[["sppEquiv"]],
  "standAgeMap" = simOutPreamble[["ageMap"]],
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]]
)

objects2a_2011 <- objects2a_2001
objects2a_2011[["speciesLayers"]] <- simOutSpeciesLayers2011[["speciesLayers"]]

outputs2a_2001 <- data.frame(objectName = c("cohortData",
                                            "pixelGroupMap",
                                            "speciesLayers",
                                            "standAgeMap",
                                            "rawBiomassMap"),
                             saveTime = 2001,
                             file = paste0(studyAreaName, "_",
                                           c("cohortData2001_fireSense.rds",
                                             "pixelGroupMap2001_fireSense.rds",
                                             "speciesLayers2001_fireSense.rds",
                                             "standAgeMap2001_borealDataPrep.rds",
                                             "rawBiomassMap2001_borealDataPrep.rds"))
)

outputs2a_2011 <- data.frame(objectName = c("cohortData",
                                            "pixelGroupMap",
                                            "speciesLayers",
                                            "standAgeMap",
                                            "rawBiomassMap"),
                             saveTime = 2011,
                             file = paste0(studyAreaName, "_",
                                           c("cohortData2011_fireSense.rds",
                                             "pixelGroupMap2011_fireSense.rds",
                                             "speciesLayers2011_fireSense.rds",
                                             "standAgeMap2011_borealDataPrep.rds",
                                             "rawBiomassMap2011_borealDataPrep.rds"))
)

parameters2a <- list(
  Biomass_borealDataPrep = list(
    "biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup))),
    #"biomassModel" = quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
    #                                    (logAge + cover + speciesCode | ecoregionGroup))),
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "forestedLCCClasses" = c(1:15, 20, 32, 34:36),
    "LCCClassesToReplaceNN" = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "runName" = runName,
    "pixelGroupAgeClass" = successionTimestep * 2,  ## can be coarse because initial conditions are irrelevant
    "pixelGroupBiomassClass" = 1000 / (250/resolution)^2, ## can be coarse because initial conditions are irrelevant
    "sppEquivCol" = sppEquivCol,
    "subsetDataAgeModel" = 100,
    "subsetDataBiomassModel" = 100,
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "useCloudCacheForStats" = useCloudCache,
    ".studyAreaName" = studyAreaName,
    ".useCache" = eventCaching
  )
)

## run boreal data prep
year <- 2001
dataPrepFile2001 <- file.path(Paths$inputPath, paste0("simOutDataPrep_", studyAreaName, "_", year, ".qs"))
simOutDataPrep2001 <- Cache(simInitAndSpades,
                            times = list(start = year, end = year),
                            params = parameters2a,
                            modules = c("Biomass_borealDataPrep"),
                            objects = objects2a_2001,
                            omitArgs = c("debug", "paths", ".plotInitialTime"),
                            useCache = TRUE,
                            useCloud = useCloudCache,
                            cloudFolderID = cloudCacheFolderID,
                            .plotInitialTime = year + .plotInitialTime,
                            paths = paths2a,
                            debug = 1)
saveSimList(simOutDataPrep2001, dataPrepFile2001) ## TODO: fix issue loading simList

simOutDataPrep2011 <- simOutDataPrep2001
simOutDataPrep2011$Biomass_borealDataPrep$.studyAreaName <- paste0(studyAreaName, 2011)

year <- 2011
dataPrepFile2011 <- file.path(Paths$inputPath, paste0("simOutDataPrep_", studyAreaName, "_", year, ".qs"))
simOutDataPrep2011 <- Cache(simInitAndSpades,
                            times = list(start = year, end = year),
                            params = parameters2a,
                            modules = c("Biomass_borealDataPrep"),
                            objects = objects2a_2011,
                            omitArgs = c("debug", "paths", ".plotInitialTime"),
                            useCache = TRUE,
                            useCloud = useCloudCache,
                            cloudFolderID = cloudCacheFolderID,
                            .plotInitialTime = year + .plotInitialTime,
                            paths = paths2a,
                            debug = 1)
saveSimList(simOutDataPrep2011, dataPrepFile2011) ## TODO: fix issue loading simList

################################################################################
## fireSense data prep
################################################################################

