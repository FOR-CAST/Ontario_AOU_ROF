################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2)

objects2 <- list(
  "nonTreePixels" = simOutPreamble[["nonTreePixels"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]],
  "studyAreaReporting" = simOutPreamble[["studyAreaReporting"]]
)

parameters2 <- list(
  Biomass_speciesData = list(
    "omitNonVegPixels" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "types" = c("KNN", "CASFRI", "Pickell", "ForestInventory"),
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = studyAreaName,
    ".useCache" = FALSE
  )
)

sppLayersFile <- file.path(Paths$inputPath, paste0("simOutSpeciesLayers_", studyAreaName, ".qs"))
if (isTRUE(rerunSpeciesLayers)) {
  ## delete existing species layers data
  if (peutils::user("achubaty") && isTRUE(deleteSpeciesLayers)) {
    exts <- c(".tif", ".tif.vat.dbf", ".tif.vat.cpg", ".tif.ovr", ".tif.aux.xml", ".tfw")
    forInvFiles <- vapply(c("BlackSpruce1", "Deciduous1", "Fir1", "Pine1", "WhiteSpruce1"),
                          function(f) {
                            paste0(f, exts)
                          }, character(length(exts))) %>%
      c(., "CurrentCondition.zip") %>%
      file.path(paths2$inputPath, .)
    vapply(forInvFiles, function(f) if (file.exists(f)) file.remove(f) else FALSE, logical(1))
  }
}
## (re)create species layers
simOutSpeciesLayers <- Cache(simInitAndSpades,
                             times = list(start = 0, end = 1),
                             params = parameters2,
                             modules = c("Biomass_speciesData"),
                             objects = objects2,
                             omitArgs = c("debug", "paths", ".plotInitialTime"),
                             useCache = if (isTRUE(rerunSpeciesLayers)) "overwrite" else TRUE,
                             useCloud = useCloudCache,
                             cloudFolderID = cloudCacheFolderID,
                             ## make .plotInitialTime an argument, not a parameter:
                             ##  - Cache will see them as unchanged regardless of value
                             .plotInitialTime = .plotInitialTime,
                             paths = paths2,
                             debug = 1)
saveSimList(Copy(simOutSpeciesLayers), sppLayersFile) ## TODO: fix issue loading simList

if (!is.na(.plotInitialTime)) {
  lapply(dev.list(), function(x) {
    try(quickPlot::clearPlot(force = TRUE))
    try(dev.off())
  })
  quickPlot::dev(3, width = 18, height = 10)
  grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
  grid::grid.text(label = runName, x = 0.90, y = 0.03)

  Plot(simOutSpeciesLayers$speciesLayers)
}

####################

################################################################################
## species layers
################################################################################

do.call(SpaDES.core::setPaths, paths2a)

objects2a <- list(
  "cloudFolderID" = cloudCacheFolderID,
  "rstLCC" = simOutPreamble[["LCC"]],
  "rasterToMatch" = simOutPreamble[["rasterToMatch"]],
  "rasterToMatchLarge" = simOutPreamble[["rasterToMatchLarge"]],
  "speciesLayers" = simOutSpeciesLayers[["speciesLayers"]],
  "speciesParams" = speciesParams,
  "speciesTable" = speciesTable,
  "sppColorVect" = sppColorVect,
  "sppEquiv" = sppEquivalencies_CA,
  "standAgeMap" = simOutPreamble[["CC TSF"]],
  "studyArea" = simOutPreamble[["studyArea"]],
  "studyAreaLarge" = simOutPreamble[["studyAreaLarge"]]
)

parameters2a <- list(
  Biomass_borealDataPrep = list(
    ## fastLM is ~35% faster than the default lmer but needs 820GB RAM !!
    ## also, fastLM cannot deal with rank-deficient models
    #"biomassModel" = quote(RcppArmadillo::fastLm(formula = B ~ logAge * speciesCode * ecoregionGroup +
    #                                               cover * speciesCode * ecoregionGroup)),
    "biomassModel" = if (grepl("FMU", runName)) {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode + (1 | ecoregionGroup)))
    } else {
      quote(lme4::lmer(B ~ logAge * speciesCode + cover * speciesCode +
                         (logAge + cover + speciesCode | ecoregionGroup)))
    },
    "ecoregionLayerField" = "ECOREGION", # "ECODISTRIC"
    "forestedLCCClasses" = c(1:15, 20, 32, 34:36),
    "LCCClassesToReplaceNN" = 34:36,
    # next two are used when assigning pixelGroup membership; what resolution for
    #   age and biomass
    "runName" = runName,
    "pixelGroupAgeClass" = successionTimestep * 2,  ## can be coarse because initial conditions are irrelevant
    "pixelGroupBiomassClass" = 1000 / mapResFact^2, ## can be coarse because initial conditions are irrelevant
    "sppEquivCol" = sppEquivCol,
    "subsetDataAgeModel" = 100,
    "subsetDataBiomassModel" = 100,
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(LandR::updateSpeciesTable(sim$species, sim$speciesParams))
    ),
    "useCloudCacheForStats" = useCloudCache, #TRUE,
    ".plotInitialTime" = .plotInitialTime,
    ".studyAreaName" = studyAreaName,
    ".useCache" = eventCaching
  )
)

dataPrepFile <- file.path(Paths$inputPath, paste0("simOutDataPrep_", substr(runName, 1, 8), ".qs"))
## (re)run boreal data prep
simOutDataPrep <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters2a,
                        modules = c("Biomass_borealDataPrep"),
                        objects = objects2a,
                        omitArgs = c("debug", "paths", ".plotInitialTime"),
                        useCache = if (isTRUE(rerunDataPrep)) "overwrite" else TRUE,
                        useCloud = useCloudCache,
                        cloudFolderID = cloudCacheFolderID,
                        ## make .plotInitialTime an argument, not a parameter:
                        ##  - Cache will see them as unchanged regardless of value
                        .plotInitialTime = .plotInitialTime,
                        paths = paths2a,
                        debug = 1)
saveSimList(simOutDataPrep, dataPrepFile) ## TODO: fix issue loading simList
