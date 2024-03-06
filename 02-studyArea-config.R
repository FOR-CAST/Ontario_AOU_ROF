## project-specific studyArea and other config
.historicFireYears <- 2011:2022 ## TODO: change this for HRV

config.studyArea <- list(
  args = list(
    simYears = list(start = 2011, end = 2100) ## TODO: change this for HRV
  ),
  modules = list(
    ## include ON and QC preambles, which are dropped below based on study area to be run
    Ontario_preamble = "Ontario_preamble",
    Quebec_fires_preamble = "Quebec_fires_preamble"
  ),
  options = list(
    ## TODO
  ),
  params = list(
    .globals = list(
      sppEquivCol = if (grepl("^ON", config$context[["studyAreaName"]])) "ON" else "LandR",
      .runInitialTime = max(.historicFireYears) + 1 ## fireSense simulates fires for years w/o data; TODO: add to config
    ),
    Biomass_borealDataPrep = list(
      forestedLCCClasses = if (grepl("^ON_ROF", config$context[["studyAreaName"]])) c(9:10, 12, 14, 15:18) else 1:6,
      overrideAgeInFires = if (grepl("^ON_ROF", config$context[["studyAreaName"]])) FALSE else TRUE,
      overrideBiomassInFires = if (grepl("^ON_ROF", config$context[["studyAreaName"]])) FALSE else TRUE,
      speciesTableAreas = c("WestON"),
      subsetDataAgeModel = 50,
      subsetDataBiomassModel = 50
    ),
    Biomass_speciesData = list(
      types = if (grepl("ON", config$context[["studyAreaName"]])) {
        c("KNN", "ONFRI")
      } else {
        "KNN" ## TODO: even for ROF??
      }
    ),
    canClimateData = list(
      ## TODO
    ),
    historicFires = list( ## TODO: add to config
      staticFireYears = .historicFireYears  ## TODO: not for HRV
    ),
    ## include ON and QC preambles, which are dropped below based on study area to be run
    Ontario_preamble = list(
      studyAreaName = config$context[["studyAreaName"]],
      useAgeMapkNN = !grepl("ROF", config$context[["runName"]]), ## don't use kNN for ROF
      .resolution = ifelse(grepl("ROF", config$context[["studyAreaName"]]), 125, 250),
      .useCache = FALSE #".inputObjects"
    ),
    Quebec_fires_preamble = list(
      studyAreaName = config$context[["studyAreaName"]],
      .useCache = FALSE #".inputObjects"
    )
  ),
  paths = list(
    ## TODO
  )
)

## drop modules/params based on whether ON or QC is being run
if (grepl("^ON_", config$context[["studyAreaName"]])) {
  config.studyArea[["modules"]][["Quebec_fires_preamble"]] <- NULL
  config.studyArea[["params"]][["Quebec_fires_preamble"]] <- NULL
} else if (grepl("^QC_", config$context[["studyAreaName"]])) {
  config.studyArea[["params"]][["Ontario_preamble"]] <- NULL
  config.studyArea[["modules"]][["Ontario_preamble"]] <- NULL
} else {
  stop("Currently only study areas in ON and QC supported, and mush be prefixed by 'ON_' or 'QC_', respectively.")
}
