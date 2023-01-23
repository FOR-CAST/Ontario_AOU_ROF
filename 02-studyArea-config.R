## project-specific studyArea and other config

config.studyArea <- list(
  args = list(
    simYears = list(start = 2011, end = 2100)
  ),
  modules = list(
    Ontario_preamble = "Ontario_preamble"
  ),
  options = list(
    ## TODO
  ),
  params = list(
    .globals = list(
      sppEquivCol = "ON"
    ),
    Biomass_borealDataPrep = list(
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
      studyAreaName = gsub("^(ON|QC).*", "\\1", config$context[["studyAreaName"]])
    ),
    Ontario_preamble = list(
      studyAreaName = config$context[["studyAreaName"]],
      useAgeMapkNN = !grepl("ROF", config$context[["runName"]]), ## don't use kNN for ROF
      .resolution = ifelse(grepl("ROF", config$context[["studyAreaName"]]), 125, 250),
      .useCache = FALSE #".inputObjects"
    )
  ),
  paths = list(
    ## TODO
  )
)
