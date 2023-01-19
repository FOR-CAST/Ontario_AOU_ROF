gid_preamble <- gdriveSims[studyArea == .studyAreaName & simObject == "simOutPreamble" &
                             gcm == .climateGCM & ssp == .climateSSP, gid]
upload_preamble <- config$context[["rep"]] == 1 & (config$args[["reupload"]] | length(gid_preamble) == 0)

preambleObjects <- list(
  .runName = config$context[["runName"]]
)

preambleParams <- list(
  # config$params[[".globals"]],
  canClimateData = config$params[["canClimateData"]],
  Ontario_preamble = config$params[["Ontario_preamble"]]
)

preambleModules <- list("Ontario_preamble", "canClimateData")  ## TODO: use config$modules

fsimOutPreamble <- simFile(paste0("simOutPreamble_", config$context[["studyAreaName"]],
                                  "_", config$context[["climateGCM"]],
                                  "_", config$context[["climateSSP"]]),
                           prjPaths[["outputPath"]], ext = "qs")

if (isTRUE(config$args[["usePrerun"]]) & isFALSE(upload_preamble)) {
  if (!file.exists(fsimOutPreamble)) {
    googledrive::drive_download(file = as_id(gid_preamble), path = fsimOutPreamble)
  }
  simOutPreamble <- loadSimList(fsimOutPreamble)
} else {
  simOutPreamble <- simInitAndSpades(
    times = list(start = 0, end = 1),
    params = preambleParams,
    modules = preambleModules,
    loadOrder = unlist(preambleModules),
    objects = preambleObjects
  )

  if (isTRUE(attr(simOutPreamble, ".Cache")[["newCache"]])) {
    simOutPreamble@.xData[["._sessionInfo"]] <- projectSessionInfo(prjDir)
    saveSimList(simOutPreamble, fsimOutPreamble, fileBackend = 2)
  }

  if (isTRUE(upload_preamble)) {
    fdf <- googledrive::drive_put(media = fsimOutPreamble, path = as_id(gdriveURL), name = basename(fsimOutPreamble))
    gid_preamble <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- update_googleids(
      data.table(studyArea = studyAreaName, simObject = "simOutPreamble", runID = NA,
                 gcm = climateGCM, ssp = climateSSP, gid = gid_preamble),
      gdriveSims
    )
  }
}

firstRunMDCplots <- if (config$context[["rep"]] == 1 && config$args[["reupload"]]) TRUE else FALSE

if (isTRUE(firstRunMDCplots)) {
  ggMDC <- fireSenseUtils::compareMDC(
    historicalMDC = simOutPreamble$historicalClimateRasters$MDC,
    projectedMDC = simOutPreamble$projectedClimateRasters$MDC,
    flammableRTM = simOutPreamble$flammableRTM
  )
  fggMDC <- file.path(config$paths[["outputPath"]], "figures",
                      paste0("compareMDC_", studyAreaName, "_", climateGCM, "_", climateSSP, ".png"))
  checkPath(dirname(fggMDC), create = TRUE)

  ggplot2::ggsave(plot = ggMDC, filename = fggMDC)

  if (isTRUE(upload_preamble)) {
    googledrive::drive_put(
      media = fggMDC,
      path = unique(as_id(gdriveSims[studyArea == studyAreaName & simObject == "results", gid])),
      name = basename(fggMDC)
    )
  }
}

nSpecies <- length(unique(simOutPreamble$sppEquiv$LandR))
