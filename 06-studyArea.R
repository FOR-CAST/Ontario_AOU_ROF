source("05-google-ids.R")

gid_preamble <- gdriveSims[studyArea == .studyAreaName & simObject == "simOutPreamble" &
                             gcm == .climateGCM & ssp == .climateSSP, gid]
# upload_preamble <- config$context[["rep"]] == 1 & (config$args[["reupload"]] | length(gid_preamble) == 0)
upload_preamble <- FALSE ## TODO: restore uploads

# paths ---------------------------------------------------------------------------------------

if (!"postprocess" %in% config$context[["mode"]]) {
  ## don't need replicated copies of preamble outputs
  repID <- basename(config$paths[["outputPath"]])
  config$paths[["outputPath"]] <- dirname(config$paths[["outputPath"]]) ## TODO: add to config
  checkPath(config$paths[["logPath"]], create = TRUE)
}

# modules & parameters ------------------------------------------------------------------------

if (grepl("^ON", config$context[["studyAreaName"]])) {
  preambleParams <- list(
    # config$params[[".globals"]],
    Ontario_preamble = config$params[["Ontario_preamble"]]
  )

  preambleModules <- list("Ontario_preamble") ## TODO: use config$modules
} else if (grepl("^QC", config$context[["studyAreaName"]])) {
  preambleParams <- list(
    # config$params[[".globals"]],
    Quebec_fires_preamble = config$params[["Quebec_fires_preamble"]]
  )

  preambleModules <- list("Quebec_fires_preamble") ## TODO: use config$modules
} else {
  stop("Currently only ON and QC study areas supported.")
}

if (config$context[["fireModel"]] == "firesense") {
  preambleParams <- modList(preambleParams, list(canClimateData = config$params[["canClimateData"]]))
  preambleModules <- append(preambleModules, "canClimateData") |> unique()
}

# objects -------------------------------------------------------------------------------------

preambleObjects <- list()

# outputs -------------------------------------------------------------------------------------

outputs1 <- data.frame()

# run simulation ------------------------------------------------------------------------------

preambleFile <- simFile(
  name = paste0("simOutPreamble_", config$context[["studyAreaName"]],
                "_", config$context[["climateGCM"]],
                "_", config$context[["climateSSP"]]),
  path = config$paths[["outputPath"]],
  ext = config$args[["fsimext"]]
)

if (isTRUE(config$args[["usePrerun"]]) && isFALSE(upload_preamble)) {
  if (!file.exists(preambleFile)) {
    googledrive::drive_download(file = as_id(gid_preamble), path = preambleFile)
  }
  simOutPreamble <- loadSimList(preambleFile)
} else {
  tryCatch({
    simOutPreamble <- simInitAndSpades(
      times = list(start = 0, end = 1),
      params = preambleParams,
      modules = preambleModules,
      loadOrder = unlist(preambleModules),
      objects = preambleObjects
    )

    ## TODO: find and fix these warnings:
    ## 4: In assessDataTypeOuter(from, ...elt(hasMethod)) :
    ##   method is bilinear, but the data are integer; please confirm this is correct
    ## 5: In assessDataTypeOuter(from, ...elt(hasMethod)) :
    ##   method is bilinear, but the data are integer; please confirm this is correct

  }, error = function(e) {
    if (requireNamespace("notifications") & file.exists("~/.rgooglespaces")) {
      notifications::notify_google(
        paste0("ERROR in 06-studyArea: `", config$context[["runName"]],
               "` on host `", config$context[["machine"]], "`.\n",
               "```\n", e$message, "\n```")
      )

      stop(e$message)
    }
  })

  if (isUpdated(simOutPreamble) || isFALSE(config$args[["useCache"]])) {
    simOutPreamble@.xData[["._sessionInfo"]] <- workflowtools::projectSessionInfo(prjDir)
    saveSimList(
      simOutPreamble,
      preambleFile,
      inputs = FALSE,
      outputs = FALSE,
      cache = FALSE,
      files = FALSE
    )
    amc::.gc()
  }

  if (isTRUE(upload_preamble)) {
    source("05-google-ids.R")

    fdf <- googledrive::drive_put(media = fsimOutPreamble, path = as_id(gdriveURL), name = basename(fsimOutPreamble))
    gid_preamble <- as.character(fdf$id)
    rm(fdf)
    gdriveSims <- data.table(
      studyArea = config$context[["studyAreaName"]],
      simObject = "simOutPreamble",
      runID = NA,
      gcm = config$context[["climateGCM"]],
      ssp = config$context[["climateSSP"]],
      gid = gid_preamble
    ) |>
      update_googleids(gdriveSims)
  }
}

firstRunMDCplots <- if (config$context[["rep"]] == 1 && config$args[["reupload"]]) TRUE else FALSE

## TODO move to canClimateData
if (config$context[["fireModel"]] == "firesense" && isTRUE(firstRunMDCplots)) {
  ggMDC <- fireSenseUtils::compareMDC(
    historicalMDC = simOutPreamble[["historicalClimateRasters"]][["MDC"]],
    projectedMDC = simOutPreamble[["projectedClimateRasters"]][["MDC"]],
    flammableRTM = simOutPreamble[["flammableRTM"]]
  )
  fggMDC <- file.path(config$paths[["outputPath"]], "figures",
                      paste0("compareMDC_", config$context[["studyAreaName"]], "_",
                             config$context[["climateGCM"]], "_",
                             config$context[["climateSSP"]], ".png"))
  checkPath(dirname(fggMDC), create = TRUE)

  ggplot2::ggsave(plot = ggMDC, filename = fggMDC)

  if (isTRUE(upload_preamble)) {
    source("05-google-ids.R")

    googledrive::drive_put(
      media = fggMDC,
      path = unique(as_id(gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid])),
      name = basename(fggMDC)
    )
  }
}

nSpecies <- length(unique(simOutPreamble[["sppEquiv"]][["LandR"]]))

stopifnot(
  !is.null(attr(simOutPreamble[["standAgeMap2001"]], "imputedPixID")),
  !is.null(attr(simOutPreamble[["standAgeMap2011"]], "imputedPixID"))
)

## restore paths + cleanup
if (!"postprocess" %in% config$context[["mode"]]) {
  config$paths[["outputPath"]] <- file.path(config$paths[["outputPath"]], repID)
}
terra::tmpFiles(remove = TRUE)
