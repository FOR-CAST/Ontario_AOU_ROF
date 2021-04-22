library(SpaDES.core)

sim <- loadSimList("outputs/AOU_CCSM4_RCP85_res250_rep02/AOU_CCSM4_RCP85_res250_rep02.qs")
et <- elapsedTime(sim, units = "hours")



sim_rof <- loadSimList("outputs/ROF_CCSM4_RCP85_res125_rep02/ROF_CCSM4_RCP85_res125_rep02.qs")
et_rof <- elapsedTime(sim_rof)



#############

library(googledrive)
library(raster)
library(SpaDES.tools)
library(qs)

lapply(1:9, function(rep) { ## TODO: change back to 1:10
  runName <- sprintf("ROF_CCSM4_RCP85_res125_rep%02d", rep)
  resultsDir <- file.path("outputs", runName)

  lapply(2011:2100, function(year) {
    cohortData <- qread(file = file.path(resultsDir, paste0("cohortData_", year, "_year", year, ".qs")))
    cohortData[, bWeightedAge := floor(sum(age * B) / sum(B) / 10) * 10, .(pixelGroup)]
    cohortDataReduced <- cohortData[, c("pixelGroup", "bWeightedAge")]
    cohortDataReduced <- unique(cohortDataReduced)
    pixelGroupMap <- raster(file.path(resultsDir, paste0("pixelGroupMap_", year, "_year", year, ".tif")))
    names(pixelGroupMap) <- "pixelGroup"
    standAgeMap <- rasterizeReduced(cohortDataReduced, pixelGroupMap, "bWeightedAge", mapCode = "pixelGroup")
    writeRaster(standAgeMap, filename = file.path(resultsDir, paste0("standAgeMap_", year, ".tif")), overwrite = TRUE)
  })

  unlink(paste0(resultsDir, ".tar.gz"))
  utils::tar(paste0(resultsDir, ".tar.gz"), resultsDir, compression = "gzip")

  retry(quote(drive_upload(paste0(resultsDir, ".tar.gz"),
                           as_id("1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4"),
                           overwrite = TRUE)),
        retries = 5, exponentialDecayBase = 2)
})
