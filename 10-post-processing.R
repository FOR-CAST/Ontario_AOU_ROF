library(Require)

Require(c("cowplot", "data.table", "ggplot2", "googledrive", "raster", "rasterVis",
          "SpaDES.core",
          "SpaDES.tools",
          "qs"))

# Require("PredictiveEcology/pemisc@development (>= 0.0.3.9001)")
# Require("PredictiveEcology/LandR@development (>= 1.0.0.9004)")
# Require("PredictiveEcology/fireSenseUtils@development (>= 0.0.4.9071)")

studyAreaNames <- c("AOU", "ROF")
climateScenarios <- c("CCSM4_RCP45", "CCSM4_RCP85")
#cs <- climateScenarios[1]
#studyAreaName <- "ROF"
Nreps <- 10

gdriveURL <- function(studyAreaName) {
  if (studyAreaName == "AOU") {
    "https://drive.google.com/drive/folders/1DWOgy-XxZO9pmgfRXEzHJPX7jU4x3Vki/"
  } else if (studyAreaName == "ROF") {
    "https://drive.google.com/drive/folders/1OjTkQVUhVq65YPGGOpijZ1ifeRWCwBA4/"
  }
}

###############

sim <- loadSimList("outputs/AOU_CCSM4_RCP85_res250_rep02/AOU_CCSM4_RCP85_res250_rep02.qs")
et <- elapsedTime(sim, units = "hours")

sim_rof <- loadSimList("outputs/ROF_CCSM4_RCP85_res125_rep02/ROF_CCSM4_RCP85_res125_rep02.qs")
et_rof <- elapsedTime(sim_rof)

# summary figures -----------------------------------------------------------------------------

## BURN SUMMARIES
lapply(studyAreaNames, function(studyAreaName) {
  lapply(climateScenarios, function(cs) {
    sim <- loadSimList(file.path("outputs", studyAreaName,
                                 paste0("simOutPreamble_", studyAreaName, "_", cs, ".qs")))
    rasterToMatch <- sim$rasterToMatchReporting
    rm(sim)

    burnSummaryAllReps <- rbindlist(lapply(1:Nreps, function(rep) {
      res <- if (studyAreaName == "AOU") 250 else if (studyAreaName == "ROF") 125
      runName <- sprintf("%s_%s_res%03d_rep%02d", studyAreaName, cs, res, rep)
      resultsDir <- file.path("outputs", runName)

      burnDT <- qs::qload(file.path(resultsDir, "burnSummary_year2100.qs"))
      burnSummary <- data.table(year = burnDT[["year"]],
                                N = burnDT[["N"]],
                                areaBurnedHa = burnDT[["areaBurnedHa"]],
                                rep = as.integer(rep))
      burnSummary ## TODO: this is the BUFFERED studyARea, not the REPORTING one!!!!
    }))

    # totAreaBurned <- burnSummaryAllReps[, lapply(.SD, sum), by = c("year", "rep"), .SDcols = "areaBurnedHa"]
    # totAreaBurend <- totAreaBurned[, lapply(.SD, mean), by = "year", .SDcols = "areaBurnedHa"]

    burnSummaryAllReps[, sumAB := sum(areaBurnedHa), by = c("year", "rep")]
    areaBurned <- unique(burnSummaryAllReps[, c("year", "rep", "sumAB")])

    tend <- lm(sumAB ~ year, data = areaBurned)
    coeff <- coefficients(tend)
    Fstats <- summary(tend)$fstatistic
    names(Fstats) <- NULL
    pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")

    areaBurned[, var := "area_burned"]
    areaBurned[, val := sumAB]

    # numberFires <- burnSummaryAllReps[, lapply(.SD, length), by = c("year", "rep"), .SDcols = "N"]
    # numberFires <- numberFires[, lapply(.SD, mean), by = "year", .SDcols = "N"]

    burnSummaryAllReps[, Nfires := length(N), by = c("year", "rep")]
    nFires <- unique(burnSummaryAllReps[, c("year", "rep", "Nfires")])

    tendF <- lm(Nfires ~ year, data = nFires)
    coeffF <- coefficients(tendF)
    Fstats <- summary(tendF)$fstatistic
    names(Fstats) <- NULL
    pValueF <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
    nFires[, var := "number_fires"]
    nFires[, val := Nfires]

    # meanFireSize <- burnSummaryAllReps[, lapply(.SD, mean), by = c("year", "rep"), .SDcols = "areaBurnedHa"]
    # meanFireSize <- meanFireSize[, lapply(.SD, mean), by = "year", .SDcols = "areaBurnedHa"]

    burnSummaryAllReps[areaBurnedHa > 6.25, fireSize := mean(areaBurnedHa, na.rm = TRUE),
                       by = c("year", "rep")]
    fireSize <- na.omit(unique(burnSummaryAllReps[, c("year", "rep", "fireSize")]))

    tendS <- lm(fireSize ~ year, data = fireSize)
    coeffS <- coefficients(tendS)
    Fstats <- summary(tendS)$fstatistic
    names(Fstats) <- NULL
    pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")

    fireSize[, var := "fire_size"]
    fireSize[, val := fireSize]

    ### plotting

    coefXA <- round(coeff[2], 1)
    coefYA <- round(coeff[1], 1)
    coefXF <- round(coeffF[2], 1)
    coefYF <- round(coeffF[1], 1)
    coefXS <- round(coeffS[2], 1)
    coefYS <- round(coeffS[1], 1)

    replacementNames <- c(
      paste0("Area burned:\n",
             "y = ", ifelse(coefXA < 10000, coefXA, formatC(coefXA, format = "e", digits = 2)),
             "x + ", ifelse(coefYA < 10000, coefYA, formatC(coefYA, format = "e", digits = 2)), pValueA),
      paste0("No fires:\n",
             "y = ", ifelse(coefXF < 10000, coefXF, formatC(coefXF, format = "e", digits = 2)),
             "x + ", ifelse(coefYF < 10000, coefYF, formatC(coefYF, format = "e", digits = 2)), pValueF),
      paste0("Mean fire size:\n",
             "y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
             "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)), pValueS)
    )
    names(replacementNames) <- c("area_burned", "number_fires", "fire_size")

    dt <- rbind(areaBurned, nFires, fireSize, use.names = FALSE)
    # Now remove original variable. It uses the first item's nameL sumAB
    dt[, sumAB := NULL]

    p1 <- ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
      geom_point(colour = "grey70") +
      stat_smooth(method = "lm", color = "darkred", fill = "red") +
      facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
      theme(legend.position = "none",
            strip.text.y = element_text(size = 9, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
      labs(y = "total area burned (ha)")
    p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
      geom_point(colour = "grey70") +
      stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
      facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
      theme(legend.position = "none",
            strip.text.y = element_text(size = 9, face = "bold"),
            plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      ylab(label = "no. of fires")
    p3 <- ggplot2::ggplot(data = dt[var == "fire_size",], aes(x = year, y = val)) +
      geom_point(colour = "grey70") +
      stat_smooth(method = "lm", color = "orange", fill = "orange") +
      facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
      theme(legend.position = "none",
            strip.text.y = element_text(size = 9, face = "bold"),
            plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
      labs(y = "mean fire size (ha)")

    title <- ggdraw() +
      draw_label(paste("Fires in the", studyAreaName, "study area under", cs))

    p <- plot_grid(p1, p2, p3, align = "h", nrow = 3, labels = "AUTO")

    fgg <- file.path("outputs", studyAreaName, "figures",
                     paste0("burnSummary_", studyAreaName, "_", cs, ".png"))
    gg <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
    ggsave(gg, filename = fgg, height = 8, width = 11)

    drive_put(fgg, gdriveURL(studyAreaName), basename(fgg), overwrite = TRUE)
  })
})

## CUMULATIVE BURN MAPS

lapply(studyAreaNames, function(studyAreaName) {
  lapply(climateScenarios, function(cs) {
    sim <- loadSimList(file.path("outputs", studyAreaName,
                                 paste0("simOutPreamble_", studyAreaName, "_", cs, ".qs")))
    rasterToMatch <- sim$rasterToMatchReporting
    rm(sim)

    burnMapAllReps <- lapply(1:Nreps, function(rep) {
      res <- if (studyAreaName == "AOU") 250 else if (studyAreaName == "ROF") 125
      runName <- sprintf("%s_%s_res%03d_rep%02d", studyAreaName, cs, res, rep)
      resultsDir <- file.path("outputs", runName)

      burnMap <- raster(file.path(resultsDir, "burnMap_2100_year2100.tif"))
    })

    cumulBurnMap <- calc(stack(burnMapAllReps), fun = sum) / Nreps
    cumulBurnMap <- mask(crop(cumulBurnMap, rasterToMatch), rasterToMatch)

    myPal <- RColorBrewer::brewer.pal("Reds", n = Nreps + 1) ## include 0 ## TODO: max 9 cols!
    myTheme <- rasterVis::rasterTheme(region = myPal)

    fburnMap <- file.path("outputs", studyAreaName, "figures",
                          paste0("cumulBurnMap_", studyAreaName, "_", cs, ".png"))
    png(filename = fburnMap, height = 800, width = 800)
    rasterVis::levelplot(cumulBurnMap, margin = list(FUN = "mean"), ## median?
                         main = paste0("Cumulative burn map 2011-2100 under ", cs),
                         colorkey = list(
                           at = seq(0, maxValue(cumulBurnMap),
                                    length.out = Nreps + 1),
                           space = "bottom",
                           axis.line = list(col = "black"),
                           width = 0.75
                         ),
                         par.settings = myTheme)
    dev.off()

    drive_put(fburnMap, gdriveURL(studyAreaName), basename(fburnMap), overwrite = TRUE)
  })
})

## leading species plots
#
# Climate Effect on Conifer --> Deciduous or Deciduous --> Conifer conversions
# 1. for each rep within a scenario, calculate difference -->
#    if conifer to decid = 1, if decid to conifer = -1, otherwise 0
# 2. Create one single map of "proportion net conversion" sum of difference / Nreps

treeSpecies <- unique(sppEquiv[, c("ON", "Type")])
setnames(treeSpecies, "ON", "Species")

treeType <- data.frame(
  leading = as.integer(c(1:length(treeSpecies[["Species"]]),
                         paste0(length(treeSpecies[["Species"]]) + 1, 1:length(treeSpecies[["Species"]])))),
  landcover = c(treeSpecies[["Species"]], paste0("Mixed_", treeSpecies[["Species"]])),
  leadingType = c(tolower(treeSpecies[["Type"]]), rep("mixed", length(treeSpecies[["Species"]]))),
  stringsAsFactors = FALSE
)
treeType$newClass <- ifelse(treeType$leadingType == "deciduous", 1,
                            ifelse(treeType$leadingType == "conifer", 0, 0.5))

leadingPercentage <- 0.8
.defineLeading <- function(x, leadingPercentage = 0.8, totalCol) {
  colID <- which(x[-length(x)] > (leadingPercentage*x[[totalCol]]))
  if (length(colID) == 0) {
    # If we don't have a leading, we need to id conifer leading,
    # or deciduous leading
    colID1 <- which.max(x[-length(x)])
    colID <- as.integer(paste0(length(x), colID1))
  }
  return(colID)
}

lapply(studyAreaNames, function(studyAreaName) {
  lapply(climateScenarios, function(cs) {
    sim <- loadSimList(file.path("outputs", studyAreaName,
                                 paste0("simOutPreamble_", studyAreaName, "_", cs, ".qs")))
    rasterToMatch <- sim$rasterToMatchReporting
    rm(sim)

    allReps <- lapply(1:10, function(rep) {
      res <- if (studyAreaName == "AOU") 250 else if (studyAreaName == "ROF") 125
      runName <- sprintf("%s_%s_res%03d_rep%02d", studyAreaName, cs, res, rep)
      resultsDir <- file.path("outputs", runName)

      bothYears <- lapply(c(2011, 2100), function(year) {
        cohortData <- qread(file = file.path(resultsDir, paste0("cohortData_", year, "_year", year, ".qs")))
        pixelGroupMap <- raster(file.path(resultsDir, paste0("pixelGroupMap_", year, "_year", year, ".tif")))

        cohortDataReduced <- cohortData[, list(sumBio = sum(B, na.rm = TRUE)), by = c("speciesCode", "pixelGroup")]

        biomassStack <- raster::stack(lapply(treeSpecies[["Species"]], function(tSp) {
          message(paste0("[", studyAreaName, "_", cs, "]: creating biomass map for ",
                         tSp, " in year ", year, " [rep ", rep, "]"))
          r <- SpaDES.tools::rasterizeReduced(reduced = cohortDataReduced[speciesCode == tSp, ],
                                              fullRaster = pixelGroupMap,
                                              newRasterCols = "sumBio",
                                              mapcode = "pixelGroup")
          r[is.na(r[])] <- 0
          r[is.na(pixelGroupMap)] <- NA
          return(r)
        }))
        names(biomassStack) <- treeSpecies[["Species"]]

        biomassDT <- data.table(pixelID = 1:raster::ncell(biomassStack), raster::getValues(biomassStack))
        biomassDT[, totalBiomass := rowSums(.SD, na.rm = TRUE),
                  .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
        biomassDT <- biomassDT[totalBiomass != 0, ]
        biomassDT[, leading := apply(.SD, 1, .defineLeading,
                                     leadingPercentage = leadingPercentage,
                                     totalCol = "totalBiomass"),
                  .SDcols = names(biomassDT)[names(biomassDT) != "pixelID"]]
        biomassDT <- merge(biomassDT, treeType[, c("leading","newClass")])
        allPixels <- data.table(pixelID = 1:raster::ncell(biomassStack))
        biomassDTfilled <- merge(allPixels, biomassDT, all.x = TRUE, by = "pixelID")
        leadingSpeciesRaster <- raster::setValues(raster(biomassStack), biomassDTfilled[["newClass"]])
        names(leadingSpeciesRaster) <- paste("biomassMap", studyAreaName, cs, sep = "_")

        leadingSpeciesRaster
      })
      names(bothYears) <- paste0("Year", c("2011", "2100"))

      leadingStackChange <- raster::calc(raster::stack(bothYears[[2]], -bothYears[[1]]),
                                         fun = sum, na.rm = TRUE)
      assertthat::assert_that(all(minValue(leadingStackChange) >= -1, maxValue(leadingStackChange) <= 1))
      leadingStackChange[is.na(rasterToMatch)] <- NA
      names(leadingStackChange) <- paste("leadingMapChange", studyAreaName, cs, rep, sep = "_")

      leadingStackChange
    })
    names(allReps) <- paste0("rep", 1:10)

    fmeanLeadingChange <- file.path("outputs", studyAreaName,
                                    paste0("leadingChange_", studyAreaName, "_", cs, ".tif"))
    meanLeadingChange <- raster::calc(raster::stack(allReps), mean, na.rm = TRUE)
    meanLeadingChange <- mask(crop(meanLeadingChange, rasterToMatch), rasterToMatch)
    writeRaster(meanLeadingChange, filename = fmeanLeadingChange, overwrite = TRUE)

    maxV <- max(abs(round(minValue(meanLeadingChange), 1)), abs(round(maxValue(meanLeadingChange), 1)))
    AT <- seq(-maxV, maxV, length.out = 12)

    pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
    pal[6] <- "#f7f4f2"

    b <- levelplot(meanLeadingChange,
                   sub = paste0("Proportional change in leading species\n",
                                " Red: conversion to conifer\n",
                                " Blue: conversion to deciduous."),
                   margin = FALSE,
                   maxpixels = 7e6,
                   at = AT,
                   colorkey = list(
                     space = "bottom",
                     axis.line = list(col = "black"),
                     width = 0.75
                   ),
                   par.settings = list(
                     strip.border = list(col = "transparent"),
                     strip.background = list(col = "transparent"),
                     axis.line = list(col = "transparent")
                   ),
                   scales = list(draw = FALSE),
                   col.regions = pal,
                   par.strip.text = list(cex = 0.8, lines = 1, col = "black"))

    fmeanLeadingChange_gg <- file.path("outputs", studyAreaName, "figures",
                                       paste0("leadingChange_", studyAreaName, "_", cs, ".png"))

    png(filename = fmeanLeadingChange_gg, width = 1000, height = 1000, res = 300) ## TODO: fails in lapply
    b
    dev.off()

    drive_put(fmeanLeadingChange_gg, gdriveURL(studyAreaName), basename(fmeanLeadingChange_gg), overwrite = TRUE)
  })
})
