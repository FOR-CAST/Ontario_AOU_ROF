# sAN <- "ON_AOU_6.1" ## terrible ignition fits; sims can't run
sAN <- "ON_AOU_6.2" ## 370 running; 585 done
#sAN <- "ON_AOU_6.5" ## 370 done; 585 done
#sAN <- "ON_AOU_6.6" ## 370 done; 585 running

fit <- FALSE

gcm <- "CanESM5"
# gcm <- "CNRM-ESM2-1"

# ssp <- 245
# ssp <- 370
ssp <- 585

Nstart <- 1L
Nreps <- 10L

delay <- 0L * 3600L

reps2run <- if (isTRUE(fit)) {
  Nstart ## fitting loops already in script
} else {
  Nstart:Nreps
}

lapply(reps2run, function(rep) {
  cmd <- paste(
    sprintf("screen -d -m -S %s_%02d Rscript -e '.rep <- %d; .studyAreaName <- \"%s\"; .climateGCM <- \"%s\"; .climateSSP <- \"%d\";",
            sAN, rep, rep, sAN, gcm, ssp),
    if (isTRUE(fit)) sprintf(".mode <- c(\"production\", \"fit\");") else ".mode <- \"production\";",
    sprintf("Sys.sleep(%d);", delay),
    "source(\"00-global.R\")'"
  )
  system(cmd, intern = TRUE)
  Sys.sleep(5)
})

rm(delay, fit, gcm, Nstart, Nreps, reps2run, sAN, ssp)
