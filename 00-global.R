library(Require)
Require(c("config", "crayon", "PredictiveEcology/peutils@development"))

switch(peutils::user(),
       "achubaty" = Sys.setenv(R_CONFIG_ACTIVE = "alex"),
       Sys.setenv(R_CONFIG_ACTIVE = "test")
)
#Sys.getenv("R_CONFIG_ACTIVE") ## verify

if (isFALSE(config::get("batchmode"))) {
  runName <- paste0(
    config::get("studyarea"),
    sprintf("_CCSM4_RCP%02g", config::get("rcp")),
    "_res", config::get("resolution"),
    if (isTRUE(config::is_active("test"))) "_test" else "",
    sprintf("_rep%02g", config::get("rep"))
  )
}
stopifnot(exists("runName", envir = .GlobalEnv)) ## run name should be set: e.g., see batch_runs.R

message(crayon::red(runName))

source("01-init.R")
source("02-paths.R")
source("03-packages.R")
source("04-options.R")
source("05-sim-objects.R")

message(crayon::red(runName))

if (delayStart > 0) {
  message(crayon::green("\nStaggered job start: delaying by", as.integer(delayStart), "minutes."))
  Sys.sleep(delayStart*60)
}

source("06-studyArea.R")
source("07-dataPrep.R")

message(crayon::red(runName))

#source("08a-ignitionFit.R")
#source("08b-escapeFit.R")
source("08c-spreadFit.R")

#source("09-pre-sim.R")
