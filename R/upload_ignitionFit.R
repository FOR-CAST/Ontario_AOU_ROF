Require::Require("fs")
Require::Require("googledrive")

source("05-google-ids.R")

try(fs::file_move(
  file.path(config$paths[["outputPath"]], "figures", "ignitionNoFiresFitted.png"),
  file.path(config$paths[["outputPath"]], "figures", paste0("ignitionNoFiresFitted_",
                                                                            config$context[["studyAreaName"]], ".png"))
))

try(fs::file_move(
  file.path(config$paths[["outputPath"]], "figures", "IgnitionRatePer100.png"),
  file.path(config$paths[["outputPath"]], "figures", paste0("IgnitionRatePer100_",
                                                                            config$context[["studyAreaName"]], ".png"))
))

filesToUpload <- c(
  paste0("figures/ignitionNoFiresFitted_", config$context[["studyAreaName"]], ".png"),
  paste0("figures/IgnitionRatePer100_", config$context[["studyAreaName"]], ".png")
)

gid_results <- gdriveSims[studyArea == config$context[["studyAreaName"]] & simObject == "results", gid]
lapply(filesToUpload, function(f) {
  retry(quote(drive_put(file.path(config$paths[["outputPath"]], f), unique(as_id(gid_results)))),
        retries = 5, exponentialDecayBase = 2)
})
