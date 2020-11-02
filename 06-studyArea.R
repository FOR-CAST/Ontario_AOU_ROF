################################################################################
## Preamble (creates study areas, etc.)
################################################################################

do.call(SpaDES.core::setPaths, paths1) # Set them here so that we don't have to specify at each call to Cache

objects1 <- list()

parameters1 <- list(
  Ontario_preamble = list(
    ".plotInitialTime" = ifelse(usePlot, 0, NA),
    "mapResFact" = mapResFact,
    "runName" = runName
  )
)

simOutPreamble <- Cache(simInitAndSpades,
                        times = list(start = 0, end = 1),
                        params = parameters1,
                        modules = c("Ontario_preamble"),
                        objects = objects1,
                        paths = paths1,
                        debug = 1,
                        omitArgs = c("debug", "paths"),
                        #useCache = "overwrite", ## TODO: remove this workaround
                        useCloud = useCloudCache,
                        cloudFolderID = cloudCacheFolderID)

saveRDS(simOutPreamble$ml, file.path(Paths$outputPath, "ml_preamble.rds")) ## TODO: use `qs::qsave()`
