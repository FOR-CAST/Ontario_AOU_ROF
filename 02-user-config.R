## user + machine specific configs

.fitUsing <- if (grepl("for-cast[.]ca", .nodename)) 4 else 0

if (grepl("picea[.]for-cast[.]ca", .nodename)) {
  .fitUsing <- 2
}

config.user <- switch(
  .user,

  ## Alex ------------------------------------------------------------------------------------------
  achubaty = list(
    args = list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      reupload = FALSE, ## TODO: don't reupload for now, while updating + testing
      usePrerun = FALSE
    ),
    options = list(
      LandR.assertions = isTRUE("development" %in% .mode),
      # parallelly.makeNodePSOCK.setup_strategy = "sequential", ## can be slow, but sometime needed
      pemisc.useParallel = pemisc::optimalClusterNum(1000, .ncores), ## used by scfmDriver
      renv.config.sandbox.enabled = FALSE, ## copying pkgs to sandbox is too slow during cluster setup
      reproducible.cacheSaveFormat = "rds", ## TODO: use qs once Cache is fixed (reproducible#359)
      reproducible.conn = SpaDES.config::dbConnCache("postgresql"),
      # reproducible.gdalwarp = TRUE,
      spades.memoryUseInterval = FALSE ## TODO: temp workaround for broken memuse
    ),
    params = list(
      .globals = list(
        ##
      ),
      fireSense_IgnitionFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = 8L, ## TODO: update mem estimate: ~200 GB
          "picea.for-cast.ca" = 16L,
          "pseudotsuga.for-cast.ca" = 16L, ## TODO: update mem estimate: ~400 GB
          1L
        )
      ),
      fireSense_SpreadFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = {
            switch(
              .fitUsing,
              `1` = rep("pseudotsuga.for-cast.ca", 100),
              `2` = c(rep("picea.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 68)),
              `3` = c(rep("localhost", 8), rep("picea.for-cast.ca", 25), rep("pseudotsuga.for-cast.ca", 67)),
              `4` = c(rep("localhost", 32), rep("picea.for-cast.ca", 48), rep("pseudotsuga.for-cast.ca", 20)),
              `5` = c(rep("localhost", 32), rep("pseudotsuga.for-cast.ca", 68))
            )
          },
          "picea.for-cast.ca" = {
            switch(
              .fitUsing,
              `2` = c(rep("localhost", 68), rep("pinus.for-cast.ca", 32)),
              `3` = c(rep("localhost", 25), rep("pinus.for-cast.ca", 8), rep("pseudotsuga.for-cast.ca", 67)),
              `4` = c(rep("localhost", 48), rep("pinus.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 20)),
              `5` = c(rep("pinus.for-cast.ca", 32), rep("pseudotsuga.for-cast.ca", 68))
            )
          },
          "pseudotsuga.for-cast.ca" = {
            rep("localhost", 100)
          }
        )
      ),
      LandWeb_summary = list(
        .clInit = function() {
          ## NOTE: everything here has to be able to run from clean R session
          if (file.exists("Ontario_AOU_ROF.Renviron")) readRenviron("Ontario_AOU_ROF.Renviron") ## database credentials

          try(options(reproducible.conn = NULL)) ## ensure it's not set
          options(reproducible.conn = SpaDES.config::dbConnCache("postgresql"))

          return(invisible(NULL))
        },
        upload = FALSE ## TODO: use TRUE once `uploadTo` specified per study area
      )
    ),
    paths = list(
      scratchPath = switch(.nodename,
                           `larix.for-cast.ca` = file.path("/tmp/scratch", basename(prjDir)),
                           file.path("/mnt/scratch", .user, basename(prjDir)))
    )
  ),

  ## docker (user rstudio) -------------------------------------------------------------------------
  rstudio = list(
    args = list(
      cloud = list(
        googleUser = "", ## TODO
        useCloud = FALSE
      ),
    ),
    paths = list(
      cachePath = "cache_sqlite"
    )
  ),

  ## default (i.e, no changes) ---------------------------------------------------------------------
  list()
)
