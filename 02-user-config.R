## user + machine specific configs

.fitUsing <- if (grepl("for-cast[.]ca", .nodename)) 4 else 0

config.user <- switch(
  .user,

  ## Alex ------------------------------------------------------------------------------------------
  achubaty = list(
    args = list(
      cloud = list(
        googleUser = "achubaty@for-cast.ca",
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "@alex.chubaty"
      ),
      reupload = TRUE,
      usePrerun = FALSE
    ),
    options = list(
      reproducible.cacheSaveFormat = "qs",
      reproducible.conn = SpaDES.config::dbConnCache("postgresql")
    ),
    params = list(
      fireSense_IgnitionFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = 8L, ## ~200 GB
          "picea.for-cast.ca" = 16L,
          "pseudotsuga.for-cast.ca" = 16L, ## ~400 GB
          1L
        )
      ),
      fireSense_SpreadFit = list(
        cores = switch(
          .nodename,
          "pinus.for-cast.ca" = {
            if (.fitUsing == 4) {
              c(rep("localhost", 32), rep("picea.for-cast.ca", 52), rep("pseudotsuga.for-cast.ca", 16))
            } else if (.fitUsing == 3) {
              c(rep("localhost", 8), rep("picea.for-cast.ca", 25), rep("pseudotsuga.for-cast.ca", 67))
            } else if (.fitUsing == 2) {
              c(rep("pseudotsuga.for-cast.ca", 68), rep("picea.for-cast.ca", 32))
            } else if (.fitUsing == 1) {
              rep("pseudotsuga.for-cast.ca", 100)
            }
          },
          "picea.for-cast.ca" = {
            if (.fitUsing == 3) {
              c(rep("localhost", 25), rep("pinus.for-cast.ca", 8), rep("pseudotsuga.for-cast.ca", 67))
            } else if (.fitUsing == 2) {
              c(rep("localhost", 68), rep("pinus.for-cast.ca", 32))
            }
          },
          "pseudotsuga.for-cast.ca" = {
            rep("localhost", 100)
          }
        )
      )
    ),
    paths = list(
      scratchPath = switch(.nodename,
                           `larix.for-cast.ca` = file.path("/tmp/scratch", basename(prjDir)),
                           file.path("/mnt/scratch", .user, basename(prjDir)))
    )
  ),

  ## Ian ------------------------------------------------------------------------------------------
  ieddy = list(
    args = list(
      cloud = list(
        googleUser = "ianmseddy@gmail.com",
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = ""
      ),
      usePrerun = TRUE,
      upload_preamble = FALSE
      spades.memoryUseInterval = FALSE
    ),
    options = list(
      reproducible.cacheSaveFormat = "qs"
    ),
    params = list(
      .plotInitialTime = NA
    ),
    paths = list(
      scratchPath = file.path("/mnt/scratch", .user, basename(prjDir))
    )
  ),

  ## docker (user rstudio) -------------------------------------------------------------------------
  rstudio = list(
    args = list(
      cloud = list(
        googleUser = "", ## TODO
        useCloud = FALSE
      ),
      notifications = list(
        slackChannel = "" ## TODO
      )
    ),
    paths = list(
      cachePath = "cache_sqlite"
    )
  ),

  ## default (i.e, no changes) ---------------------------------------------------------------------
  list()
)
