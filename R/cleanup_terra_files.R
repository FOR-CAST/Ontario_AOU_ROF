## show temporary terra files from current session
terra::tmpFiles()

## old files can only be found if terra uses the R tempdir
terra::tmpFiles(current = FALSE, orphan = TRUE, old = FALSE)

## cleanup old/orphaned terra files from other sesssions
terra::tmpFiles(current = FALSE, orphan = TRUE, old = TRUE, remove = TRUE)
