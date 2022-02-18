library("googledrive")

files2upload <- c(
  list.files("outputs", "(CanESM5|CNRM-ESM2-1).*[.]tar.gz$", full.names = TRUE)
)
lapply(files2upload, function(tarball) {
  retry(quote(drive_put(media = tarball, path = unique(as_id(gid_results)), name = basename(tarball))),
        retries = 5, exponentialDecayBase = 2)
})
