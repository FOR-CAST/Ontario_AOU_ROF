library(data.table)

options(Ncpus = min(parallel::detectCores() / 2, 24L))

# Require::pkgSnapshot("packages_latest.txt")
# pkgs <- read.csv("packageVersions_2023-03-21.txt")
# colnames(pkgs) |> gsub("Github", "Remote", x = _) |> gsub("SHA1", "Sha", x =_) -> colnames(pkgs)
# cranPkgs <- ghPkgs <- pkgs[is.na(pkgs$GithubUsername), ]
# ghPkgs <- pkgs[!is.na(pkgs$GithubUsername), ]

renv::snapshot(type = "all")

pkgs <- jsonlite::fromJSON(txt = "renv.lock")[["Packages"]] |>
  lapply(as.data.frame) |>
  rbindlist(fill = TRUE)
set(pkgs, NULL, "Requirements", NULL)
pkgs <- pkgs[!duplicated(pkgs)]
cranPkgs <- ghPkgs <- pkgs[is.na(RemoteUsername), ]
ghPkgs <- pkgs[!is.na(pkgs$RemoteUsername), ]

# install.packages("usethis")
usethis::use_description(fields = list(
  Type = "project",
  Title = "Assessment of Historic and Future Ranges of Variability in Ontario's Managed Forests",
  Description = paste(""),
  Package = NULL,
  `Authors@R` = "c(
    person('Alex M', 'Chubaty', email = 'achubaty@for-cast.ca', role = c('aut', 'cre'),
           comment = c(ORCID = '0000-0001-7146-8135'))
  )",
  Version = "1.0.0",
  Language = "en-CA",
  License = "GPL-3",
  Depends = paste0("R (== 4.3)", collapse = ",\n    "),
  Imports = paste0(pkgs$Package, " (== ", pkgs$Version, ")", collapse = ",\n    "),
  Remotes = paste0(ghPkgs$RemoteUsername, "/", ghPkgs$RemoteRepo, "@", ghPkgs$RemoteSha, collapse = ",\n    ")
), check_name = FALSE, roxygen = FALSE)

if (FALSE) {
  ## first run only ------------------------
  # install.packages("renv")
  # file.move(".Rprofile", ".Rprofile-old")
  renv::init(bare = TRUE)

  renv::install(paste0(cranPkgs$Package, "@", cranPkgs$Version))
  renv::install(paste0(ghPkgs$GithubUsername, "/", ghPkgs$GithubRepo, "@", ghPkgs$GithubSHA1))

  renv::settings$ignored.packages(c("SpaDES", "SpaDES.project")) ## per-machine
  renv::settings$snapshot.type("explicit") ## per-machine

  renv::snapshot()

  renv::activate()

  ## first + subsequent runs ---------------
  renv::snapshot()

  ## restoring packages from snapshot ------
  # options(renv.config.cache.symlinks = FALSE)
  options(Ncpus = min(parallel::detectCores() / 2, 24L))

  renv::restore()
  # renv::isolate() ## not needed if symlinks option set to FALSE per above
}
