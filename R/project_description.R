options(renv.lockfile.version = 1)

workflowtools::description(
  fields = list(
    Type = "project",
    Title = paste("Assessment of Historic and Future Ranges of Variability",
                  "in Ontario's Managed Forests"),
    Description = paste(""),
    `Authors@R` = "c(
      person('Alex M', 'Chubaty', email = 'achubaty@for-cast.ca', role = c('aut', 'cre'),
             comment = c(ORCID = '0000-0001-7146-8135'))
    )",
    Version = "1.0.0",
    Language = "en-CA",
    License = "GPL-3",
    Depends = "R (== 4.3)"
  ),
  snapshot = "renv.lock"
)
