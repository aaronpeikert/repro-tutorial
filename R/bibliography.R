knitr::write_bib(
  c(
    .packages(),
    "repro",
    "here",
    "rticles",
    "gert",
    "bookdown",
    "lavaan",
    "knitr",
    "targets",
    "renv",
    "tidyverse"
  ),
  here::here("packages.bib")
)
cat(c(readLines(here::here("packages.bib")), "\n", readLines(here::here("references.bib"))),
    sep = "\n",
    file = here::here("temp.bib"))
