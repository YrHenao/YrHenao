
if (!require(xfun)) install.packages("xfun")

library(xfun)
library(ggplot2)
library(gridExtra)
library(blastula)
library(keyring)
library(emayili)
library(DBI)
library(odbc)

libraries_basic <- c(
  "beepr",
  "conflicted",
  "fs",
  "rlang"
)

libraries_tidyverse <- c(
  "glue",
  "knitr",
  "lubridate",
  "magrittr",
  "rmarkdown",
  "tidyverse",
  "stringi"
)

libraries_sources <- c(
  "readxl",
  "writexl",
  "odbc",
  "RODBC",
  "RSocrata",
  "feather",
  "xml2"
)

libraries_utilities <- c(
  "janitor",
  "scales",
  "Hmisc",
  "naniar",
  "ggthemes",
  "viridis",
  "ggrepel",
  "socviz",
  "stringdist"
)

libraries_reports <- c(
  "kableExtra",
  "DiagrammeR",
  "pander",
  "hrbrthemes",
  "htmlwidgets",
  "webshot",
  "DT",
  "htmlwidgets",
  "magick"
)

pkg_attach2(libraries_basic)
pkg_attach2(libraries_tidyverse)
pkg_attach2(libraries_sources)
pkg_attach2(libraries_utilities)
pkg_attach2(libraries_reports)


conflict_prefer("filter", "dplyr")

