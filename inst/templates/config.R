
# Carga de paquetes -------------------------------------------------------
# Parche provisional para Windows.
pkg_dep <- c(
  "knitr",
  "mapproj",
  "maptools",
  "R2WinBUGS",
  "rgdal",
  "rgeos",
  "rmarkdown",
  "sp",
  "tidyverse",
  "tufte",
  "xtable"
)
invisible(sapply(pkg_dep, function(x) {
  if (!suppressPackageStartupMessages(require(x, character.only = T)) )
    install.packages(x, dependencies = T)
  suppressPackageStartupMessages(require(x, character.only = T))
  }
))


pkgs <- c(
  "data.table",
  "devtools",
  "dplyr",
  "fisabior",
  "ggplot2",
  "gridExtra",
  "haven",
  "httr",
  "knitr",
  "latex2exp",
  "lme4",
  "mapproj",
  "maptools",
  "parallel",
  "R2WinBUGS",
  "RColorBrewer",
  "readr",
  "readxl",
  "rgdal",
  "rmarkdown",
  "sp",
  "spdep",
  "stringr",
  "tableone",
  "tidyr",
  "xtable"
)
pkgs <- sapply(pkgs, function(x) suppressPackageStartupMessages(require(x, character.only = T)))
if (length(pkgs[!pkgs]) != 0) cat("No he podido cargar los paquetes:",
                                  paste(names(pkgs[!pkgs]), collapse = ", "))


# Configuración de proxy (FISABIO) ----------------------------------------
set_config(use_proxy(
  url      = "193.145.201.49",
  port     = 8080,
  username = "invitado",
  password = "invitado"
))

rm(pkgs)
