
# Carga de paquetes -------------------------------------------------------
pkgs <- c(
  "data.table",
  "knitr",
  "maptools",
  "mlr",
  "R2WinBUGS",
  "RColorBrewer",
  "rgdal",
  "rmarkdown",
  "rms",
  "sp",
  "tidyverse",
  "tufte",
  "xtable",
  "fisabior"
)
invisible(sapply(pkgs, function(x) {
  if (!x %in% rownames(installed.packages()))
    install.packages(x)
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}))
rm(pkgs)


# Configuración de proxy (FISABIO) ----------------------------------------
httr::set_config(httr::use_proxy(
  url      = "193.145.201.49",
  port     = 8080,
  username = "invitado",
  password = "invitado"
))
