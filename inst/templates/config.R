
# Configuración de proxy (FISABIO) ----------------------------------------
httr::set_config(httr::use_proxy(
  url      = "193.145.201.49",
  port     = 8080,
  username = "invitado",
  password = "invitado"
))


# Carga de paquetes -------------------------------------------------------
pkgs <- c(
  "fisabior"
)
sapply(pkgs, function(x)
  suppressPackageStartupMessages(require(x, character.only = T))
)
rm(pkgs)
