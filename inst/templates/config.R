
# Configuración de proxy (FISABIO) ----------------------------------------
httr::set_config(httr::use_proxy(
  url      = "193.145.201.49",
  port     = 8080,
  username = "invitado",
  password = "invitado"
))


# Carga de paquetes -------------------------------------------------------
pkgs <- c(
  "knitr",
  "rmarkdown",
  "tufte",
  "fisabior"
)
pkgs <- sapply(pkgs, function(x)
  suppressPackageStartupMessages(require(x, character.only = T))
)


# Funciones ---------------------------------------------------------------
## Bloques de alerta en presentaciones HTML
bloque <- function(texto, tipo = "alerta") {
  stopifnot(is.character(texto))
  if (tipo == "alerta") {
    cat(paste('<div class="alerta">',
              '<i class="fa fa-exclamation-triangle fa-1x"',
              'style="color:#f44336" aria-hidden="true"></i>',
              texto, '</div>'), sep = "  ")
  } else if (tipo == "exito") {
    cat(paste('<div class="alerta exito">',
              '<i class="fa fa-check fa-1x',
              'text-danger" style="color:#4CAF50" aria-hidden="true"></i>',
              texto, '</div>'), sep = "  ")
  } else if (tipo == "info") {
    cat(paste('<div class="alerta info">',
              '<i class="fa fa-info fa-1x"',
              'style="color:#2196F3" aria-hidden="true"></i>',
              texto, '</div>'), sep = "  ")
  }
}
