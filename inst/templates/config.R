
pkgs <- c('dplyr', 'data.table', 'ggplot2', 'haven', 'latex2exp', 'lme4', 'mapproj',
          'maptools', 'R2WinBUGS', 'readr', 'readxl', 'rgdal', 'sp', 'stringr',
          'tableone', 'tidyr', 'xtable', 'parallel', 'knitr', 'rmarkdown',
          'spdep', 'RColorBrewer', 'gridExtra')
pkgs <- sapply(pkgs, function(x) suppressPackageStartupMessages(require(x, character.only = T)))
if (length(pkgs[!pkgs]) != 0) cat('No he podido cargar los paquetes:',
                                  paste(names(pkgs[!pkgs]), collapse = ', '))
rm(pkgs)
