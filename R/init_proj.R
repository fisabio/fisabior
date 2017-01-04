#' Crea un proyecto con una jerarquía de directorios unificada, cargando
#' archivos y funciones comunes.
#'
#' @export
#' @param proj_nom Cadena de caracteres con el nombre a asignar al proyecto. Por
#'   defecto se asigna el nombre 'proyecto-yyy-mm-dd'.
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal donde se creará el proyecto. Por defecto se escoge el directorio
#'   actual de trabajo, devuelto por la función getwd().
#' @param git Valor lógico (TRUE o FALSE), indicando si se desea generar un
#'   repositorio git asociado al proyecto.
#' @details El valor asignado por defecto al argumento git (TRUE) hace que la
#'   función trate de encontrar si git se ha instalado en los lugares habituales.
#' @return La función crea la siguiente estructura de directorios en proj_dir/proj_nom/:
#' \itemize{
#'   \item datos/
#'     \itemize{
#'       \item brutos/
#'       \item procesados/
#'   }
#'   \item figuras/
#'   \item informes/
#'   \itemize{
#'     \item docx/
#'     \item latex/
#'     \item markdown/
#'     \item odt/
#'   }
#'   \item cache/
#'   \item src/
#'   \itemize{
#'     \item bugs/
#'     \item cpp/
#'     \item jags/
#'     \item stan/
#'   }
#'   \item configuracion/
#'   \item r/
#'   \item articulo/
#'   \itemize{
#'     \item enviado/
#'     \item revision/
#'     \item proof/
#'   }
#' }
#' Asimismo, también se crea un archivo en formato proyecto de RStudio, un
#'   documento R Markdown y Markdown con información genérica del mismo, y un
#'   script de configuración a modo de prueba.
#' @examples
#' proj_dir()
#' proj_dir(proj_nom = 'proyecto_europeo_X', proj_dir = '~/Proyectos')
init_proj <- function(proj_nom = NULL, proj_dir = NULL, git = TRUE) {
  if (is.null(proj_dir))
    stop('Por favor, indica el directorio que albergará el proyecto (proj_dir).')
  if (!dir.exists(proj_dir)) {
    cat('El directorio de proyecto proporcionado no existe.\n',
        '¿Quieres crearlo ahora? \n 1:si \n 2:no \n')
    acto <- readline()
    if (acto != '1') {
      stop('Por favor, proporciona un directorio alternativo para el proyecto.')
    } else {
      dir_info <- file.info(dirname(proj_dir))
      if (all(is.na.data.frame(dir_info)))
        stop('Revisa el directorio de proyecto que has proporcionado. ',
             'Quizá el problema sea que no existe el directorio inmediatamente superior ',
             'o alguno intermediario...')
      if (!as.numeric(substr(dir_info$mode, 1, 1)) > 5 || Sys.info()['user'] != dir_info$uname) {
        stop('No tienes permisos suficientes para crear ese directorio. Prueba con otro...')
      }
      dir.create(proj_dir, recursive = T)
      proj_nom <- paste('proyecto-', format(Sys.time(), '%Y-%m-%d'), sep = '')
    }
  } else {
    dir_info <- file.info(proj_dir)
    if (!as.numeric(substr(dir_info$mode, 1, 1)) > 5 || Sys.info()['user'] != dir_info$uname) {
      stop('No tienes permisos suficientes para crear ese directorio. Prueba con otro...')
    }
  }
  if (is.null(proj_nom)) {
  	cat('No has proporcionado un nombre para el proyecto.\n',
  			'¿Quieres asignar uno por defecto en el directorio de trabajo actual? \n 1:si \n 2:no \n')
    acto <- readline()
    if (acto != '1') {
      stop('Por favor, proporciona un nombre para el proyecto (proj_nom).')
    } else {
      proj_nom <- paste('proyecto-', format(Sys.time(), '%Y-%m-%d'), sep = '')
    }
  }

  if (!grepl('/$', proj_dir)) proj_dir <- paste(proj_dir, '/', sep = '')
  proj_dir <- paste(proj_dir, proj_nom, sep = '')
  if (dir.exists(proj_dir))
    stop('¡El directorio que iba a crearse ya existe! Marca una ruta o nombre diferente.')
  dir.create(proj_dir)

  sub_dirs <- paste(
    proj_dir, '/',
    c('datos/brutos', 'datos/procesados', 'figuras', 'informes/markdown', 'informes/docx',
      'informes/odt', 'informes/latex', 'cache', 'src/bugs', 'src/cpp', 'src/jags', 'src/stan',
      'configuracion', 'r',  'articulo/enviado', 'articulo/revision', 'articulo/proof'),
    sep = '')
  invisible(sapply(sub_dirs, dir.create, recursive = T))

  template_path <- system.file('templates/template.Rproj', package = 'fisabioR', mustWork = TRUE)
  template_out <- paste(readLines(template_path), collapse = '\n')
  config_path <- system.file('templates/config.R', package = 'fisabioR', mustWork = TRUE)
  config_out <- paste(readLines(config_path), collapse = '\n')
  readme_path <- system.file('templates/README.Rmd', package = 'fisabioR', mustWork = TRUE)
  readme_out <- paste(readLines(readme_path), collapse = '\n')
  writeLines(template_out, paste(proj_dir, '/', proj_nom, '.Rproj', sep = ''))
  writeLines(config_out, paste(proj_dir, '/configuracion/config.R', sep = ''))
  writeLines(readme_out, paste(proj_dir, '/README.Rmd', sep = ''))
  knitr::knit(paste(proj_dir, '/README.Rmd', sep = ''),
              paste(proj_dir, '/README.md', sep = ''), quiet = T)

  files <- paste('/r/', c('importar_datos.R', 'depurar_datos.R',
                          'descriptiva.R', 'analisis.R'), sep = '')
  invisible(sapply(files, function(x) {
    writeLines('\nsource(configuracion/config.R)', paste(proj_dir, x, sep = ''))
  }))

  if (!file.exists(Sys.which('git')) && git == TRUE)
    warning('No tienes instalado git, así que no puedo crear el repositorio.')
  if (git == TRUE && git2r::in_repository(proj_dir) )
    warning('El exite el repositorio git: no se hace nada.')
  if (git == TRUE) {
    repo <- git2r::init(proj_dir)
    git2r::add(repo = repo, path = '*')
    invisible(git2r::commit(repo = repo, message = 'Primer commit', all = TRUE))
  } else message('Proyecto configurado sin git: puedes emplearlo más adelante.')
}
