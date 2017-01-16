#' Crea un proyecto fisabior
#'
#' Crea automáticamente un proyecto siguiendo la plantilla fisabior, empleando
#' una jerarquía de directorios unificada, cargando archivos y funciones comunes
#' del grupo de colaboradores. Opcionalmente, genera un repositorio git.
#'
#' @export
#' @param proj_nom Cadena de caracteres con el nombre a asignar al proyecto. Por
#'   defecto se asigna el nombre 'proyecto-yyy-mm-dd'.
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal donde se creará el proyecto. Por defecto se escoge el directorio
#'   actual de trabajo, devuelto por la función getwd().
#' @param git Valor lógico (TRUE o FALSE), indicando si se desea generar un
#'   repositorio git asociado al proyecto.
#' @details La función crea la estructura de directorios a partir del nombre del
#'   proyecto empleando minúsculas, con independencia de si el usuario ha
#'   introducido el argumento con mayúsculas: P. ej., proj_name = 'Proyecto_X'
#'   siempre será reconocido por la función como proj_name = 'proyecto_x'.
#' @return La función crea la siguiente estructura de directorios en
#'   proj_dir/proj_nom/:
#' \itemize{
#'   \item datos/
#'     \itemize{
#'       \item brutos/
#'       \item procesados/
#'       \item shapefiles/
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
#' @examples
#' init_proj()
#' init_proj(proj_nom = 'proyecto_europeo_X', proj_dir = '~/proyectos')


init_proj <- function(proj_nom = NULL, proj_dir = NULL, git = TRUE) {
  if (is.null(proj_dir))
    stop('Por favor, indica el directorio que albergará el proyecto (proj_dir).')
  dir_info <- sapply(c(0, 1:2, 4), function(x)
    file.access(names = dirname(proj_dir), mode = x))
  if (!dir.exists(proj_dir)) {
    cat('El directorio de proyecto proporcionado no existe.\n',
        '¿Quieres crearlo ahora? \n 1:si \n 2:no \n')
    acto <- readline()
    if (acto != '1') {
      stop('Por favor, proporciona un directorio alternativo para el proyecto.')
    } else {
      if (dir_info[1] != 0)
        stop('Revisa el directorio de proyecto que has proporcionado. ',
             'Quizá el problema sea que no existe el directorio inmediatamente superior ',
             'o alguno intermediario...')
      if (any(dir_info[-1] != 0)) {
        stop('No tienes permisos de ejecución, escritura o lectura en ese ',
             'directorio. Prueba con otro...')
      }
      dir.create(proj_dir, recursive = T)
    }
  } else {
    if (any(dir_info[-1] != 0))
      stop('Revisa el directorio de proyecto que has proporcionado. ',
           'Quizá el problema sea que no existe el directorio inmediatamente superior ',
           'o alguno intermediario...')
  }
  if (is.null(proj_nom)) {
    cat('No has proporcionado un nombre para el proyecto.\n',
        '¿Quieres asignar uno por defecto en el directorio de trabajo actual? \n 1:si \n 2:no \n')
    acto <- readline()
    if (acto != '1') {
      stop('Por favor, proporciona un nombre para el proyecto (proj_nom).')
    } else {
      proj_nom <- paste0('proyecto-', format(Sys.time(), '%Y-%m-%d'))
    }
  }
  proj_nom <- tolower(proj_nom)
  if (!grepl('/$', proj_dir)) proj_dir <- paste0(proj_dir, '/')
  proj_dir <- paste0(proj_dir, proj_nom)
  if (!grepl('/$', proj_dir)) proj_dir <- paste0(proj_dir, '/')
  if (dir.exists(proj_dir))
    stop('¡El directorio que iba a crearse ya existe! Marca una ruta o nombre diferente.')
  dir.create(proj_dir)

  sub_dirs <- paste0(
    proj_dir,
    c(paste0('datos/', c('brutos', 'procesados', 'shapefiles')), 'figuras',
      paste0('informes/', c('markdown', 'docx', 'odt', 'latex')), 'cache',
      paste0('src/', c('bugs', 'cpp', 'jags', 'stan')),
      'configuracion', 'r',  paste0('articulo/', c('enviado', 'revision', 'proof')))
  )
  invisible(sapply(sub_dirs, dir.create, recursive = T))

  template_path <- system.file('templates/template.Rproj', package = 'fisabior', mustWork = TRUE)
  template_out <- paste(readLines(template_path), collapse = '\n')
  config_path <- system.file('templates/config.R', package = 'fisabior', mustWork = TRUE)
  config_out <- paste(readLines(config_path), collapse = '\n')
  readme_path <- system.file('templates/README.Rmd', package = 'fisabior', mustWork = TRUE)
  readme_out <- paste(readLines(readme_path), collapse = '\n')
  writeLines(template_out, paste0(proj_dir, proj_nom, '.Rproj'))
  writeLines(config_out, paste0(proj_dir, 'configuracion/config.R'))
  writeLines(readme_out, paste0(proj_dir, 'README.Rmd'))
  knitr::knit(paste0(proj_dir, 'README.Rmd'),
              paste0(proj_dir, 'README.md'), quiet = T)

  sample_scripts <- paste0('r/', c('importar_datos.R', 'depurar_datos.R',
                           'descriptiva.R', 'analisis.R'))
  invisible(sapply(sample_scripts, function(x) {
    writeLines('\nsource(configuracion/config.R)', paste0(proj_dir, x))
  }))
  sample_data <- paste0('templates/',
                        paste0('ejemplo_aragon',
                               c('.shx', '.shp', '.prj', '.dbf')))
  invisible(sapply(sample_data, function(x) {
    file.copy(system.file(x, package = 'fisabior', mustWork = TRUE),
              sub('templates/', paste0(proj_dir, 'datos/shapefiles/'), x))
  }))
  invisible(file.copy(system.file('templates/ejemplo_aragon.Rdata', package = 'fisabior', mustWork = T),
                      paste0(proj_dir, 'datos/procesados/ejemplo_aragon.Rdata')))
  if (!file.exists(Sys.which('git')) && git == TRUE) {
    warning('No tienes instalado git, así que no puedo crear el repositorio.')
  } else if (git == TRUE) {
    if (git2r::in_repository(proj_dir)) {
      warning('Ya exite el repositorio git: no se hace nada.')
    } else {
      repo <- git2r::init(proj_dir)
      git2r::add(repo = repo, path = '*')
      invisible(git2r::commit(repo = repo, message = 'Primer commit', all = TRUE))
    }
  } else message('Proyecto configurado sin git: puedes emplearlo más adelante.')
}
