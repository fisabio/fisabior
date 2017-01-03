#' Crea un proyecto con una jerarquía de directorios unificada, cargando
#' archivos y funciones comunes.
#'
#' @export
#' @param proj_nom Cadena de caracteres con el nombre a asignar al proyecto. Por
#'   defecto se asigna el nombre 'proyecto-yyy-mm-dd'.
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal donde se creará el proyecto. Por defecto se escoge el directorio
#'   actual de trabajo, devuelto por la función getwd().
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
#' @examples
#' proj_dir()
#' proj_dir(proj_nom = 'proyecto_europeo_X', proj_dir = '~/Proyectos')
init_proj <- function(proj_nom = NULL, proj_dir = NULL) {
  if (length(dir(getwd())) != 0 & length(proj_dir) == 0) {
    cat('El directo de trabajo actual no está vacío y no has indicado un directorio para el proyecto.\n',
    		'¿Quieres continuar a pesar de esto? \n 1:si \n 2:no \n')
  	acto <- readline()
  	if (acto == '2') {
  		stop('Por favor, proporciona la ruta al directorio que albergará el proyecto.')
  	}
  }
  if (is.null(proj_dir)) proj_dir <- getwd()
  if (is.null(proj_nom)) {
  	cat('No has proporcionado un nombre para el proyecto.\n',
  			'¿Quieres asigne uno por defecto en el directorio de trabajo actual? \n 1:si \n 2:no \n')
    acto <- readline()
    if (acto == '2') {
      stop('Por favor, proporciona un nombre para el proyecto en la llamada a la función.')
    } else {
      proj_dir <- paste(proj_dir, '/proyecto-', format(Sys.time(), '%Y-%m-%d'), sep = '')
      dir.create(proj_dir)
    }
  } else {
  	proj_dir <- paste(proj_dir, '/', proj_nom, sep = '')
  	dir.create(proj_dir)
  }
  dirs <- paste(
    proj_dir, '/',
    c('datos/brutos', 'datos/procesados', 'figuras', 'informes/markdown', 'informes/docx',
      'informes/odt', 'informes/latex', 'cache', 'src/bugs', 'src/cpp', 'src/jags', 'src/stan',
      'configuracion', 'r',  'articulo/enviado', 'articulo/revision', 'articulo/proof'),
    sep = '')
  invisible(sapply(dirs, dir.create, recursive = T))
  template_path <- system.file('templates', template, package = 'fisabioR', mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template_path))
  writeLines(template_out, path)
}
