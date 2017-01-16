#' Crea un proyecto fisabior
#'
#' Crea automáticamente un proyecto siguiendo la plantilla fisabior, empleando
#' una jerarquía de directorios unificada, cargando archivos y funciones comunes
#' del grupo de colaboradores. Opcionalmente, genera un repositorio git.
#'
#' @export
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal del proyecto. Por defecto se escoge el directorio actual de
#'   trabajo, devuelto por la función getwd().
#' @param autor Cadena de caracteres con el nombre del autor del informe. Por
#'   defecto asigna el nombre del usuario del sistema operativo.
#' @param titulo_proj Cadena de caracteres con el título del informe. Por
#'   defecto se asigna 'Análisis del proyecto X', donde X es el nombre del
#'   directorio principal del proyecto.
#' @param titulo_doc Cadena de caracteres con el nombre del archivo '.Rnw'. Por
#'   defecto se asigna el nombre 'informe_latex.Rnw'.
#' @return Cada una de estas funciones crea un documento y lo abre en el panel
#'   de documentos para su edición.
#' @examples
#' init_proj(autor = 'Carlos Vergara-Hernández',
#'           titulo_proj = 'Ejemplo de Informe Estadístico',
#'           titulo_doc = 'prueba_informe')


init_latex <- function(proj_dir = getwd(), autor = NULL, titulo_proj = NULL, titulo_doc = NULL) {
  if (is.null(titulo_proj)) titulo_proj <- paste('Análisis del proyecto', basename(getwd()))
  if (is.null(autor)) autor <- Sys.getenv('USER')
  if (is.null(titulo_doc)) titulo_doc <- 'informe_latex'

  template_path <- system.file('templates/template.Rnw', package = 'fisabior', mustWork = TRUE)
  template_out <- paste(readLines(template_path), collapse = '\n')
  template_out <- sub('nombre_autor', author, template_out)
  template_out <- sub('titulo_proj', titulo_proj, template_out)
  writeLines(template_out, paste0(proj_dir, 'informes/latex/', titulo_doc, '.Rnw'))
  file.edit(paste0(proj_dir, 'informes/latex/', titulo_doc, '.Rnw'))
}
