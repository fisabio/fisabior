#' Crea un informe estadístico desde una plantilla
#'
#' Crea automáticamente un informe estadístico empleando una plantilla base con
#' detalles del grupo, ofreciendo la posibilidad de que el documento generado
#' tenga diversos formatos (LaTeX, RMarkdown, DOCX, ODT).
#'
#' @export
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal del proyecto. Por defecto, y dado que se supone que se está
#'   trabajando desde el directorio principal del proyecto, se escoge el
#'   directorio actual de trabajo, devuelto por la función getwd().
#' @param autor Cadena de caracteres con el nombre del autor del informe. Por
#'   defecto asigna el nombre del usuario del sistema operativo.
#' @param formato Cadena de caracteres con el formato deseado para el informe.
#'   Las opciones admitidas son xelatex, pdflatex, rmarkdown, docx y odt. Por
#'   defecto se selecciona xelatex.
#' @param titulo_proj Cadena de caracteres con el título del informe. Por
#'   defecto se asigna 'Análisis del proyecto X', donde X es el nombre del
#'   directorio principal del proyecto.
#' @param titulo_doc Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto se asigna el nombre 'informe_form.ext', donde 'form'
#'   (formato) y 'ext' (extensión) varían en función de lo seleccionado en en el
#'   argumento formato.
#'
#' @details La función solo funciona si en el directorio de trabajo existe un
#'   proyecto de RStudio, es decir, un archivo '.Rproj'.
#'
#'   Esta función requiere tener operativo el paquete knitr y, si se desea
#'   generar informes PDF directamente, también es necesario tener una
#'   distribución LaTeX. En princio la opción por defecto es un documento LaTeX
#'   compilado con XeLaTeX (en lugar de pdfLaTeX, ya que el primero ofrece una
#'   mayor compatibilidad de fuentes), de forma que la función comprueba en
#'   primer lugar que el proyecto tenga declaradas estas opciones (vienen por
#'   defecto al emplear init_proj). Si no hubiera un acuerdo entre lo esperado y
#'   lo declarado en las opciones del proyecto, la función pregunta qué hacer
#'   para que, o bien se cambien las opciones del proyecto o se escoja un
#'   formato acorde a ellas.
#'
#'   A la hora de definir múltiples autores, lo apropiado es crear un documento
#'   inicial con un único autor y, una vez generado, añadir los autores a ese
#'   documento.
#'
#' @return Cada una de estas funciones crea un documento y lo abre en el panel
#'   de documentos para su edición.
#'
#' @examples
#' init_proj(autor       = 'Carlos Vergara-Hernández',
#'           formato     = 'xelatex',
#'           titulo_proj = 'Informe Estadístico de Prueba',
#'           titulo_doc  = 'informe_prueba')


init_latex <- function(
  autor       = Sys.getenv('USER'),
  formato     = 'xelatex',
  titulo_proj = 'Análisis del proyecto ',
  titulo_doc  = 'informe_') {

  proj_dir <- getwd()
  proj_files <- list.files(proj_dir)
  if (!any(grepl('.Rproj', proj_files)))
    stop('\nEl directorio de trabajo no contiene ningún proyecto de RStudio.',
         '\nCambia al directorio principal creado con la función "init_proj()"')

  proj_opt <- readLines(proj_files[grep('.Rproj', proj_files)])
  formato <- tolower(formato)
  if (!any(grepl(formato, c('xelatex', 'rmarkdown', 'docx', 'odt'))))
    stop('\nEl formato que has escogido no está disponible o es erróneo.',
         '\nLos posibles formatos son: xelatex, rmarkdown, docx u odt.')

  if (formato == 'xelatex') {
    if (!any(grepl('knitr', proj_opt, ignore.case = T))) {
      cat('\nknitr no es la opción por defecto para compilar archivos\n',
          'Sweave (archivos con extensión .Rnw, LaTeX). ¿Qué quieres hacer?\n',
          ' 1: cambiar esa a knitr ahora\n 2: parar y cambiarlo a mano\n', sep = '')
      acto <- readline()
      if (acto != '1') {
        stop('\nDe acuerdo, vuelve a ejecutar la función cuando lo hayas cambiado.')
      } else {
        proj_opt[grep('rnwweave', proj_opt, ignore.case = T)] <- 'RnwWeave: knitr'
        writeLines(paste(proj_opt, collapse = '\n'), proj_files[grep('.Rproj', proj_files)])
      }


    }
      stop('\nknitr no es la opción por defecto para compilar archivos\n',
           'Sweave (extensión .Rnw, LaTeX). ¿Qué quieres hacer?',
           ' 1: cambiar a XeLaTeX\n 2: continuar con pdfLaTeX \n', sep = '')

           '\nPor favor, cambia esto en: Tools/Project Options/Sweave/Program Defaults'
    proj_tex <- sub('latex: ', '', proj_opt[grep('latex', proj_opt)])




    if (proj_tex != 'xelatex') {



      cat('Aunque es preferible usar XeLaTeX para una mayor compatibilidad de\n',
          'fuentes, no es la opción por defecto del proyecto. ¿Qué quieres hacer?\n',
          ' 1: cambiar a XeLaTeX\n 2: continuar con pdfLaTeX \n', sep = '')



      acto <- readline()
      if (acto != '1') {
        stop('Por favor, proporciona un directorio alternativo para el proyecto.')
      }
    }



    if (!grepl(formato, proj_tex)) {
      cat('El formato escogido (', formato, ') y el declarado en el proyecto (',
          proj_tex, ') no coinciden.\n',
          'Sabiendo que es preferible XeLaTeX... ¿qué quieres hacer?\n',
          '1:cambiar  \n 2:no \n', sep = '')
      acto <- readline()
      if (acto != '1') {
        stop('Por favor, proporciona un directorio alternativo para el proyecto.')
      }
    }
  }




  if (is.null(titulo_proj)) titulo_proj <- paste('Análisis del proyecto', basename(getwd()))
  if (is.null(autor)) autor <- Sys.getenv('USER')
  if (is.null(titulo_doc)) titulo_doc <- 'informe_latex'



  if (!grepl('/$', proj_dir)) proj_dir <- paste0(proj_dir, '/')





  template_path <- system.file('templates/template.Rnw', package = 'fisabior', mustWork = TRUE)
  template_out <- paste(readLines(template_path), collapse = '\n')
  template_out <- sub('nombre_autor', autor, template_out)
  template_out <- sub('titulo_proj', titulo_proj, template_out)
  writeLines(template_out, paste0(proj_dir, 'informes/latex/', titulo_doc, '.Rnw'))
  file.edit(paste0(proj_dir, 'informes/latex/', titulo_doc, '.Rnw'))
}
