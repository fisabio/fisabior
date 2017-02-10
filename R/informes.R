#' Crea un informe estadístico (estático o dinámico) empleando una plantilla
#'
#' Crea automáticamente un informe estadístico empleando una plantilla base con
#' detalles del grupo. El documento generado puede tener diversos formatos (PDF
#' desde Markdown o desde LaTeX, y HTML, DOCX y ODT desde Markdown).
#'
#' @export
#' @param formato Cadena de caracteres con el formato deseado para el informe.
#'   Las opciones admitidas son pdf-markdown, latex, html, docx y odt. Por
#'   defecto se selecciona pdf-markdown.
#' @param titulo_proj Cadena de caracteres con el título del informe. Por
#'   defecto se asigna \emph{Informe estadístico del proyecto \code{X}}, donde
#'   \code{X} es el nombre del directorio principal del proyecto.
#' @param titulo_doc Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico se asigna
#'   el nombre \code{informe_form}, donde 'form' (formato) varía en función de
#'   lo seleccionado en el argumento formato (.Rmd, .Rnw, .pdf, .html, .odt o
#'   .docx).
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea un
#'   \emph{*.Rproj} por defecto.
#'
#'   Esta función requiere tener operativo los paquetes \code{knitr} y
#'   \code{rmarkdown}. Si se desea generar informes PDF, también es necesario
#'   tener instalada una distribución LaTeX que incorpore (como mínimo) los
#'   siguientes paquetes: \code{adjustbox}, \code{amsmath}, \code{array},
#'   \code{authblk}, \code{biblatex}, \code{booktabs}, \code{caption},
#'   \code{csquotes}, \code{float}, \code{fontenc}, \code{fontspec},
#'   \code{footmisc}, \code{graphicx}, \code{hyperref}, \code{longtable},
#'   \code{lscape}, \code{mathtools}, \code{multirow}, \code{polyglossia},
#'   \code{rotating}, \code{setspace}, \code{tikz}, \code{xltxtra},
#'   \code{xunicode}.
#'
#'   La opción por defecto es un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr, pandoc y XeLaTeX (en lugar de pdfLaTeX, ya que el
#'   primero ofrece una mayor compatibilidad de fuentes para idiomas con
#'   caracteres especiales --caracteres no ASCII, como el nuestro--). Del mismo
#'   modo, la opción por defecto para gestionar la bibliografía es a través de
#'   \code{biblatex} empleando el gestor de referencias \code{biber}, de modo
#'   que este último debe estar instalado en el sistema.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si no
#'   hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return En función del formato escogido, se crea un documento principal y un
#'   conjunto de documentos de respaldo (algunos directamente en el directorio
#'   de informes correspondiente al formato --informes/pdf-markdown/--, otros en
#'   los directorios de data/cache o figuras/formato).
#'
#' @examples
#' informe(formato     = 'pdf-markdown',
#'         titulo_proj = 'Informe Estadístico en FISABIO',
#'         titulo_doc  = 'informe_fisabio')
#' informe(formato     = 'latex',
#'         titulo_proj = 'Informe Estadístico en FISABIO',
#'         titulo_doc  = 'informe_fisabio')
#' informe(formato     = 'odt',
#'         titulo_proj = 'Informe Estadístico en FISABIO',
#'         titulo_doc  = 'informe_fisabio')
#' informe(formato     = 'docx',
#'         titulo_proj = 'Informe Estadístico en FISABIO',
#'         titulo_doc  = 'informe_fisabio')
#' informe(formato     = 'html',
#'         titulo_proj = 'Informe Estadístico en FISABIO',
#'         titulo_doc  = 'informe_fisabio')
#'
informe <- function(
  formato     = 'pdf-markdown',
  titulo_proj = NULL,
  titulo_doc  = NULL) {
  proj_dir <- getwd()
  proj_files <- list.files(proj_dir)
  if (!any(grepl('.Rproj', proj_files)))
    stop('\nEl directorio de trabajo no contiene ningún proyecto de RStudio.',
         '\nCambia al directorio principal creado con la función fisabior::init_proj()')
  if (is.null(titulo_proj)) titulo_proj <- paste('Informe estadístico del proyecto',
                                                 basename(getwd()))
  if (is.null(titulo_doc)) titulo_doc <- paste0('informe_', formato)
  if (!grepl('/$', proj_dir)) proj_dir <- paste0(proj_dir, '/')
  proj_opt <- readLines(proj_files[grep('.Rproj', proj_files)])
  formato <- tolower(formato)
  if (!any(grepl(formato, c('pdf-markdown','latex', 'html', 'docx', 'odt'))))
    stop('\nEl formato que has escogido no está disponible o es erróneo.',
         '\nLos posibles formatos son: pdf-markdown, latex, html, docx u odt.')
  if (formato %in% c('pdf-markdown', 'latex')) {
    if (!any(grepl('knitr', proj_opt, ignore.case = T))) {
      cat('\nknitr no es la opción por defecto para compilar archivos\n',
          'Sweave (archivos con extensión .Rnw, LaTeX). ¿Qué quieres hacer?\n',
          ' 1: cambiar a knitr ahora\n 2: parar y cambiarlo a mano\n', sep = '')
      acto <- readline()
      if (acto != '1') {
        stop('\nDe acuerdo, vuelve a ejecutar la función cuando lo hayas cambiado en:\n',
             'Tools/Project Options/Sweave/Program Defaults')
      } else {
        proj_opt[grep('rnwweave', proj_opt, ignore.case = T)] <- 'RnwWeave: knitr'
        writeLines(paste(proj_opt, collapse = '\n'), proj_files[grep('.Rproj', proj_files)])
      }
    } else if (!any(grepl('xelatex', proj_opt, ignore.case = T))) {
      cat('\nXeLaTeX no es la opción por defecto para compilar LaTeX.\n',
          '¿Qué quieres hacer?\n',
          ' 1: cambiar a XeLaTeX ahora\n 2: parar y cambiarlo a mano\n', sep = '')
      acto <- readline()
      if (acto != '1') {
        stop('\nDe acuerdo, vuelve a ejecutar la función cuando lo hayas cambiado en:\n',
             'Tools/Project Options/Sweave/Program Defaults')
      } else {
        proj_opt[grep('rnwweave', proj_opt, ignore.case = T)] <- 'RnwWeave: knitr'
        writeLines(paste(proj_opt, collapse = '\n'), proj_files[grep('.Rproj', proj_files)])
      }
    }
    if (formato == 'pdf-markdown') {
      report_path <- paste0('informes/pdf-markdown/', titulo_doc, '.Rmd')
      rmarkdown::draft(file = report_path, create_dir = F, template = 'fisabior',
                       package = 'fisabior', edit = F)
      pdf_draft <- readLines(report_path)
      pdf_draft[grep('^title:', pdf_draft)] <- paste('title:', titulo_proj)
      writeLines(paste(pdf_draft, collapse = '\n'), report_path)
    } else {
      report_path <- paste0('informes/pdf-latex/', titulo_doc, '.Rnw')
      rnw_path <- system.file('templates/template.Rnw', package = 'fisabior', mustWork = T)
      rnw_out <- paste(readLines(rnw_path))
      rnw_out[grep('^\\\\title\\{', rnw_out)] <- paste0('\\title{', titulo_proj, '}')
      writeLines(paste(rnw_out), report_path)
      file.copy(system.file('rmarkdown/templates/fisabior/skeleton/referencias.bib',
                            package = 'fisabior', mustWork = T),
                paste0(dirname(report_path), '/referencias.bib'))
    }
  } else if (formato == 'docx') {
    report_path <- paste0('informes/docx/', titulo_doc, '.Rmd')
    rmd_path <- system.file('templates/template_docx.Rmd', package = 'fisabior', mustWork = T)
    rmd_out <- paste(readLines(rmd_path))
    rmd_out[grep('^title:', rmd_out)] <- paste('title:', titulo_proj)
    writeLines(paste(rmd_out), report_path)
    file.copy(system.file('rmarkdown/templates/fisabior/skeleton/referencias.bib',
                          package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/referencias.bib'))
    file.copy(system.file('templates/template_fisabior.docx', package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/template_fisabior.docx'))
  } else if (formato == 'odt') {
    report_path <- paste0('informes/odt/', titulo_doc, '.Rmd')
    rmd_path <- system.file('templates/template_odt.Rmd', package = 'fisabior', mustWork = T)
    rmd_out <- paste(readLines(rmd_path))
    rmd_out[grep('^title:', rmd_out)] <- paste('title:', titulo_proj)
    writeLines(paste(rmd_out), report_path)
    file.copy(system.file('rmarkdown/templates/fisabior/skeleton/referencias.bib',
                          package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/referencias.bib'))
    file.copy(system.file('templates/template_fisabior.odt', package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/template_fisabior.odt'))
    file.copy(system.file('templates/fisabior_odt.xml', package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/fisabior_odt.xml'))
  } else if (formato == 'html') {
    report_path <- paste0('informes/html/', titulo_doc, '.Rmd')
    rmd_path <- system.file('templates/template.html', package = 'fisabior', mustWork = T)
    rmd_out <- paste(readLines(rmd_path))
    rmd_out[grep('^title:', rmd_out)] <- paste('title:', titulo_proj)
    writeLines(paste(rmd_out), report_path)
    file.copy(system.file('rmarkdown/templates/fisabior/skeleton/referencias.bib',
                          package = 'fisabior', mustWork = T),
              paste0(dirname(report_path), '/referencias.bib'))
  }
  file.edit(report_path)
}


#' Función que devuelve el formato de salida apropiado para generar el informe
#' en PDF.
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un PDF fisabior, siguiendo el formato de la plantilla oficial.
#'
#' @export
#' @param keep_tex Lógico: ¿debe guardarse el archivo .tex?
#' @param md_extensions Cadena de caracteres indicando extensiones a markdown
#'   desde pandoc. Por defecto se elimina la compilación de URL's.
#'
#' @return Objeto con clase 'rmarkdown_output_format'.
#'
#' @examples
#' informe_pdf(keep_tex = FALSE,
#'             md_extensions = '-autolink_bare_uris')
informe_pdf <- function(keep_tex = TRUE, md_extensions = '-autolink_bare_uris') {
  template <- system.file('rmarkdown/templates/fisabior/resources/template.tex',
                          package = 'fisabior')
  formato <- rmarkdown::pdf_document(
    template         = template,
    dev              = 'tikz',
    latex_engine     = 'xelatex',
    citation_package = 'biblatex',
    fig_caption      = T,
    md_extensions    = md_extensions,
    keep_tex         = keep_tex)
  formato$inherits <- 'pdf_document'
  formato
}
