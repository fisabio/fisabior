#' Crea un informe estadístico (estático o dinámico) empleando una plantilla
#'
#' Crea automáticamente un informe estadístico empleando una plantilla base con
#' detalles del grupo. El documento generado puede tener diversos doc_formats
#' (PDF desde Markdown o desde LaTeX, y HTML, DOCX y ODT desde Markdown).
#'
#' @export
#' @param doc_format Cadena de caracteres con el doc_format deseado para el
#'   informe. Las opciones admitidas son pdf_markdown, latex, html, docx y odt.
#'   Por defecto se selecciona pdf_markdown.
#' @param title Cadena de caracteres con el título del informe. Por defecto se
#'   asigna \emph{Informe estadístico del proyecto \code{X}}, donde \code{X} es
#'   el nombre del directorio principal del proyecto.
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico se asigna
#'   el nombre \code{informe_form}, donde "form" (doc_format) varía en función
#'   de lo seleccionado en el argumento doc_format (.Rmd, .Rnw, .pdf, .html,
#'   .odt o .docx).
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
#'   de informes correspondiente al formato --informes/pdf_markdown/--, otros en
#'   los directorios de data/cache o figuras/formato).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#' informe(doc_format = "pdf_markdown",
#'         title      = "Informe Estadístico en FISABIO",
#'         file_name  = "informe_fisabio_md")
#' informe(doc_format = "latex",
#'         title      = "Informe Estadístico en FISABIO",
#'         file_name  = "informe_fisabio_tex")
#' informe(doc_format = "odt",
#'         title      = "Informe Estadístico en FISABIO",
#'         file_name  = "informe_fisabio_odt")
#' informe(doc_format = "docx",
#'         title      = "Informe Estadístico en FISABIO",
#'         file_name  = "informe_fisabio_docx")
#' informe(doc_format = "html",
#'         title      = "Informe Estadístico en FISABIO",
#'         file_name  = "informe_fisabio_html")
#' informe(doc_format = "beamer",
#'         title      = "Presentación Estadística en FISABIO",
#'         file_name  = "presentacion_fisabio_beamer")
#' }
informe <- function(
  doc_format = "pdf_markdown",
  title      = NULL,
  file_name  = NULL) {

  ######################################
  # Comprobaciones iniciales           #
  ######################################
  proj_dir <- getwd()
  proj_files <- list.files(proj_dir)
  if (!any(grepl(".Rproj", proj_files)))
    stop("\nEl directorio de trabajo no contiene ningún proyecto de RStudio.",
         "\nCambia al directorio principal creado con la función fisabior::init_proj()")
  if (is.null(title))
    title <- paste("Informe Estadístico del Proyecto", basename(getwd()))
  if (is.null(file_name))
    file_name <- paste0("informe_", doc_format)
  if (!grepl("/$", proj_dir))
    proj_dir <- paste0(proj_dir, "/")
  proj_opt <- readLines(proj_files[grep(".Rproj", proj_files)])
  doc_format <- tolower(doc_format)
  if (!any(grepl(doc_format, c("pdf_markdown", "latex", "html", "docx", "odt", "beamer"))))
    stop("\nEl formato que has escogido no está disponible o es erróneo.",
         "\nLos posibles formatos son: pdf_markdown, latex, html, docx, odt o beamer.")

  ######################################
  # Documentos en PDF                  #
  ######################################
  if (doc_format %in% c("pdf_markdown", "latex", "beamer")) {
    if (!any(grepl("knitr", proj_opt, ignore.case = TRUE))) {
      stop("\nknitr no es la opción por defecto para compilar archivos LaTeX",
           "\nVuelve a ejecutar la función cuando lo hayas cambiado en:\n",
           "Tools/Project Options/Sweave/Program Defaults")
    } else if (!any(grepl("xelatex", proj_opt, ignore.case = TRUE))) {
      stop("\nXeLaTeX no es la opción por defecto para compilar LaTeX.",
           "\nVuelve a ejecutar la función cuando lo hayas cambiado en:\n",
           "Tools/Project Options/Sweave/Program Defaults")
    }
    if (doc_format == "pdf_markdown") {
      if (!dir.exists("informes/pdf_markdown")) dir.create("informes/pdf_markdown", recursive = T)
      report_path <- paste0("informes/pdf_markdown/", file_name, ".Rmd")
      rmarkdown::draft(file = report_path, create_dir = FALSE, template = "pdf_markdown",
                       package = "fisabior", edit = TRUE)
      pdf_draft <- readLines(report_path)
      pdf_draft[grep("^title:", pdf_draft)] <- paste("title:", title)
      writeLines(paste(pdf_draft, collapse = "\n"), report_path)
      copy_fisabior(from_ = "templates/referencias_prueba.bib",
                    to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
    } else if (doc_format == "latex") {
      if (!dir.exists("informes/latex")) dir.create("informes/latex", recursive = T)
      report_path <- paste0("informes/latex/", file_name, ".Rnw")
      rnw_path <- system.file("templates/template.Rnw", package = "fisabior", mustWork = TRUE)
      rnw_out <- paste(readLines(rnw_path))
      rnw_out[grep("^\\\\title\\{", rnw_out)] <- paste0("\\title{", title, "}")
      writeLines(paste(rnw_out), report_path)
      copy_fisabior(from_ = "templates/referencias_prueba.bib",
                    to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
    } else if (doc_format == "beamer") {
      if (!dir.exists("informes/beamer")) dir.create("informes/beamer", recursive = T)
      report_path <- paste0("informes/beamer/", file_name, ".Rmd")
      rmarkdown::draft(file = report_path, create_dir = FALSE, template = "beamer",
                       package = "fisabior", edit = TRUE)
      pdf_draft <- readLines(report_path)
      pdf_draft[grep("^title:", pdf_draft)] <- paste("title:", title)
      writeLines(paste(pdf_draft, collapse = "\n"), report_path)
      copy_fisabior(from_ = "templates/referencias_prueba.bib",
                    to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
    }
  } else if (doc_format == "docx") {

    ######################################
    # Documentos en DOCX                 #
    ######################################
    if (!dir.exists("informes/docx")) dir.create("informes/docx", recursive = T)
    report_path <- paste0("informes/docx/", file_name, ".Rmd")
    rmarkdown::draft(file = report_path, create_dir = FALSE, template = "docx",
                     package = "fisabior", edit = TRUE)
    docx_draft <- readLines(report_path)
    docx_draft[grep("^title:", docx_draft)] <- paste("title:", title)
    writeLines(paste(docx_draft, collapse = "\n"), report_path)
    copy_fisabior(from_ = "templates/referencias_prueba.bib",
                  to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
  } else if (doc_format == "odt") {

    ######################################
    # Documentos en ODT                  #
    ######################################
    if (!dir.exists("informes/odt")) dir.create("informes/odt", recursive = T)
    report_path <- paste0("informes/odt/", file_name, ".Rmd")
    rmarkdown::draft(file = report_path, create_dir = FALSE, template = "odt",
                     package = "fisabior", edit = TRUE)
    odt_draft <- readLines(report_path)
    odt_draft[grep("^title:", odt_draft)] <- paste("title:", title)
    writeLines(paste(odt_draft, collapse = "\n"), report_path)
    copy_fisabior(from_ = "templates/referencias_prueba.bib",
                  to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
  } else if (doc_format == "html") {

    ######################################
    # Documentos en HTML                 #
    ######################################
    if (!dir.exists("informes/html")) dir.create("informes/html", recursive = T)
    report_path <- paste0("informes/html/", file_name, ".Rmd")
    rmarkdown::draft(file = report_path, create_dir = FALSE, template = "html",
                     package = "fisabior", edit = TRUE)
    html_draft <- readLines(report_path)
    html_draft[grep("^title:", html_draft)] <- paste("title:", title)
    writeLines(paste(html_draft, collapse = "\n"), report_path)
    copy_fisabior(from_ = "templates/referencias_prueba.bib",
                  to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
  }
}


#' Función que devuelve el formato de salida apropiado para generar el informe
#' o la presentación en PDF.
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un PDF siguiendo el formato de la plantilla de fisabior. Solo tiene un
#' argumento, el cual especifica si se trata de una presentación o no.
#'
#' @export
#' @param beamer Lógico: ¿el documento es una presentación Beamer? Falso por
#'   defecto.
#'
#' @details
#' La función solo debe utilizarse desde una llamada a rmarkdown::render()
#' o dentro de un documento .Rmd
#'
#' @return Objeto con clase "rmarkdown_output_format".
#'
informe_pdf <- function(beamer = FALSE) {
  if (!beamer) {
    template <- system.file("rmarkdown/templates/pdf_markdown/resources/template.tex",
                            package = "fisabior")
    doc_format <- rmarkdown::pdf_document(
      template         = template,
      dev              = "tikz",
      latex_engine     = "xelatex",
      citation_package = "biblatex",
      fig_caption      = TRUE,
      md_extensions    = "-autolink_bare_uris",
      keep_tex         = TRUE)
  } else {
    template <- system.file("rmarkdown/templates/beamer/resources/template.tex",
                            package = "fisabior")
    doc_format <- rmarkdown::beamer_presentation(
      template         = template,
      dev              = "tikz",
      latex_engine     = "xelatex",
      citation_package = "biblatex",
      fig_caption      = TRUE,
      md_extensions    = "-autolink_bare_uris",
      keep_tex         = TRUE)
  }
  doc_format$inherits <- "beamer_presentation"
  doc_format
}
