#' Comprobaciones para informes y presentaciones
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un informe o presentación siguiendo el formato de la plantilla de
#' fisabior.
#'
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe_form}, donde "form" varía en función del formato del informe:
#'   pdf, html, odt, docx, etc).
#' @param doc_format Cadena de caracteres con el formato del documento. Los
#'   formatos admitidos son pdf, latex, html, docx, odt, pres_html y beamer
#'
comprueba <- function(file_name = NULL, doc_format = NULL) {
  proj_dir    <- getwd()
  proj_files  <- list.files(proj_dir)
  if (!any(grepl(".Rproj", proj_files)))
    stop("\nEl directorio de trabajo no contiene ningún proyecto de RStudio.",
         "\nCambia al directorio principal creado con",
         " la función fisabior::init_proj()")
  if (is.null(file_name))
    file_name <- "informe"
  if (!grepl("/$", proj_dir))
    proj_dir  <- paste0(proj_dir, "/")
  proj_opt    <- readLines(proj_files[grep(".Rproj", proj_files)])
  doc_format  <- tolower(doc_format)
  if (!any(grepl(doc_format, c("pdf", "latex", "html", "docx", "odt",
                               "pres_html", "beamer", "rmd_beamer"))))
    stop("\nEl formato que has escogido no está disponible o es erróneo.",
         "\nLos posibles formatos son: pdf, latex, html, docx,",
         " odt, pres_html o beamer.")
  if (doc_format %in% c("pdf", "latex", "beamer")) {
    if (!any(grepl("knitr", proj_opt, ignore.case = TRUE))) {
      stop("\nknitr no es la opción por defecto para compilar archivos LaTeX",
           "\nVuelve a ejecutar la función cuando lo hayas cambiado en:\n",
           "Tools/Project Options/Sweave/Program Defaults")
    }
  }
}



#' Función que devuelve el formato de salida apropiado para generar el informe o
#' la presentación en PDF.
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un PDF siguiendo el formato de la plantilla de fisabior. Por defecto se
#' usa XeLaTeX como motor de LaTeX.
#'
#' @param ... Otros argumentos a pasar.
#' @export
#'
#' @details La función solo debe utilizarse desde una llamada a
#'   rmarkdown::render() o dentro de un documento .Rmd
#'
#' @return Objeto con clase "rmarkdown_output_format".
#'
informe <- function(beamer = FALSE, ...) {
  if (!beamer) {
    template <- system.file("rmarkdown/templates/pdf/resources/template.tex",
                            package = "fisabior")
    doc_format <- rmarkdown::pdf_document(
      template         = template,
      latex_engine     = "xelatex",
      citation_package = "none",
      fig_caption      = TRUE,
      md_extensions    = "-autolink_bare_uris",
      keep_tex         = TRUE,
      ...
    )
    doc_format$inherits <- "pdf_document"
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
      keep_tex         = TRUE,
      ...)
    doc_format$inherits <- "beamer_presentation"
  }
  return(doc_format)
}



#' Copia el archivo bib con las referencias de prueba y lo introduce en el
#' directorio donde se encuentra el informe.
#' @param path Ruta al documento.
copia_referencias <- function(path) {
  copy_fisabior(from_ = "templates/referencias_prueba.bib",
                to_   = paste0(dirname(path), "/referencias_prueba.bib"))
}



#' Copia el archivo csl para estilizar la bibliografía y lo introduce en el
#' directorio donde se encuentra el informe.
#' @param path Ruta al documento.
copia_csl <- function(path) {
  copy_fisabior(from_ = "templates/apa.csl",
                to_   = paste0(dirname(path), "/apa.csl"))
}

#' Copia imágenes y las introduce en el subdirectorio figuras se encuentra el
#' informe.
#' @param path Ruta al documento.
copia_imagenes <- function(path) {
  if (!dir.exists(paste0(dirname(path), '/figuras')))
    dir.create(paste0(dirname(path), '/figuras'), recursive = T)
  copy_fisabior(from_ = "templates/fisabior.png",
                to_   = paste0(dirname(path), "/figuras/fisabior.png"))
  copy_fisabior(from_ = "templates/logo-ccby.png",
                to_   = paste0(dirname(path), "/figuras/logo-ccby.png"))
}


#' Crea un informe estadístico en PDF -- Markdown
#'
#' Crea automáticamente un informe estadístico en PDF -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   Es necesario tener instalada una distribución LaTeX que incorpore (como
#'   mínimo) los siguientes paquetes: \code{adjustbox}, \code{amsmath},
#'   \code{amssymb}, \code{array}, \code{authblk}, \code{booktabs},
#'   \code{caption}, \code{ccicons}, \code{csquotes}, \code{float},
#'   \code{fontenc}, \code{fontspec}, \code{footmisc}, \code{graphicx},
#'   \code{hyperref}, \code{letltxmacro}, \code{longtable}, \code{lscape},
#'   \code{mathtools}, \code{microtype}, \code{multirow}, \code{parskip},
#'   \code{polyglossia}, \code{rotating}, \code{setspace}, \code{tabularx},
#'   \code{textpos}, \code{tikz}, \code{xltxtra}, \code{xunicode}.
#'
#'   El proceso genera un documento R Markdown que produce un PDF tras ser
#'   compilado con knitr, pandoc y XeLaTeX (en lugar de pdfLaTeX, ya que el
#'   primero ofrece una mayor compatibilidad de fuentes para idiomas con
#'   caracteres especiales --caracteres no ASCII, como el nuestro--). La
#'   bibliografía se gestiona a través de pandoc-citeproc y un archivo csl con
#'   el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/pdf y otros en los
#'   directorios de data/cache o figuras/pdf).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_pdf(file_name = "informe_proyecto_x")
#' }
informe_pdf <- function(file_name = "informe") {
  doc_format <- "pdf"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/pdf"))
    dir.create("informes/pdf", recursive = T)
  report_path <- paste0("informes/pdf/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE,
                   template = "pdf", package = "fisabior", edit = FALSE)
  copia_referencias(path = report_path)
  copia_csl(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea un informe estadístico en ODT -- Markdown
#'
#' Crea automáticamente un informe estadístico en ODT -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de pandoc-citeproc y un archivo csl
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/odt y otros en los
#'   directorios de data/cache o figuras/odt).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_odt(file_name = "informe_proyecto_x")
#' }
informe_odt <- function(file_name = "informe") {
  doc_format <- "odt"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/odt"))
    dir.create("informes/odt", recursive = T)
  report_path <- paste0("informes/odt/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "odt",
                   package = "fisabior", edit = FALSE)
  copia_referencias(path = report_path)
  copia_csl(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea un informe estadístico en DOCX -- Markdown
#'
#' Crea automáticamente un informe estadístico en DOCX -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de pandoc-citeproc y un archivo csl
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/docx y otros en los
#'   directorios de data/cache o figuras/docx).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_docx(file_name = "informe_proyecto_x")
#' }
informe_docx <- function(file_name = "informe") {
  doc_format <- "docx"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/docx"))
    dir.create("informes/docx", recursive = T)
  report_path <- paste0("informes/docx/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "docx",
                   package = "fisabior", edit = FALSE)
  copia_referencias(path = report_path)
  copia_csl(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea un informe estadístico en HTML -- Markdown
#'
#' Crea automáticamente un informe estadístico en HTML -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de pandoc-citeproc y un archivo csl
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/html y otros en los
#'   directorios de data/cache o figuras/html).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_html(file_name = "informe_proyecto_x")
#' }
informe_html <- function(file_name = "informe") {
  doc_format <- "html"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/html"))
    dir.create("informes/html", recursive = T)
  report_path <- paste0("informes/html/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "html",
                   package = "fisabior", edit = FALSE)
  copia_referencias(path = report_path)
  copia_csl(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea una presentación estadística en HTML -- Markdown
#'
#' Crea automáticamente una presentación estadística en HTML -- Markdown --
#' ioslides empleando una plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{presentacion}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de pandoc-citeproc y un archivo csl
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de presentacion/pres_html y otros en
#'   los directorios de data/cache o figuras/presentacion).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' presentacion_html(file_name = "presentacion_proyecto_x")
#' }
presentacion_html <- function(file_name = "presentacion") {
  doc_format <- "pres_html"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/presentacion/fonts"))
    dir.create("informes/presentacion/fonts", recursive = T)
  report_path <- paste0("informes/presentacion/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "pres_html",
                   package = "fisabior", edit = FALSE)
  copy_fisabior(from_ = "templates/referencias_prueba.bib",
                to_   = paste0(dirname(report_path), "/referencias_prueba.bib"))
  fonts <- list.files(system.file("templates/fonts", package = "fisabior",
                                  mustWork = TRUE), recursive = TRUE)
  dirs <- c(paste0("font-awesome-4.7.0/", c("scss", "less", "fonts", "css")),
            paste0("bootstrap-3.3.7-dist/", c("js", "fonts", "css")))
  for (i in dirs)
    dir.create(path = paste0("informes/presentacion/fonts/", i), recursive = TRUE)
  for (i in fonts) {
    copy_fisabior(from_ = paste0("templates/fonts/", i),
                  to_   = paste0("informes/presentacion/fonts/", i))
  }
  copia_referencias(path = report_path)
  copia_csl(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea un informe estadístico en PDF -- LaTeX
#'
#' Crea automáticamente un informe estadístico en PDF -- LaTeX empleando una
#' plantilla base con detalles del grupo. El documento emplea formato LaTeX puro
#' y tiene extensión .Rnw (no utiliza en ningún caso Markdown).
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   Es necesario tener instalada una distribución LaTeX que incorpore (como
#'   mínimo) los siguientes paquetes: \code{adjustbox}, \code{amsmath},
#'   \code{amssymb}, \code{array}, \code{authblk}, \code{beamer},
#'   \code{biblatex}, \code{booktabs}, \code{caption}, \code{ccicons},
#'   \code{csquotes}, \code{float}, \code{fontenc}, \code{fontspec},
#'   \code{footmisc}, \code{graphicx}, \code{hyperref}, \code{letltxmacro},
#'   \code{longtable}, \code{lscape}, \code{mathtools}, \code{microtype},
#'   \code{multirow}, \code{parskip}, \code{polyglossia}, \code{rotating},
#'   \code{setspace}, \code{tabularx}, \code{textpos}, \code{tikz},
#'   \code{xltxtra}, \code{xunicode} y el tema \code{motropolis}.
#'
#'   El proceso genera un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr y XeLaTeX. La bibliografía se gestiona a través de
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
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/latex y otros en los
#'   directorios de data/cache o figuras/latex).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_latex(file_name = "informe_proyecto_x")
#' }
informe_latex <- function(file_name = "informe") {
  doc_format <- "latex"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/latex"))
    dir.create("informes/latex", recursive = T)
  report_path <- paste0("informes/latex/", file_name, ".Rnw")
  copy_fisabior(from_ = "templates/template.Rnw",
                to_   = report_path)
  copia_referencias(path = report_path)
  copia_imagenes(path = report_path)
}



#' Crea una presentación estadística en PDF -- LaTeX -- Beamer
#'
#' Crea automáticamente una presentación estadística en PDF -- LaTeX -- Beamer
#' empleando una plantilla base con detalles del grupo. El documento emplea
#' formato LaTeX puro y tiene extensión .Rnw (no utiliza en ningún caso
#' Markdown).
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea un
#'   \emph{*.Rproj} por defecto.
#'
#'   Es necesario tener instalada una distribución LaTeX que incorpore (como
#'   mínimo) los siguientes paquetes: \code{adjustbox}, \code{amsmath},
#'   \code{amssymb}, \code{array}, \code{authblk}, \code{beamer},
#'   \code{biblatex}, \code{booktabs}, \code{caption}, \code{ccicons},
#'   \code{csquotes}, \code{float}, \code{fontenc}, \code{fontspec},
#'   \code{footmisc}, \code{graphicx}, \code{hyperref}, \code{letltxmacro},
#'   \code{longtable}, \code{lscape}, \code{mathtools}, \code{microtype},
#'   \code{multirow}, \code{parskip}, \code{polyglossia}, \code{rotating},
#'   \code{setspace}, \code{tabularx}, \code{textpos}, \code{tikz},
#'   \code{xltxtra}, \code{xunicode} y el tema \code{motropolis}.
#'
#'   El proceso genera un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr y XeLaTeX. La bibliografía se gestiona a través de
#'   \code{biblatex} empleando el gestor de referencias \code{biber}, de modo
#'   que este último debe estar instalado en el sistema.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de presentacion/latex y otros en los
#'   directorios de data/cache o figuras/presentacion).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' presentacion_beamer(file_name  = "presentacion_proyecto_x")
#' }
presentacion_beamer <- function(file_name = "presentacion") {
  doc_format <- "beamer"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/presentacion"))
    dir.create("informes/presentacion", recursive = T)
  report_path <- paste0("informes/presentacion/", file_name, ".Rnw")
  copy_fisabior(from_ = "templates/template_b.Rnw",
                to_   = report_path)
  copia_referencias(path = report_path)
  copia_imagenes(path = report_path)
}


#' Crea una presentación estadística en PDF -- R Markdown -- Beamer
#'
#' Crea automáticamente una presentación estadística en PDF -- R Markdown --
#' Beamer empleando una plantilla base con detalles del grupo. El documento
#' emplea formato R Markdown y tiene extensión .Rmd.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{init_proj}}, esto no debería causar molestias, pues se crea un
#'   \emph{*.Rproj} por defecto.
#'
#'   Es necesario tener instalada una distribución LaTeX que incorpore (como
#'   mínimo) los siguientes paquetes: \code{adjustbox}, \code{amsmath},
#'   \code{amssymb}, \code{array}, \code{authblk}, \code{beamer},
#'   \code{biblatex}, \code{booktabs}, \code{caption}, \code{ccicons},
#'   \code{csquotes}, \code{float}, \code{fontenc}, \code{fontspec},
#'   \code{footmisc}, \code{graphicx}, \code{hyperref}, \code{letltxmacro},
#'   \code{longtable}, \code{lscape}, \code{mathtools}, \code{microtype},
#'   \code{multirow}, \code{parskip}, \code{polyglossia}, \code{rotating},
#'   \code{setspace}, \code{tabularx}, \code{textpos}, \code{tikz},
#'   \code{xltxtra}, \code{xunicode} y el tema \code{motropolis}.
#'
#'   El proceso genera un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr y XeLaTeX. La bibliografía se gestiona a través de
#'   \code{biblatex} empleando el gestor de referencias \code{biber}, de modo
#'   que este último debe estar instalado en el sistema.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{init_proj}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de presentacion/latex y otros en los
#'   directorios de data/cache o figuras/presentacion).
#'
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' markdown_beamer(file_name  = "presentacion_proyecto_x")
#' }
markdown_beamer <- function(file_name = "presentacion") {
  doc_format <- "rmd_beamer"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/presentacion"))
    dir.create("informes/presentacion", recursive = T)
  report_path <- paste0("informes/presentacion/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE,
                   template = "beamer", package = "fisabior", edit = FALSE)
  copia_referencias(path = report_path)
  copia_imagenes(path = report_path)
}
