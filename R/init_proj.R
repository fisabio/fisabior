
copy_fisabior <- function(from_, to_) {
  invisible(
    file.copy(
      from      = system.file(from_, package = "fisabior", mustWork = T),
      to        = to_
    )
  )
}


#' Crea un proyecto fisabior
#'
#' Crea automáticamente un proyecto siguiendo la plantilla fisabior, empleando
#' una jerarquía de directorios unificada, cargando archivos y funciones comunes
#' del grupo de colaboradores. Opcionalmente, genera un repositorio git.
#'
#' @export
#' @param proj_nom Cadena de caracteres con el nombre a asignar al proyecto. Por
#'   defecto se asigna el nombre "proyecto-yyy-mm-dd".
#' @param proj_dir Cadena de caracteres indicando la ruta al directorio
#'   principal donde se creará el proyecto. Por defecto se escoge el directorio
#'   actual de trabajo, devuelto por la función getwd().
#' @param git Valor lógico (TRUE o FALSE), indicando si se desea generar un
#'   repositorio git asociado al proyecto.
#' @param articulo Cadena de caracteres indicando si ha de crearse un directorio
#'   para almacenar un artículo a enviar, revisado o una copia de imprenta. Por
#'   defecto no se crea ningún directorio.
#' @param otro_lenguaje Vector de cadena de caracteres indicando los lenguajes
#'   que se emplearán (p. ej., bugs, cpp, stan). Por defecto no se crea ningún
#'   directorio.
#' @details La función crea la estructura de directorios a partir del nombre del
#'   proyecto empleando minúsculas, con independencia de si el usuario ha
#'   introducido el argumento con mayúsculas: P. ej., proj_name = "Proyecto_X"
#'   siempre será reconocido por la función como proj_name = "proyecto_x".
#'
#'   Los informes se almacenan, junto con sus figuras y datos de caché (si los
#'   hubiera), en el directorio informes y subdirectorio específico al formato.
#' @return La función crea la siguiente estructura de directorios en
#'   proj_dir/proj_nom/:
#'   \itemize{
#'   \item datos/
#'     \itemize{
#'       \item brutos/
#'       \item procesados/
#'       \item cartografia/
#'   }
#'   \item configuracion/
#'   \item r/
#' }
#'   Opcionalmente, puede crear los siguientes directorios
#' \itemize{
#'   \item informes/
#'   \itemize{
#'     \item docx/
#'     \item pdf-markdown/
#'     \item pdf-latex/
#'     \item html/
#'     \item odt/
#'     \item beamer/
#'   }
#'   \item src/
#'   \itemize{
#'     \item bugs/
#'     \item cpp/
#'     \item stan/
#'     \item ...
#'   }
#'   \item articulo/
#'   \itemize{
#'     \item enviado/
#'     \item revision/
#'     \item proof/
#'   }
#' }
#' @examples
#' \dontrun{
#' library(fisabior)
#' init_proj(proj_nom = "proyecto_europeo_X", proj_dir = "~/proyectos")
#' }
init_proj <- function(proj_nom      = NULL,
                      proj_dir      = NULL,
                      git           = TRUE,
                      otro_lenguaje = NULL,
                      articulo      = FALSE) {

  ############################################################################
  #                                                                          #
  # Comprobaciones iniciales                                                 #
  #                                                                          #
  ############################################################################

  if (is.null(proj_dir))
    stop("Indica el directorio que albergará el proyecto (proj_dir).")
  dir_info <- sapply(c(0, 1:2, 4), function(x)
    file.access(names = dirname(proj_dir), mode = x))
  if (!dir.exists(proj_dir)) {
    if (dir_info[1] != 0)
      stop("Revisa el directorio de proyecto que has proporcionado. ",
           "Quizá el problema sea que no existe el directorio inmediatamente ",
           "superior o alguno intermediario...")
    if (any(dir_info[-1] != 0)) {
      stop("No tienes permisos de ejecución, escritura o lectura en ese ",
           "directorio. Prueba con otro...")
    }
    dir.create(proj_dir, recursive = T)
  } else {
    if (any(dir_info[-1] != 0))
      stop("Revisa el directorio de proyecto que has proporcionado. ",
           "Quizá el problema sea que no existe el directorio inmediatamente ",
           "superior o alguno intermediario...")
  }
  if (is.null(proj_nom)) {
    cat("No has proporcionado un nombre para el proyecto.\n",
        "Se asigna uno por defecto en el directorio de trabajo actual\n")
    proj_nom <- paste0("proyecto-", format(Sys.time(), "%Y-%m-%d"))
  }
  proj_nom <- tolower(proj_nom)
  if (!grepl("/$", proj_dir)) proj_dir <- paste0(proj_dir, "/")
  proj_dir <- paste0(proj_dir, proj_nom)
  if (!grepl("/$", proj_dir)) proj_dir <- paste0(proj_dir, "/")
  if (dir.exists(proj_dir))
    stop("¡El directorio que iba a crearse ya existe!",
         "Marca una ruta o nombre diferente.")
  dir.create(proj_dir)

  ############################################################################
  #                                                                          #
  # Crear subdirectorios                                                     #
  #                                                                          #
  ############################################################################

  sub_dirs <- paste0(
    proj_dir,
    c(paste0("datos/", c("brutos", "procesados", "cartografia")),
      "configuracion", "r")
  )
  if (!is.null(otro_lenguaje)) {
    if (!is.character(otro_lenguaje))
      stop("Especifica los lenguajes a utilizar como cadena de caracteres.")
    for (i in otro_lenguaje)
      sub_dirs <- c(sub_dirs, paste0("src/", i))
  }
  if (articulo)
    sub_dirs <- c(sub_dirs, paste0("articulo/", c("enviado", "revision", "proof")))
  invisible(sapply(sub_dirs, dir.create, recursive = T))

  ############################################################################
  #                                                                          #
  # Copia de archivos                                                        #
  #                                                                          #
  ############################################################################

  copy_fisabior(from_ = "templates/template.Rproj",
                to_   = paste0(proj_dir, proj_nom, ".Rproj"))
  copy_fisabior(from_ = "templates/config.R",
                to_   = paste0(proj_dir, "configuracion/config.R"))
  copy_fisabior(from_ = "templates/README.Rmd",
                to_   = paste0(proj_dir, "README.Rmd"))
  knitr::knit(paste0(proj_dir, "README.Rmd"),
              paste0(proj_dir, "README.md"), quiet = T)
  sample_scripts <- paste0("r/", c("importar_datos.R", "depurar_datos.R",
                           "descriptiva.R", "analisis.R"))
  invisible(sapply(sample_scripts, function(x) {
    writeLines("\nsource('configuracion/config.R')", paste0(proj_dir, x))
  }))

  ############################################################################
  #                                                                          #
  # Configuración de Git                                                     #
  #                                                                          #
  ############################################################################

  if (.Platform$OS.type == "windows") {
    git_exist <- file.exists(
      list.files(
        path        = paste0("c:/program files", c("", " (x86)"), "/Git/bin"),
        pattern     = "git",
        recursive   = TRUE,
        ignore.case = TRUE,
        full.names  = TRUE
      )
    )
  } else {
    git_exist <- file.exists(Sys.which("git"))
  }
  if (!git_exist && git == TRUE) {
    warning("No tienes instalado git, así que no puedo crear el repositorio.")
  } else if (git == TRUE) {
    if (git2r::in_repository(proj_dir)) {
      warning("Ya exite el repositorio git: no se hace nada.")
    } else {
      repo <- git2r::init(proj_dir)
      copy_fisabior(from_ = "templates/ignore_latex_r.txt",
                    to_   = paste0(proj_dir, ".gitignore"))
      git2r::add(repo = repo, path = "*")
      invisible(git2r::commit(repo    = repo,
                              message = "Primer commit: creo proyecto",
                              all     = TRUE))
    }
  } else message("Proyecto configurado sin git: puedes emplearlo más adelante.")
  message("No olvides editar el archivo README para describir el proyecto.")
}
