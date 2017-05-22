#' @title Mortalidad por IAM en hombres en Aragón (1991-2000)
#'
#' @description Base de datos que contiene la mortalidad observada y esperada para hombres
#' por IAM en 729 municipios de Aragón entre 1991 y 2001 y la cartografía de la
#' región.
#'
#' @format Un Spatial Polygons Data Frame (clase ) con 729 filas y siete variables:
#' \describe{
#'   \item{area}{área del municipio}
#'   \item{perimeter}{perímetro del municipio}
#'   \item{muni_id}{id INE del municipio}
#'   \item{codmuni}{id del municipio}
#'   \item{nombre}{nombre del municipio}
#'   \item{observada}{mortalidad observada}
#'   \item{esperada}{mortalidad esperada}
#' }
#'
"aragon_iam"
