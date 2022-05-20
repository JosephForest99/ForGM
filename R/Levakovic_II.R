#' Modelo base de Levakovic II (1935)
#'
#' La función puede ser utilizada de dos formas: 1) para ajustar el modelo a
#' un conjunto de datos de crecimiento a través de la función \code{\link{nls}}
#' entre otras relacionadas con el ajuste de modelos no lineales, y 2) para realizar la
#' estimación del crecimiento a un vector de datos que corresponden a la
#' edad de los árboles con parámetros conocidos.
#'
#' @details La expresión matemática del modelo base de Levakovic II (1935):
#' \deqn{y = \beta_0 \times (\frac{E}{\beta_1+E})^{\beta_2}}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0} que corresponde a
#'     la asíntota horizontal.
#' @param B1 Valor numérico o parámetro \eqn{\beta_1}.
#' @param B2 Valor numérico o parámetro \eqn{\beta_2}.
#' @param E Valor numérico o vector de observaciones de la edad de los árboles.
#'
#' @return Devuelve el valor del crecimiento estimado del árbol.
#'
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @export Levakovic_II

Levakovic_II <- function(B0,B1,B2,Y,E){
  B0*(E/(B1+E))^B2
}
