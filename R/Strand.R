#' Modelo base de Strand generalizado (1964)
#'
#' La función puede ser utilizada de dos formas: 1) para ajustar el modelo a
#' un conjunto de datos de crecimiento a través de la función \code{\link{nls}}
#' entre otras relacionadas con el ajuste de modelos no lineales, y 2) para realizar la
#' estimación del crecimiento a un vector de datos que corresponden a la
#' edad de los árboles con parámetros conocidos.
#'
#' @details La expresión matemática del modelo base de Strand generalizado (1964):
#' \deqn{y = \left( \frac{E}{\beta_0 + \beta_1 \times E} \right)^{\beta_2}}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0}.
#' @param B1 Valor numérico o parámetro \eqn{\beta_1}.
#' @param B2 Valor numérico o parámetro \eqn{\beta_2}.
#' @param E Valor numérico o vector de observaciones de la edad de los árboles.
#'
#' @return Devuelve el valor del crecimiento estimado del árbol.
#'
#' @references Quiñonez-Barraza et-al. (2015) - Crecimiento en diámetro normal
#'     para \emph{Pinus} en Durango. Revista Mexicana de Ciencias Forestales 6:108-125.
#'
#' @export Strand

Strand <- function(B0,B1,B2,E){
  (E/(B0+B1*E))^B2
}
