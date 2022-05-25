#' Modelo base de Hossfeld IV exponencial (1822)
#'
#' La función puede ser utilizada de dos formas: 1) para ajustar el modelo a
#' un conjunto de datos de crecimiento a través de la función \code{\link{nls}}
#' entre otras relacionadas con el ajuste de modelos no lineales, y 2) para realizar la
#' estimación del crecimiento a un vector de datos que corresponden a la
#' edad de los árboles con parámetros conocidos.
#'
#' @details La expresión matemática del modelo base de Hossfeld IV exponencial (1822):
#' \deqn{y = \frac{\beta_0}{1 + exp(\beta_1) \times exp(-\beta_2 \times log(E))}}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @export Hossfeld_IV_exp

Hossfeld_IV_exp <- function(B0,B1,B2,E){
  B0/(1+exp(B1)*exp(-B2*log(E)))
}
