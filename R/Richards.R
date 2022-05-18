#' Modelo base de Chapman-Richards (1959)
#'
#' La función puede ser utilizada de dos formas: 1) para ajustar el modelo a
#' un conjunto de datos de crecimiento a través de la función \code{\link{nls}}
#' entre otras relacionadas con el ajuste de modelos no lineales, y 2) para realizar la
#' estimación del crecimiento a un vector de datos que corresponden a la
#' edad de los árboles con parámetros conocidos.
#'
#' @details La expresión matemática del modelo base de Chapman-Richards (1959) es:
#' \deqn{y = \beta_0 \times (1-exp(-\beta_1 \times E))^{\beta_2}}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0} que corresponde a
#'     la asíntota horizontal.
#' @param B1 Valor numérico o parámetro \eqn{\beta_1} que corresponde a la
#'     tasa de crecimiento.
#' @param B2 Valor numérico o parámetro \eqn{\beta_2} que corresponde a la
#'     tasa de cambio.
#' @param E Valor numérico o vector de observaciones de la edad de los árboles.
#'
#' @return Devuelve el valor del crecimiento estimado del árbol.
#' @references Quiñonez-Barraza et al. (2015). Índice de sitio con polimorfimo
#'     complejo para masas forestales de Durango, México. Agrociencia 49:439-454.
#'
#' @export Richards

Richards <- function(B0,B1,B2,E){
  B0*(1-exp(-B1*E))^B2
}
