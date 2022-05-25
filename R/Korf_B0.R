#' Modelo anamórfico \eqn{\beta_0} de Korf (1939)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la asíntota (\eqn{\beta_0})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_0}:
#'     \deqn{Y_1 = Y_0 \times \left( \frac{exp \left(-\beta_1 \times E_1^{-\beta_2} \right)}{exp \left(-\beta_1 \times E_0^{-\beta_2} \right)} \right)}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B1 Valor numérico o parámetro \eqn{\beta_1}.
#' @param B2 Valor numérico o parámetro \eqn{\beta_2}.
#' @param Y0 Valor del crecimiento inicial.
#' @param E0 Valor numérico o vector de observaciones de la edad inicial
#'     de los árboles.
#' @param E1 Valor numérico o vector de observaciones de la edad futura
#'     de los árboles.
#'
#' @return Devuelve el valor del crecimiento estimado del árbol.
#'
#' @seealso \code{\link{Korf}}
#'
#' @export Korf_B0

Korf_B0 <- function(B1,B2,Y0,E0,E1){
  Y0*(exp(-B1*E1^-B2)/exp(-B1*E0^-B2))
}
