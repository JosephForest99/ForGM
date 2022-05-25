#' Modelo polimórfico \eqn{\beta_2} de Bailey y Clutter (1974)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la
#' tasa de cambio (\eqn{\beta_2}) se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_2}:
#'     \deqn{y_1 = exp \left( \beta_0 + \beta_1 \times E_1^ { \left( \frac{log \left( \frac{log(Y_0)-\beta_0}{\beta_1} \right)} {log(E_0)}  \right)} \right)}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0}.
#' @param B1 Valor numérico o parámetro \eqn{\beta_1}.
#' @param Y0 Valor del crecimiento inicial.
#' @param E0 Valor numérico o vector de observaciones de la edad inicial
#'     de los árboles.
#' @param E1 Valor numérico o vector de observaciones de la edad futura
#'     de los árboles.
#'
#' @return Devuelve el valor del crecimiento estimado del árbol.
#'
#' @references Bailey & Clutter (1974). Base-age invariant polymorphic site curves.
#'     Forest Science. 20(2):155-159.
#'
#' @seealso \code{\link{Bailey_Clutter}}
#'
#' @export Bailey_Clutter_B2

Bailey_Clutter_B2 <- function(B0,B1,Y0,E0,E1){
  exp(B0+B1*E1^( log( (log(Y0)-B0)/B1 ) / log(E0) ))
}
