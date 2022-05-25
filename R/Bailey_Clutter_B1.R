#' Modelo polimórfico \eqn{\beta_1} de Bailey y Clutter (1974)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la
#' tasa de crecimiento (\eqn{\beta_1}) se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_1}:
#'     \deqn{y_1 = exp\left(\beta_0 + \left( \frac{log(Y_0)-\beta_0}{E_0^{\beta_2} }\right) \times E_1^{\beta_2} \right)}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0}.
#' @param B2 Valor numérico o parámetro \eqn{\beta_2}.
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
#' @export Bailey_Clutter_B1


Bailey_Clutter_B1 <- function(B0,B2,Y0,E0,E1){
  exp(B0+((log(Y0)-B0)/E0^B2)*E1^B2)
}
