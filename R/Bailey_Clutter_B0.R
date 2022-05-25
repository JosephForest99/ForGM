#' Modelo anamórfico \eqn{\beta_0} de Bailey y Clutter (1974)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la asíntota (\eqn{\beta_0})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_0} es:
#'     \deqn{y_1 = exp \left( (log(Y_0)-\beta_1\times E_0^{\beta_2})+\beta_1 \times E_1^{\beta_2} \right)}
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
#' @references Bailey & Clutter (1974). Base-age invariant polymorphic site curves.
#'     Forest Science. 20(2):155-159.
#'
#' @seealso \code{\link{Bailey_Clutter}}
#'
#' @export Bailey_Clutter.B0

Bailey_Clutter.B0 <- function(B1,B2,Y0,E0,E1){
  exp((log(Y0)-B1*E0^B2)+B1*E1^B2)
}
