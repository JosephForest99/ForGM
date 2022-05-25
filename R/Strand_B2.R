#' Modelo polimórfico \eqn{\beta_2} de Strand generalizado (1964)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro (\eqn{\beta_2})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_2}:
#'     \deqn{Y_1 = \left( \frac{E_1}{\beta_0 + \beta_1 \times E_1} \right)^{\left( \frac{log(Y_0)}{log \left( \frac{E_0}{\beta_0 + \beta_1 \times E_0} \right)} \right)}}
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
#' @seealso \code{\link{Strand}}
#'
#' @export Strand_B2

Strand_B2 <- function(B0,B1,Y0,E0,E1){
  (E1/(B0+B1*E1))^(log(Y0)/log(E0/(B0+B1*E0)))
}
