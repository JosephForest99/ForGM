#' Modelo polimórfico \eqn{\beta_1} de Strand generalizado (1964)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro (\eqn{\beta_1})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_1}:
#'     \deqn{Y_1 = \left( \frac{E_1}{\beta_0 + \left( \frac{E_0 \times Y_0^{\frac{-1}{\beta_2}} - \beta_0}{E_0}  \right) \times E_1} \right)^{\beta_2}}
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
#' @seealso \code{\link{Strand}}
#'
#' @export Strand_B0

Strand_B1 <- function(B0,B2,Y0,E0,E1){
  (E1/(B0+( (E0*Y0^(-1/B2)-B0)/E0 )*E1))^B2
}
