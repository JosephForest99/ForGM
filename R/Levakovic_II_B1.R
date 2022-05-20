#' Modelo polimórfico \eqn{\beta_1} de Levakovic II (1935)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés) con el parámetro de la
#' tasa de crecimiento.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_1} es:
#'     \deqn{Y_1 = \beta_0 \times \left( E_1 \times \left( \left( \frac{E_0} {exp \left( \frac{log(Y_0/\beta_0)}{\beta_2} \right)}-E_0 \right)+E_1 \right)^{-1} \right)^{\beta_2}}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Levakovic_II}}
#'
#' @export Levakovic_II_B1

Levakovic_II_B1 <- function(B0,B2,Y0,E0,E1){
  B0*(E1*((E0/exp(log(Y0/B0)/B2)-E0)+E1)^-1)^B2
}
