#' Modelo polimórfico \eqn{\beta_2} de Levakovic II (1935)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés) con el parámetro de la
#' tasa de cambio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_2} es:
#'     \deqn{Y_1 = \beta_0 \times \left( \frac{E_1}{\beta_1+E_1} \right)^ { \left( \frac{log \left(\frac{Y_0}{\beta_0} \right)}{log \left( \frac{E_0}{(\beta_1+E_0)} \right)} \right) } }
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Levakovic_II}}
#'
#' @export Levakovic_II_B2


Levakovic_II_B2 <- function(B0,B1,Y0,E0,E1){
  B0*(E1/(B1+E1))^(log(Y0/B0)/log(E0/(B1+E0)))
}
