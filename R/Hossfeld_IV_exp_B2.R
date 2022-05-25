#' Modelo polimórfico \eqn{\beta_2} de Hossfeld IV exponencial (1882)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la
#' tasa de cambio (\eqn{\beta_2}) se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_2}:
#'     \deqn{Y_1 = \frac{\beta_0}{1+exp(\beta_1) \times exp \left( \left( \frac{log \left( \frac{\beta_0 - Y_0}{Y_0 \times exp(\beta_1)} \right)}{log(E_0)} \right) \times log(E_1) \right)}}
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
#' @seealso \code{\link{Hossfeld_IV_exp}}
#'
#' @export Hossfeld_IV_exp_B2

Hossfeld_IV_exp_B2 <- function(B0,B1,Y0,E0,E1){
  B0/(1+exp(B1)*exp((log((B0-Y0)/(Y0*exp(B1)))/log(E0))*log(E1)))
}
