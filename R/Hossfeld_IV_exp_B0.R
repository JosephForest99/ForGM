#' Modelo anamórfico \eqn{\beta_0} de Hossfeld IV exponencial (1882)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro de la asíntota (\eqn{\beta_0})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_0}:
#'     \deqn{Y_1 = Y_0 \times \left( \frac{1+exp(\beta_1) \times exp(-\beta_2 \times log(E_0))} {1+exp(\beta_1) \times exp(-\beta_2 \times log(E_1))} \right)}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Hossfeld_IV_exp}}
#'
#' @export Hossfeld_IV_exp_B0

Hossfeld_IV_exp_B0 <- function(B1,B2,Y0,E0,E1){
  Y0*((1+exp(B1)*exp(-B2*log(E0)))/(1+exp(B1)*exp(-B2*log(E1))))
}
