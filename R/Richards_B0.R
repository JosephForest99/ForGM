#' Modelo anamórfico \eqn{\beta_0} de Chapman-Richards (1959)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés) con el parámetro de la asíntota que se relaciona con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_0} es:
#'     \deqn{y_1 = y_0 \times \left[ \frac{1-exp(-\beta_1 \times E_1)}{1-exp(-\beta_1 \times E_0)} \right]^{\beta_2}}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Richards}}
#'
#' @export Richards_B0

Richards_B0 <- function(B1,B2,Y0,E0,E1){
  Y0*((1-exp(-B1*E1))/(1-exp(-B1*E0)))^B2
}
