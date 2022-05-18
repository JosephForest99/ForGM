#' Modelo polimórfico \eqn{\beta_1} de Chapman-Richards (1959)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés) con el parámetro de la
#' tasa de crecimiento.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_1} es:
#'     \deqn{y_1 = \beta_0 \times \left(1-\left(1-\left(\frac{Y_0}{\beta_0}\right)^{(\frac{1}{\beta_2})}\right)^{(\frac{E_1}{E_0})}\right)^{B2}}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Richards}}
#'
#' @export Richards_B1

Richards_B1 <- function(B0,B2,YO,E0,E1){
  B0*(1-(1-(Y0/B0)^(1/B2))^(E1/E0))^B2
}
