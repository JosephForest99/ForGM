#' Modelo polimórfico complejo (GADA 3) de Chapman-Richards (1935)
#'
#' @description La función es el resultado de la aplicación del enfoque de Diferencia
#'     Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó:
#'     \deqn{y = \alpha_0 \times \left( 1- exp(-\alpha_1 \times E) \right)^{\alpha_2}} donde:
#'     \deqn{\alpha_0 = \exp(\beta_0 + \frac{\beta_1}{\chi_0})}
#'     \deqn{\alpha_1 = \beta_2}
#'     \deqn{\alpha_2 = \frac{1}{\chi}}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = exp \left( \beta_0 + \beta_1 \times \left( \frac{ log(Y_0) - \beta_0 }{log \left( 1 - exp \left( -\beta_2 \times E_0 \right) + \beta_1 \right)} \right) \right)  \times \left( 1 - exp \left( -\beta_2 \times E_1 \right) \right)^{\left( \frac{log(Y_0) - \beta_0}{ log \left( 1 - exp \left( -\beta_2 \times E_0 \right) + \beta_1 \right)} \right)}}
#'
#' @author Abel Joseph Hernández-Martínez
#'
#' @param B0 Valor numérico o parámetro \eqn{\beta_0}.
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
#' @references Tamarit-Urias et al. (2014). Ecuaciones dinámicas de índice de
#'    sitio para \emph{Tectona grandis} en Campeche, México. Agrociencia 48: 225-238.
#'
#' @seealso \code{\link{Richards}}
#'
#' @export Richards_G3

Richards_G3 <- function(B0,B1,B2,Y0,E0,E1){
  exp(B0 + B1*((log(Y0)-B0) /(log(1-exp(-B2*E0))+B1) )) * (1-exp(-B2*E1))^((log(Y0)-B0)/(log(1-exp(-B2*E0))+B1))
}
