#' Modelo polimórfico complejo (GADA 1) de Chapman-Richards (1935)
#'
#' @description La función es el resultado de la aplicación del enfoque de Diferencia
#'     Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó:
#'     \deqn{y = \alpha_0 \times \left( 1- exp(-\alpha_1 \times E) \right)^{\alpha_2}} donde:
#'     \deqn{\alpha_0 = \exp(\chi)}
#'     \deqn{\alpha_1 = \beta_0}
#'     \deqn{\alpha_2 = \beta_1 + \frac{\beta_2}{\chi}}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = Y_0 \times \left( \frac{1 + exp(-\beta_0\times E_1)}{1 + exp(-\beta_0\times E_0)} \right)^{\left( \beta_1 + \frac{\beta_2}{\chi} \right)}}
#'     donde:
#'     \deqn{\chi_0 = 0.5 \times (log(Y_0) -\beta_1 \times L_0 + R_0)}
#'     \deqn{R_0 = \sqrt{(log(Y_0) - \beta_1 \times L_0)^{2} - 4 \times \beta_2 \times L_0 }}
#'     \deqn{L_0 = log(1 - exp(-\beta_0 \times E_0))}
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
#' @export Richards_G1

Richards_G1 <- function(B0,B1,B2,Y0,E0,E1){
  L0 = log(1-exp(-B0*E0))
  R0 = sqrt((log(Y0)-B1*L0)^2-4*B2*L0)
  X0 = 0.5*((log(Y0)-B1*L0)+R0)

  Y0*((1-exp(-B0*E1))/(1-exp(-B0*E0)))^(B1+B2/X0)
}
