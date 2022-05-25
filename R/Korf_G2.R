#' Modelo polimórfico complejo (GADA 2) de Korf (1939)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó:
#' \deqn{y = \alpha_0 \times exp \left( -\alpha_1 \times E^{-\alpha_2} \right) } donde:
#' \deqn{\alpha_0 = exp(\chi)}
#' \deqn{\alpha_1 = \beta_0 + \frac{\beta_1}{\chi}}
#' \deqn{\alpha_2 = \beta_2}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = exp(\chi_0) \times exp \left( \left(-\beta_0 + \frac{\beta_1}{\chi_0} \right) \times E_1^{-\beta_2} \right)}
#'     donde:
#'     \deqn{\chi_0 = 0.5 \times \beta_0 \times E_0^{-\beta_2} + log(Y_0) + R_0}
#'     \deqn{R_0 = \sqrt{\left(\beta_0 \times E_0^{-\beta_2} + log(Y_0) \right)^{2} + 4 \times \beta_1 \times E_0^{-\beta_2}}}
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
#' @seealso \code{\link{Korf}}
#'
#' @export Korf_G2

Korf_G2 <- function(B0,B1,B2,Y0,E0,E1){
  R0 = sqrt((B0*E0^-B2+log(Y0))^2+4*B1*E0^-B2);
  X0 = 0.5*B0*E0^-B2+log(Y0)+R0;

  exp(X0)*exp((-B0+B1/X0)*E1^-B2);
}
