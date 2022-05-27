#' Modelo polimórfico complejo (GADA 1) de Korf (1939)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó:
#' \deqn{y = \alpha_0 \times exp \left( -\alpha_1 \times E^{-\alpha_2} \right) } donde:
#' \deqn{\alpha_0 = exp(\chi)}
#' \deqn{\alpha_1 = \beta_1 + \frac{1}{\chi}}
#' \deqn{\alpha_2 = \beta_2}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = exp(\chi_0) \times exp \left( \frac{-1}{E_1^{\beta_2}} \right)^{\left( \beta_1 + \frac{1}{\chi_0} \right)}}
#'     donde:
#'     \deqn{\chi_0 = 0.5 \times (-L_0 + R_0)}
#'     \deqn{R_0 = \sqrt{L_0^2 - 4 \times log \left( exp \left( \frac{-1}{E_0^{\beta_2}} \right) \right) }}
#'     \deqn{L_0 =  \beta_1 \times \left( \frac{-1}{E_0^{\beta_2}} \right) - log(Y_0)}
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
#' @references Tamarit-Urias et al. (2014). Ecuaciones dinámicas de índice de
#'    sitio para \emph{Tectona grandis} en Campeche, México. Agrociencia 48: 225-238.
#'
#' @seealso \code{\link{Korf}}
#'
#' @export Korf_G1

Korf_G1 <- function(B1,B2,Y0,E0,E1){
  L0 = B1*(-1/E0^B2)-log(Y0)
  R0 = sqrt(L0^2-4*log(exp(-1/E0^B2)));
  X0 = 0.5*(-L0+R0);

  exp(X0)*exp(-1/E1^B2)^(B1+1/X0);
}
