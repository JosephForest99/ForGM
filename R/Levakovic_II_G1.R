#' Modelo polimórfico complejo (GADA 1) de Levakovic II (1935)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó
#' \eqn{y = \alpha_0 \times \left( \frac{E}{\alpha_1+E} \right)^{\alpha_2} }, donde:
#' \deqn{\alpha_0 = exp(\chi)}
#' \deqn{\alpha_1 = \beta_1}
#' \deqn{\alpha_2 = \frac{\beta_2}{\chi}}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = exp(\chi_0) \times \left( \frac{E_1}{\beta_1+E_1} \right)^{\frac{\beta_2}{\chi_0} } }
#'     donde:
#'     \deqn{\chi_0 = \frac{log(Y_0) + \sqrt{log(Y_0)^2 - 4 \times \beta_1 \times \left( \frac{E_1}{\beta_1+E_1} \right)} }{2}}
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
#' @references Vera-Vélez et al. (2013). Efecto de variables climáticas y litológicas
#'     sobre el crecimiento en altura dominante en masas de \emph{Pinus nigra} Arn.
#'
#' @seealso \code{\link{Levakovic_II}}
#'
#' @export Levakovic_II_G1

Levakovic_II_G1 <- function(B1,B2,Y0,E0,E1){
  X0 = (log(Y0)+sqrt(log(Y0)^2-4*B2*(log(E0/(B1+E0))) ) )/2;

  exp(X0)*(E1/(B1+E1))^(B2/X0)
}
