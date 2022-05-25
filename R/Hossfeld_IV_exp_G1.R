#' Modelo polimórfico complejo (GADA 1) de Hossfeld IV exponencial (1822)
#'
#' @description La función es el resultado de la aplicación del enfoque de Diferencia
#'     Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó:
#'     \deqn{y = \frac{\alpha_0}{a+exp(\alpha_1) \times exp(-\alpha_2 \times log(E))} } donde:
#'     \deqn{\alpha_0 = \beta_0 + \chi}
#'     \deqn{\alpha_1 = \frac{\beta_1}{\chi}}
#'     \deqn{\alpha_2 = \beta_2}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = \frac{\beta_1 + \chi_0}{1 + \frac{\beta_1}{\chi_0} \times E_1^{-\beta_2} }}
#'     donde:
#'     \deqn{\chi_0 = \frac{Y_0 - \beta_0 + \sqrt{(Y_0-\beta_0)^2 - 4 \times \beta_1 \times Y_0 \times E_0^{-\beta_2}}}{2}}
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
#' @references Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
#' dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
#' Agrociencia 52:437-453.
#'
#' @seealso \code{\link{Hossfeld_IV_exp}}
#'
#' @export Hossfeld_IV_exp_G1

Hossfeld_IV_exp_G1 <- function(B0,B1,B2,Y0,E0,E1){
  X0 = 0.5*(Y0-B0+sqrt((Y0-B0)^2+4*B1*Y0*E0^-B2))

  (B0+X0)/(1+B1/X0*E1^-B2)
}
