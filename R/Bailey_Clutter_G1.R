#' Modelo polimórfico complejo (GADA 1) de Bailey y Clutter (1974)
#'
#' @description La función es el resultado de la aplicación del enfoque de Diferencia
#'     Algebraica Generalizada (GADA, por sus siglas en inglés). La ecuación original se reparametrizó
#'     \eqn{y =  exp \left( \alpha_0 + \alpha_1 E^{\beta_2}  \right) }, donde:
#'     \deqn{\alpha_0 = \beta_0 + \chi}
#'     \deqn{\alpha_1 = \beta_1  \times \chi}
#'     \deqn{\alpha_2 = \beta_2}
#'
#' @details El modelo GADA:
#'     \deqn{Y_1 = exp \left( \beta_0 + \chi_0 +\beta_1 \chi_0 E_0^{\beta_2} \right)}
#'     donde:
#'     \deqn{\chi_0 = \frac{log(Y_0)-\beta_0}{1-\beta_1 \times E_0^{\beta_2}}}
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
#' @references Özçelik et al  (2019). Modeling dominant height growth of cedar
#'     (\emph{Cedrus libani} A. Rich) stands in Turkey. Forest Science, 65(6),
#'     725-733.
#'
#' @seealso \code{\link{Bailey_Clutter}}
#'
#' @export Bailey_Clutter_G1

Bailey_Clutter_G1 <- function(B0,B1,B2,Y0,E0,E1){
  X0 = (log(Y0)-B0)/(1+B1*E0^B2)
  exp(B0+X0+B1*X0*E1^B2)
}
