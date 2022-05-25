#' Modelo polimórfico \eqn{\beta_1} de Korf (1939)
#'
#' La función es el resultado de la aplicación del enfoque de Diferencia
#' Algebraica (ADA, por sus siglas en inglés). El parámetro (\eqn{\beta_1})
#' se relacionó con el sitio.
#'
#' @details El modelo ADA con valor específico del sitio \eqn{\beta_1}:
#'     \deqn{Y_1 = \beta_0 \times \left( \frac{Y_0}{\beta_0} \right)^{ \left(\frac{E_1}{E_0} \right)^{-\beta_2} }}
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
#'
#' @references Diéguez-Aranda et al (2005) - Site quality equations for
#'     \emph{Pinus sylvestris} plantations in Galicia (northwestern Spain).
#'     Annals of Forest Science, 62(2), 143-152.
#'
#' @seealso \code{\link{Korf}}
#'
#' @export Korf_B1

Korf_B1 <- function(B0,B2,Y0,E0,E1){
  B0*(Y0/B0)^(E0/E1)^B2
}
