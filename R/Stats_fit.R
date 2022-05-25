#' @title Estadísticos de bondad ajuste para modelos no lineales
#'
#' @description La función utiliza objetos de ajuste de modelos no lineales
#'     con la función \code{\link{nls}}.
#'
#' @param fit  Un objeto resultado del ajuste de un modelo con \code{\link{nls}}.
#' @param data Valores de la variable dependiente a evaluar.
#'
#' @seealso \code{\link{nls}}
#'
#' @export Stats_fit
#'

Stats_fit <- function(fit, data) {

  # Valores para obtener estad?sticos
  SCE = sum(resid(fit)^2) # Suma de cuadrados del error
  SCM = sum((data-mean(data))^2) # Suma de cuadrados del modelo
  Desviaciones = abs(sum(resid(fit))) # Desviaciones de predicho vs observados
  n = length(resid(fit)) # N?mero de observaciones
  p = n - df.residual(fit) # N?mero de par?metros

  # Estad?sticos de Bondad de Ajuste

  RMSE = sqrt(SCE/(n-p)) # Raíz del Cuadrado Medio del Error

  R2 = 1-(SCE/SCM) # Coeficiente de determinación

  R2adj = 1-(SCE/SCM)*((n-1)/(n-p)) # Coeficiente de determinación ajustada

  ADJRSQ = 1-((n-1)*(1-R2))/(n-p) # Fórmula de SAS

  AIC = n*log(SCE/n)+2*p # Criterio de información de Akaike (AIC)

  AIC2 = AIC(fit) # AIC con la función de R

  AIC3 = 2*log(SCE/n)+2*p # Fórmula de Socha et al 2020 - Altura dominante

  Sesgo = (Desviaciones/n) # Sesgo promedio

  DW <- sum(diff(resid(fit), lag = 1)^2)/SCE # Prueba de Durbin-Watson

  Estadisticos <- data.frame(Modelo_DF = p,
                             Error_DF = n-p,
                             SCE = round(SCE,3),
                             RMSE = round(RMSE,3),
                             R2 = round(R2,3),
                             R2adj= round(R2adj,3),
                             AIC= round(AIC,3),
                             Sesgo=round(Sesgo,3),
                             DW = round(DW,3))
  return(Estadisticos)
}
