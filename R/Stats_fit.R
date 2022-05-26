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
  SCE = sum(resid(fit)^2)             # Suma de cuadrados del error
  SCM = sum((data-mean(data))^2)      # Suma de cuadrados del modelo
  Desviaciones = abs(sum(resid(fit))) # Desviaciones de predicho vs observados
  n = length(resid(fit))              # Número de observaciones
  p = n - df.residual(fit)            # Número de par?metros
  S <- abs(sum(resid(fit))/n)         # Varianza del error
  SD <- sd(resid(fit))                # Deviación estándar

  # Estad?sticos de Bondad de Ajuste

  MSE = SCE/(n-p)                            # Cuadrado Medio del Error
  RMSE = sqrt(SCE/(n-p))                     # Raíz del Cuadrado Medio del Error
  R2 = 1-(SCE/SCM)                           # Coeficiente de determinación
  R2adj = 1-(SCE/SCM)*((n-1)/(n-p))          # Coeficiente de determinación ajustada
  ADJRSQ = 1-((n-1)*(1-R2))/(n-p)            # Fórmula de SAS
  CV <- (SD/mean(data))*100                  # Coeficiente de variación
  loglik = logLik(fit)                       # Con la función de R
  AIC = n*log(SCE/n)+2*p                     # Criterio de información de Akaike (AIC)
  AIC2 = AIC(fit)                            # AIC con la función de R
  AIC3 = 2*log(SCE/n)+2*p                    # Fórmula de Socha et al 2020 - Altura dominante
  AIC4 = 2*log(-loglik)+2*p                  # Código del Dr. Geros de MEM
  BIC = BIC(fit)                             # Criterio de Información Bayesiana con R
  Sesgo = (Desviaciones/n)                   # Sesgo promedio
  DW <- sum(diff(resid(fit), lag = 1)^2)/SCE # Prueba de Durbin-Watson

  Estadisticos <- data.frame(Modelo_DF = p,
                             Error_DF = n-p,
                             SCE = round(SCE,4),
                             MSE = round(MSE,4),
                             RMSE = round(RMSE,4),
                             R2 = round(R2,4),
                             R2adj= round(R2adj,4),
                             AIC= round(AIC2,4),
                             BIC= round(BIC,4),
                             loglik = round(loglik,4),
                             Sesgo=round(Sesgo,4),
                             DW = round(DW,4),
                             CV = round(CV,4),
                             S = rpund(S,4))
  return(Estadisticos)
}
