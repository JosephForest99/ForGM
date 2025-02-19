% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hossfeld_IV_exp_B1.R
\name{Hossfeld_IV_exp_B1}
\alias{Hossfeld_IV_exp_B1}
\title{Modelo polimórfico \eqn{\beta_1} de Hossfeld IV exponencial (1822)}
\usage{
Hossfeld_IV_exp_B1(B0, B2, Y0, E0, E1)
}
\arguments{
\item{B0}{Valor numérico o parámetro \eqn{\beta_0}.}

\item{B2}{Valor numérico o parámetro \eqn{\beta_2}.}

\item{Y0}{Valor del crecimiento inicial.}

\item{E0}{Valor numérico o vector de observaciones de la edad inicial
de los árboles.}

\item{E1}{Valor numérico o vector de observaciones de la edad futura
de los árboles.}
}
\value{
Devuelve el valor del crecimiento estimado del árbol.
}
\description{
La función es el resultado de la aplicación del enfoque de Diferencia
Algebraica (ADA, por sus siglas en inglés). El parámetro de la
tasa de crecimiento (\eqn{\beta_1}) se relacionó con el sitio.
}
\details{
El modelo ADA con valor específico del sitio \eqn{\beta_1}:
    \deqn{Y_1 =  \frac{\beta_0}{1+exp \left( log \left( \frac{ \frac{\beta_0-Y_o}{Y_0}}{exp(-\beta_2 \times log(E_0))} \right) \right) \times exp(-\beta_2\times log(E_1))}}
}
\references{
Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
Agrociencia 52:437-453.
}
\seealso{
\code{\link{Hossfeld_IV_exp}}
}
\author{
Abel Joseph Hernández-Martínez
}
