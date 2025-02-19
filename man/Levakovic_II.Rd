% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Levakovic_II.R
\name{Levakovic_II}
\alias{Levakovic_II}
\title{Modelo base de Levakovic II (1935)}
\usage{
Levakovic_II(B0, B1, B2, E)
}
\arguments{
\item{B0}{Valor numérico o parámetro \eqn{\beta_0} que corresponde a
la asíntota horizontal.}

\item{B1}{Valor numérico o parámetro \eqn{\beta_1}.}

\item{B2}{Valor numérico o parámetro \eqn{\beta_2}.}

\item{E}{Valor numérico o vector de observaciones de la edad de los árboles.}
}
\value{
Devuelve el valor del crecimiento estimado del árbol.
}
\description{
La función puede ser utilizada de dos formas: 1) para ajustar el modelo a
un conjunto de datos de crecimiento a través de la función \code{\link{nls}}
entre otras relacionadas con el ajuste de modelos no lineales, y 2) para realizar la
estimación del crecimiento a un vector de datos que corresponden a la
edad de los árboles con parámetros conocidos.
}
\details{
La expresión matemática del modelo base de Levakovic II (1935):
\deqn{y = \beta_0 \times (\frac{E}{\beta_1+E})^{\beta_2}}
}
\references{
Hernández-Cuevas et al. (2018). Modelos de crecimiento en altura
dominante e índices de sitio para \emph{Pinus ayacahuite} Ehren.
Agrociencia 52:437-453.
}
\author{
Abel Joseph Hernández-Martínez
}
