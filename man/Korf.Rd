% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Korf.R
\name{Korf}
\alias{Korf}
\title{Modelo base de Korf (1939)}
\usage{
Korf(B0, B1, B2, E)
}
\arguments{
\item{B0}{Valor numérico o parámetro \eqn{\beta_0}.}

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
La expresión matemática del modelo base de Korf:
\deqn{y = \beta_0 \times exp(-\beta_1 \times E^{-\beta_2})}
}
\author{
Abel Joseph Hernández-Martínez
}
