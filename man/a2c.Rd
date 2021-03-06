% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a2c.R
\name{a2c}
\alias{a2c}
\title{Convert data format}
\usage{
a2c(a)
}
\arguments{
\item{a}{A data.frame with the format a. Un data.frame en formato a}
}
\value{
A data.frame wih the format c. Un data.frame en formato c
}
\description{
Convert fish transect data from format a to format c; See details. Convierte datos de transectos de peces del formato a al formato c; Ver detalles
}
\examples{
#library(MPAtools)

#load an "a" sample dataset
data("a")

#convert a to b format
c <- a2c(a)
c

}
\author{
Villaseñor-Derbez, J.C. <jvillasenor@turfeffect.org>
}
