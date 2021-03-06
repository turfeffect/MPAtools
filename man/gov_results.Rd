% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gov_results.R
\name{gov_results}
\alias{gov_results}
\title{Governance Results}
\usage{
gov_results(values, data, reserva)
}
\arguments{
\item{values}{A list, created within the Shiny app, that contains the list of indicators selected and the community}

\item{data}{The data to be analyzed}

\item{reserva}{The name of the reserve}
}
\value{
results, a tibble containing the name of the indicator as well as DiD estimates, p values, a string to be used as input for valueBox, the color of the valueBox, and the lm object fit to the indicator extracted from the data
}
\description{
Analyzes all the required governance indicators at once
}
