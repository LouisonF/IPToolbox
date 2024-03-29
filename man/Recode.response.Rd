% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Recode.response}
\alias{Recode.response}
\title{Recode categorial variable}
\usage{
Recode.response(data, col.number, response)
}
\arguments{
\item{data:}{a dataframe containing variables to recode}

\item{col.number:}{the numeric position of the column in the datafrale}

\item{response:}{a vector containg each possible response}
}
\value{
return the number of ROC curves (possible unique association in response)
}
\description{
Recode a numeric categorial variable with strings from a response vector
}
\details{

}
