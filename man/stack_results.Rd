% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requests.R
\name{stack_results}
\alias{stack_results}
\title{Adds Do Gooder metadata to a response rectangle}
\usage{
stack_results(x)
}
\arguments{
\item{x}{response list}
}
\value{
rectangle with count, next, previous values stored as attributes
}
\description{
Meta includes count, next, previous, which are provided as top level "flat"
values in the response json.
}
