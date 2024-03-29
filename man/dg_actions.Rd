% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/actions.R
\name{dg_actions}
\alias{dg_actions}
\title{DG Actions}
\usage{
dg_actions(
  since = this_morning(),
  campaign_id = NULL,
  starting_with = c("earliest", "latest"),
  process_pagination = FALSE,
  page_size = 1000,
  max_requests = Inf
)
}
\arguments{
\item{since}{time object representing the earliest action to be returned}

\item{campaign_id}{vector of campaign id integers to be returned}

\item{starting_with}{should the return list be arranged to start with the
earliest or latest action}

\item{process_pagination}{should additional pages of data be retrieved?}

\item{page_size}{the number of records to retrieve in each request}

\item{max_requests}{the maximum number of requests to perform when retrieving
additional pages of data}
}
\value{
tibble with nested columns for campaign, action, form_data
}
\description{
Returns a feed the latest action log entries for the entire account. It
includes the campaign, action, and form data submitted by the supporter.
}
\details{
Useful convenience functions for 'since' argument are documented with the
\code{\link[=first_moment]{first_moment()}} function.
}
\examples{
minimum_time <- lubridate::now() - lubridate::days(5)

\dontrun{
dg_actions(
  since = minimum_time,
  campaign_id = 5618,
  starting_with = "latest",
  process_pagination = FALSE
)
}
}
