% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile_acs_data.R
\name{compile_acs_data}
\alias{compile_acs_data}
\title{Analysis-ready social science measures}
\usage{
compile_acs_data(
  variables = NULL,
  years = c(2022),
  geography = "county",
  states = NULL,
  counties = NULL,
  spatial = FALSE
)
}
\arguments{
\item{variables}{A named vector of ACS variables such as that returned from
\code{urbnindicators::list_acs_variables()}.}

\item{years}{A character vector (or coercible to the same) comprising one or more
four-digit years for which to pull five-year American Community Survey estimates.}

\item{geography}{A geography type that is accepted by \code{tidycensus::get_acs()}, e.g.,
"tract", "county", "state", among others. Geographies below the tract level are not
supported.}

\item{states}{A vector of one or more state names, abbreviations, or codes as
accepted by \code{tidycensus::get_acs()}.}

\item{counties}{A vector of five-digit county FIPS codes. If specified, this parameter
will override the \code{states} parameter. If \code{NULL}, all counties in the the
state(s) specified in the \code{states} parameter will be included.}

\item{spatial}{Boolean. Return a simple features (sf), spatially-enabled dataframe?}
}
\value{
A dataframe containing the requested \code{variables}, their MOEs,
a series of derived variables, such as percentages, and the year of the data.
Returned data are formatted wide. A codebook generated with \code{generate_codebook()}
is attached and can be accessed via \code{compile_acs_data() \%>\% attr("codebook")}.
}
\description{
Construct measures frequently used in social sciences
research, leveraging \code{tidycensus::get_acs()} to acquire raw estimates from
the Census Bureau API.
}
\examples{
\dontrun{
acs_variables = list_acs_variables(year = "2022")
df = compile_acs_data(
  variables = acs_variables,
  years = c(2021, 2022),
  geography = "county",
  states = "NJ",
  counties = NULL,
  spatial = FALSE)
  }
}
\seealso{
\code{tidycensus::get_acs()}, which this function wraps.
}
