\name{validateView}
\alias{validateView}
\title{Check correctness of a view defined in \code{VIEW_SCHEMA}}
\usage{
validateView(name)
}
\arguments{
  \item{name}{view name.}
}
\value{
NULL (Throws an error or prints reassurance depending on
correctness).
}
\description{
\code{validateView} checks views against
\code{TABLE_SCHEMA} and upstream views for formatting,
typos, and logical errors.
}

