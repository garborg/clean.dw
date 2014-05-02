\name{validateViews}
\alias{validateViews}
\title{Check correctness of all views defined in \code{VIEW_SCHEMA}}
\usage{
validateViews()
}
\value{
NULL (Throws an error or prints reassurance depending on
correctness).
}
\description{
\code{validateViews} checks views against
\code{TABLE_SCHEMA} and upstream views for formatting,
typos, and logical errors.
}

