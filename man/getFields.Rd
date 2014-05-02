\name{getFields}
\alias{getFields}
\title{List available fields for a table in \code{TABLE_SCHEMA} or a view in \code{VIEW_SCHEMA}}
\usage{
getFields(name, combine = TRUE, wormhole = FALSE)
}
\arguments{
  \item{name}{table/view name.}

  \item{combine}{for internal use}

  \item{wormhole}{also for internal use}
}
\value{
vector of available field names
}
\description{
\code{getFields} is used internally to write your queries
and validate view, definitions, but it's exported for use
in middleware that exposes available fields to users.
}

