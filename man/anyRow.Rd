\name{anyRow}
\alias{anyRow}
\title{Translate a data table into \code{AND}/\code{OR} objects.}
\usage{
anyRow(dt)
}
\arguments{
  \item{dt}{A \code{data.table}, the names being field
  names, the values being possible values for those
  fields.}
}
\value{
An efficiently nested S3 object of class \code{OR} or
\code{AND}.
}
\description{
\code{anyRow} Takes a data.table and translates it into an
\code{AND}/\code{OR} object requiring that all values of at
least one row must be satisfied.
}

