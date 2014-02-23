\name{OR}
\alias{OR}
\title{Format conditions to be passed to a where clause.}
\usage{
OR(...)
}
\arguments{
  \item{...}{A variable number of arguments, each an
  \code{AND} or \code{OR} object or a named argument, the
  name being a field name, the value: list(['=', '>', '<',
  'like', or 'between'. optional '!' prepend], [values]) or
  just a vector of values, in which case '=' is assumed.}
}
\value{
S3 object of class \code{OR}.
}
\description{
\code{OR} formats conditions to be passed to a where
clause.
}

