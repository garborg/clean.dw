\name{SQL}
\alias{SQL}
\title{Let me write your queries for you.}
\usage{
SQL(select, from, where = NULL, groupby = NULL)
}
\arguments{
  \item{select}{Field names.}

  \item{from}{'db.tablename' or '@viewname', corresponding
  to an entry in \code{tableFields} or \code{viewSpec}.}

  \item{where}{An \code{AND} or \code{OR} object.}

  \item{groupby}{Optional, vector of field names. If
  provided, elements of select not in \code{groupby} will
  have an aggregate function applied.  Those elements may
  be named in the select argument as follows: '', '[new
  name].[function], '[new name].', '.[function]', or
  '[function]'.  Defaults: '[old name].[sum]'}
}
\value{
Query string.
}
\description{
\code{SQL} uses functions \code{tableFields} and
\code{viewSpec} from the calling scope to construct queries
from simple arguments.
}

