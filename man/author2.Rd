% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/academic_graph.R, R/api.R
\name{author2}
\alias{author2}
\title{This function retrieves Semantic Scholar data for
an author given the S2Author identifier}
\usage{
author2(
  author_id,
  details = c("papers"),
  offset = 0,
  limit = 10,
  fields = NULL
)

author2(
  author_id,
  details = c("papers"),
  offset = 0,
  limit = 10,
  fields = NULL
)
}
\arguments{
\item{author_id}{string with author identifier}

\item{details}{one of "authors", "citations" or "references"}

\item{offset}{integer paging offset}

\item{limit}{integer paging length}

\item{fields}{extra fields to include, for example "title,authors"}
}
\value{
list representing author object

list representing author object
}
\description{
This function retrieves Semantic Scholar data for
an author given the S2Author identifier

This function retrieves Semantic Scholar data for
an author given the S2Author identifier
}
\details{
Example of Accessible Paper Identifiers:
\itemize{
\item S2 Author ID : 1681232
}

Example of Accessible Paper Identifiers:
\itemize{
\item S2 Author ID : 1681232
}
}
\examples{
\dontrun{
 author(1681232)
 author2(1681232, fields="affiliations,aliases,externalIds")
 author2(1681232, fields="paperId,externalIds", details = "papers", offset=0, limit = 100)
 }
\dontrun{
 author(1681232)
 author2(1681232, fields="affiliations,aliases,externalIds")
 author2(1681232, fields="paperId,externalIds", details = "papers", offset=0, limit = 100)
 }
}
