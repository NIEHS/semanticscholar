#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")

cfg <- function() {

  res <- list(
    s2_api = "https://api.semanticscholar.org/",
    s2_ratelimit = round((5 * 60) / (100 - 1), digits = 2)
  )

  if (Sys.getenv("SEMANTICSCHOLAR_API") != "") {
    res$s2_key <- Sys.getenv("SEMANTICSCHOLAR_API")
    res$s2_ratelimit <- 1 / (100 - 1) # for approx 100 requests per second
  }

  return(res)
}

#' Endpoint used for requests to Semantic Scholar API
#'
#' When environment variable SEMANTICSCHOLAR_API is set to a valid API key this
#' value differs as endpoints allowing higher rate limits are being used.
#' @export
#' @noRd
s2_api <- function()
  cfg()$s2_api

#' Rate limit for API calls
#'
#' The minimum interval in seconds between requests to the API.
#'
#' When environment variable SEMANTICSCHOLAR_API is set to a valid API key this
#' value can be approx 0.01 (for 100 requests per second) but if no API key is
#' used, the default value will be around 3.5 (allowing a maximum of 100 requests per 5 minutes)
#' @export
#' @noRd
s2_ratelimit <- function()
  cfg()$s2_ratelimit

#' Attribution
#'
#' Use this attribution whenever data from the API is publicly displayed
#'
#' @details Semantic Scholar provides a RESTful API for convenient linking
#' to Semantic Scholar pages and pulling information about individual records
#' on demand. When publicly displaying data from this API,
#' please incorporate the Semantic Scholar name and logo and point back to
#' Semantic Scholar at https://www.semanticscholar.org/ with
#' a utm_source=api UTM parameter.
#' @export
s2_attribution <- function() {
  sprintf(
    "Data source: Semantic Scholar API\n%s?utm_source=api\n%s", s2_api(), "\n",
    "Data license agreement: https://www.semanticscholar.org/product/api/license")
  # https://www.semanticscholar.org/paper/The-Semantic-Scholar-Open-Data-Platform-Kinney-Anastasiades/cb92a7f9d9dbcf9145e32fdfa0e70e2a6b828eb1
}

#' Retrieve paper information
#'
#' This function retrieves Semantic Scholar data for a paper
#' given its identifier
#' @param identifier string with identifier
#' @param include_unknown_refs logical, Default: FALSE
#' @details
#' Example of Accessible Paper Identifiers:
#' <sha> - a Semantic Scholar ID, e.g. 649def34f8be52c8b66281af98ae884c09aef38b
#' CorpusId:<id> - a Semantic Scholar numerical ID, e.g. 215416146
#' DOI:<doi> - a Digital Object Identifier, e.g. DOI:10.18653/v1/N18-3011
#' ARXIV:<id> - arXiv.rg, e.g. ARXIV:2106.15928
#' MAG:<id> - Microsoft Academic Graph, e.g. MAG:112218234
#' ACL:<id> - Association for Computational Linguistics, e.g. ACL:W12-3903
#' PMID:<id> - PubMed/Medline, e.g. PMID:19872477
#' PMCID:<id> - PubMed Central, e.g. PMCID:2323736
#' URL:<url> - URL from one of the sites listed below, e.g. URL:https://arxiv.org/abs/2106.15928v1
#'
#' URLs are recognized from the following sites:
#' semanticscholar.org
#' arxiv.org
#' aclweb.org
#' acm.org
#' biorxiv.org
#'
#' https://api.semanticscholar.org/api-docs/graph#tag/Paper-Data/operation/get_graph_get_paper
#'
#' @return list representing S2 paper object
#' @examples
#' \dontrun{
#'  get_paper_details("fb5d1bb23724d9a5a5eae036a2e3cf291cac2c1b")
#'  }
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @export
get_paper_details <- function(identifier, include_unknown_refs = FALSE) {

  q <- NULL

  if (include_unknown_refs)
    q <- list(include_unknown_refs = "true")

  key <- cfg()$s2_key
  api <- s2_api()

  if (!is.null(key) && nchar(key) > 10) {
    res <- httr::GET(url = api, httr::add_headers(`x-api-key` = key),
      path = sprintf("v1/paper/%s", identifier), query = q)
  } else {
    res <- httr::GET(url = api,
      path = sprintf("v1/paper/%s", identifier), query = q)
  }

  if (status_code(res) == 200) {
    res <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "utf-8"),
      simplifyDataFrame = TRUE
    )
    class(res$references) <- c("tbl_df", "tbl", "data.frame")
    return(res)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")

  stop("HTTP status", status_code(res))

  # list(
  #   abstract = purrr::pluck(x, "abstract"),
  #   authors = purrr::pluck(x, "authors") %>% purrr::map_df(dplyr::as_tibble),
  #   citations_authors = purrr::pluck(x, "citations", "authors") %>% purrr::map_df(dplyr::as_tibble)
  # )
}


#' Retrieve author information
#'
#' This function retrieves Semantic Scholar data for
#' an author given the S2Author identifier
#' @param author_id string with author identifier
#' @details
#' Example of Accessible Paper Identifiers:
#' - S2 Author ID : 1741101
#'
#' Limitations:
#' Can only return up to 10 MB of data at a time.
#' @return list representing author object
#' @examples
#' \dontrun{
#'  get_author_details(1741101)
#'  }
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @export
get_author_details <- function(author_id) {

  identifier <- author_id
  key <- cfg()$s2_key
  api <- s2_api()

  if (!is.null(key) && nchar(key) > 10) {
    res <- httr::GET(url = api, httr::add_headers(`x-api-key` = key),
                     path = sprintf("v1/author/%s", identifier))
  } else {
    res <- httr::GET(url = s2_api(),
                     path = sprintf("v1/author/%s", identifier))
  }

  if (status_code(res) == 200)
    return(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "utf-8")))

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")

  stop("HTTP status", status_code(res))

}

#'
#' This function retrieves Semantic Scholar data for
#' an author given the S2Author identifier
#' @param author_id string with author identifier
#' @param details one of "authors", "citations" or "references"
#' @param offset integer paging offset
#' @param limit integer paging length
#' @param fields extra fields to include, for example "title,authors"
#' @details
#' Example of Accessible Paper Identifiers:
#' - S2 Author ID : 1681232
#' @return list representing author object
#' @examples
#' \dontrun{
#'  author(1681232)
#'  author2(1681232, fields="affiliations,aliases,externalIds")
#'  author2(1681232, fields="paperId,externalIds", details = "papers", offset=0, limit = 100)
#'  }
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @export
author2 <- function(
  author_id, details=c("papers"), offset = 0, limit = 10, fields = NULL) {

  #https://api.semanticscholar.org/graph/v1/author/1681232?fields=affiliations,aliases,externalIds

  identifier <- author_id
  key <- cfg()$s2_key
  api <- s2_api()

  if (missing(details)) {
    qpath <- sprintf("graph/v1/author/%s", identifier)
  } else {
    qpath <- sprintf("graph/v1/author/%s/%s", identifier, match.arg(details))
  }

  if (!missing(fields)) {
    if (!missing(details) && !is.null(fields) && !validate_fields(fields, "author_papers"))
      stop("Invalid fields")
    if (missing(details) && !validate_fields(fields, "author"))
      stop("Invalid fields")
  }

  q <- list(offset = offset, limit = limit, fields = fields)

  if (!is.null(key) && nchar(key) > 10) {
    res <- httr::GET(url = api, httr::add_headers(`x-api-key` = key),
                     path = qpath,
                     query = q)
  } else {
    res <- httr::GET(url = s2_api(),
                     path = qpath,
                     query = q)
  }

  if (status_code(res) == 200) {
    res <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "utf-8")
    )
    # TODO: fix this
    #class(res[[details]]) <- c("tbl_df", "tbl", "data.frame")
    return(res)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")

  if (status_code(res) == 400)
    stop("HTTP status 400 Bad Query Parameters.")

  if (status_code(res) == 404)
    stop("HTTP status 404 Bad Identifier.")

  stop("HTTP status", status_code(res))

}

#' This function retrieves Semantic Scholar data for
#' a keyword search
#' @param keyword string with keywords to search for, supports AND and OR and
#' inclusion and exclusion of terms, for example "+Epidemic +Modeling +Canada -COVID"
#' @param offset integer paging offset
#' @param limit integer paging length
#' @param fields extra fields to include, for example "title,authors"
#' @return list representing paper objects
#' @examples
#' \dontrun{
#'  search_papers(keyword = "literature graph")
#'  }
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @export
search_papers <- function(keyword, offset = 0, limit = 10, fields = NULL) {
  #http://api.semanticscholar.org/graph/v1/paper/search?query=literature+graph
  #https://api.semanticscholar.org/graph/v1/author/1681232?fields=affiliations,aliases,externalIds

  q <- keyword
  key <- cfg()$S2_key
  api <- s2_api()

  if (!is.null(fields) && !missing(fields)) {
    if (!validate_fields(fields, "paper_search"))
      stop("Invalid fields requested.")
  }

  if (!is.null(key) && nchar(key) > 10) {
    res <- httr::GET(url = api, httr::add_headers(`x-api-key` = key),
      path = "graph/v1/paper/search",
      query = list(query = q, offset = offset, limit = limit, fields = fields))
  } else {
    res <- httr::GET(url = s2_api(),
      path = "graph/v1/paper/search",
      query = list(query = q, offset = offset, limit = limit, fields = fields))
  }

  if (status_code(res) == 200) {
    res <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "utf-8")
    )
    if (length(res$data) > 0) {
      class(res$data) <- c("tbl_df", "tbl", "data.frame")
    }
    return(res)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")

  if (status_code(res) == 400)
    stop("HTTP status 400 Bad Query Parameters.")

  stop("HTTP status", status_code(res))

}

#' This function retrieves details about a paper's authors
#' @param identifier string with keywords to search for
#' @param details one of "authors", "citations" or "references"
#' @param offset integer paging offset
#' @param limit integer paging length
#' @param fields extra fields to include, for example "title,authors"
#' @return list representing paper objects
#' @examples
#' \dontrun{
#'  paper2(identifier = "649def34f8be52c8b66281af98ae884c09aef38b")
#'  }
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @export
paper2 <- function(identifier, details=c("authors", "citations", "references"),
                             offset = 0, limit = 10, fields = NULL) {

  key <- cfg()$S2_key
  api <- s2_api()

  if (missing(details)) {
    qpath <- sprintf("graph/v1/paper/%s", identifier)
  } else {
    qpath <- sprintf("graph/v1/paper/%s/%s", identifier, match.arg(details))
  }

  if (!missing(details)) {
    if (!is.null(fields) && !validate_fields(fields, rel = paste0("paper_", details)))
      stop("Invalid fields.")
  } else if (!is.null(fields)) {
    if (!validate_fields(fields, rel = "paper"))
      stop("Invalid fields")
  }

  q <- list(offset = offset, limit = limit, fields = fields)

  if (!is.null(key) && nchar(key) > 10) {
    res <- httr::GET(url = api, httr::add_headers(`x-api-key` = key),
       path = qpath,
       query = q
    )
  } else {
    res <- httr::GET(url = s2_api(),
       path = qpath,
       query = q
    )
  }

  if (status_code(res) == 200) {
    res <- jsonlite::fromJSON(
      httr::content(res, as = "text", encoding = "utf-8")
    )
    # TODO: sometimes we have differently named slots...
    #class(res[[details]]) <- c("tbl_df", "tbl", "data.frame")
    return(res)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests (> 100 in 5 mins). Please wait 5 minutes.")

  if (status_code(res) == 400)
    stop("HTTP status 400 Bad Query Parameters.")

  if (status_code(res) == 404)
    stop("HTTP status 404 Bad Identifier.")

  stop("HTTP status", status_code(res))
}

author_fields <- function() {
  readLines(textConnection(
"authorId
externalIds
url
name
aliases
affiliations
homepage
homepage
paperCount
citationCount
hIndex
papers
papers.paperId
papers.externalIds
papers.url
papers.title
papers.abstract
papers.venue
papers.year
papers.referenceCount
papers.citationCount
papers.influentialCitationCount
papers.isOpenAccess
papers.fieldsOfStudy
papers.authors"))
}

author_papers_fields <- function() {
  readLines(textConnection(
    "paperId
      externalIds
      url
      title
      abstract
      venue
      year
      referenceCount
      citationCount
      influentialCitationCount
      isOpenAccess
      fieldsOfStudy
      authors
      citations
      citations.paperId
      citations.url
      citations.title
      citations.venue
      citations.year
      citations.authors
      references
      references.paperId
      references.url
      references.title
      references.venue
      references.year
      references.authors"))
}

paper_search_fields <- function() {
  readLines(textConnection("paperId
externalIds
url
title
abstract
venue
year
referenceCount
citationCount
influentialCitationCount
isOpenAccess
fieldsOfStudy
authors
authors.authorId
authors.name"))
}

paper_fields <- function() {
  readLines(textConnection(
"paperId
externalIds
url
title
abstract
venue
year
referenceCount
citationCount
influentialCitationCount
isOpenAccess
fieldsOfStudy
authors
authors.authorId
authors.externalIds
authors.url
authors.name
authors.aliases
authors.affiliations
authors.homepage
citations
citations.paperId
citations.url
citations.title
citations.venue
citations.year
citations.authors
references
references.paperId
references.url
references.title
references.venue
references.year
references.authors
embedding
tldr
"))
}

paper_authors_fields <- function() {
  readLines(textConnection("authorId
externalIds
url
name
aliases
affiliations
homepage
papers
papers.paperId
papers.externalIds
papers.url
papers.title
papers.abstract
papers.venue
papers.year
papers.referenceCount
papers.citationCount
papers.influentialCitationCount
papers.isOpenAccess
papers.fieldsOfStudy
papers.authors"))
}

paper_citations_fields <- function() {
  readLines(textConnection("contexts
intents
isInfluential
externalIds
url
title
abstract
venue
year
referenceCount
citationCount
influentialCitationCount
isOpenAccess
fieldsOfStudy
authors"))
}

paper_references_fields <- function() {
  readLines(textConnection("contexts
intents
isInfluential
externalIds
url
title
abstract
venue
year
referenceCount
citationCount
influentialCitationCount
isOpenAccess
fieldsOfStudy
authors"))
}

validate_fields <- function(fields,
  rel = c("author",
          "author_papers",
          "paper_search",
          "paper",
          "paper_authors",
          "paper_citations",
          "paper_references")) {

  type <- match.arg(rel)

  fn <- unlist(strsplit(fields, ","))

  is_ok <- function(x, y) all(x %in% y)

  fields <- switch(type,
         "author" = author_fields(),
         "author_papers" = author_papers_fields(),
         "paper" = paper_fields(),
         "paper_search" = paper_search_fields(),
         "paper_authors" = paper_authors_fields(),
         "paper_citations" = paper_citations_fields(),
         "paper_references" = paper_references_fields()
         )

  if (isTRUE(is_ok(fn, fields))) {
    return(invisible(TRUE))
  }

  idx <- which(fn %in% fields)
  valid <- fn[idx]
  invalid <- setdiff(fn, fields)
  message("Please provide comma separated (no spaces) string with fields")
  message("Valid: ", paste0(collapse = ",", valid))
  message("Invalid: ", paste0(collapse = ",", invalid))
  message("Available: ")
  print(setdiff(fields, fn))
  return(invisible(FALSE))
}

#' Valid fields available for use in API calls
#' @export
api_fields <- function() {
  list(
    author = author_fields(),
    author_papers = author_papers_fields(),
    paper_search = paper_search_fields(),
    paper_authors = paper_authors_fields(),
    paper_citations = paper_citations_fields(),
    paper_references = paper_references_fields()
  )
}

