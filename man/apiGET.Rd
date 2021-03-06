% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apiGET.R
\name{apiGET}
\alias{apiGET}
\title{apiGET - Do a get from the Core ODATA REST API.}
\usage{
apiGET(coreApi, resource, query, headers = NULL, special = NULL,
  useVerbose = FALSE)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{resource}{entity type to get}

\item{query}{query string}

\item{headers}{headers to be added to get}

\item{special}{passed to buildUrl for special sdk endpoints}

\item{useVerbose}{Use verbose communication for debugging}
}
\value{
Returns a list of length two. First the content, concatenated if a chunked response, and second the entire http response.
If chuncked for the content for the last chunk only.last
}
\description{
\code{apiGET} - Do a get from the Core ODATA REST API.
}
\details{
\code{apiGET}  Base call to Core REST API.
}
\examples{
\dontrun{
api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
login<- CoreAPIV2::authBasic(api)
response <-CoreAPIV2::apiGET(login$coreApi,"json",resource,query)
content <- response$content
error <- httr::http_error(response$response)
CoreAPIV2::logOut(login$coreApi)
}
}
\author{
Craig Parman ngsAnalytics, ngsanalytics.com
}
