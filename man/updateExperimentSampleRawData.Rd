% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateExperimentSampleRawData.R
\name{updateExperimentSampleRawData}
\alias{updateExperimentSampleRawData}
\title{updateExperimentSampleRawData - Update experiment sample raw data.}
\usage{
updateExperimentSampleRawData(coreApi, experimentContainerBarcode, cellNum,
  values, useVerbose = FALSE)
}
\arguments{
\item{coreApi}{coreApi object with valid jsessionid}

\item{experimentContainerBarcode}{User provided barcode as a character string}

\item{cellNum}{cell (well) number of container}

\item{values}{assay attributes as a list of key-values pairs}

\item{useVerbose}{Use verbose communication for debugging}
}
\value{
RETURN returns a list $entity contains entity information,
       $response contains the entire http response
}
\description{
\code{updateExperimentSampleRawData} Update experiment sample assay raw data.
}
\details{
\code{updateExperimentSampleRawData} Update experiment sample raw data.
}
\examples{
\dontrun{
api<-CoreAPI("PATH TO JSON FILE")
login<- CoreAPIV2::authBasic(api)
response<-updateExperimentSampleRawData(login$coreApi,"contBarcode",cellNum=1,
                   values = list(DATA_VALUE = 100 ,CI_ACCEPT = FALSE)

updatedEntity <- response$entity
CoreAPIV2::logOut(login$coreApi ) response<- CoreAPI::authBasic(coreApi)
}
}
\author{
Craig Parman ngsAnalytics, ngsanalytics.com
}
