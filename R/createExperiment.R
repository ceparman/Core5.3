#' createExperiment - Create a new instance of an experiment.
#' 
#' \code{createExperiment} Creates a new instance of an entity.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param assayType assay type
#' @param assayBarcode assay barcode
#' @param protocolType protocol type
#' @param protocolBarcode protocol barcode
#' @param body values for experiment attributes and associations as a  list of key-values pairs
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::createExperiment(login$coreApi,"Experiment_Type","Assaybarcode","Protocolbarcode")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{createExperiment} Creates a new experimen.

createExperiment<-function (coreApi,experimentType,assayType,assayBarcode,protocolType,
                            protocolBarcode,body=NULL,useVerbose=FALSE)

{

  #clean the names for ODATA
  
  experimentType<- CoreAPIV2::ODATAcleanName(experimentType)
  assayType<- CoreAPIV2::ODATAcleanName(assayType)
  protocolType<- CoreAPIV2::ODATAcleanName(protocolType)
  
  assayRef<- list('EXPERIMENT_ASSAY@odata.bind' = paste0("/",assayType,"('",assayBarcode,"')"))
  
  protocolRef<- list('EXPERIMENT_PROTOCOL@odata.bind' = paste0("/",protocolType,"('",protocolBarcode,"')"))
  
  fullBody<- jsonlite::toJSON(c(body,assayRef,protocolRef),auto_unbox = TRUE)
  
  
  
  
 headers <- c('Content-Type' = "application/json")
 
response<-CoreAPIV2::apiPOST(coreApi,resource=experimentType,body=fullBody,encode = "json", headers =headers ,special=NULL,useVerbose=useVerbose)
 


list(entity=httr::content(response),response=response)

}


