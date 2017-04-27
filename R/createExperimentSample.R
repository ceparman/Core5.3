#' createExperimentSample - Create a new experiment sample from a sample lot.
#' 
#' \code{createExperimentSample} Creates a new instance of an entity.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param experimentBarcode experiment barcode
#' @param sampleLotBarcode barcode of sample to add to experiment 
#' @param body values for sample attributes as a  list of key-values pairs
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::createExperimentSample(login$coreApi,"Experiment_Type",
#'        "Assaybarcode","ProtocolBarcode")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{createExperimentSample} Creates a new experiment sample fomr a sample lot.

createExperimentSample<-function (coreApi,experimentType,experimentBarcode,
                                  sampleLotBarcode,body=NULL,useVerbose=FALSE)

{

  #clean the names for ODATA
  
  experimentType<- CoreAPIV2::ODATAcleanName(experimentType)
  
  experimentSampleType <- paste0(experimentType,"_SAMPLE")
  
  exptRef<- list('EXPERIMENT@odata.bind' = paste0("/",experimentType,"('",experimentBarcode,"')"))
  
  entityRef<- list('ENTITY@odata.bind' = paste0("/ENTITY('",sampleLotBarcode,"')"))
  
  fullBody<- jsonlite::toJSON(c(body,exptRef,entityRef),auto_unbox = TRUE)
  
  

 headers <- c('Content-Type' = "application/json;odata.metadata=full")
 
 response<-CoreAPIV2::apiPOST(coreApi,resource=experimentSampleType,body=fullBody,encode = "json",
                              headers =headers ,special=NULL,useVerbose=useVerbose)
 


list(entity=httr::content(response),response=response)

}


