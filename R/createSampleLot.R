#' createSampleLot - Creates a lot of a sample.
#'
#' \code{createSampleLot} Creates a sinple lot of a sample. ASSUMES SAMPLE LOT ENTITY NAME IS OF THE FORM SAMPLENAME_LOT
#' @param coreApi coreApi object with valid jsessionid
#' @param sampleType sample type to create the lot of
#' @param sampleBarcode parent sample barcode
#' @param body attributes as list of key-values pairs (optional)
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::createSampleLot(login$coreApi,"Sample_Name")
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{createSampleLot} Creates a new sample lot using the parent sample barcode




createSampleLot<-function (coreApi,sampleType,sampleBarcode,body=NULL,
                           useVerbose=FALSE)
{

  #clean the name for ODATA
  
  sampleType <- CoreAPIV2::ODATAcleanName(sampleType)
  
  lotName <- paste0(sampleType,"_LOT")
  
  
  
  lotRef<- list('IMPL_LOT_SAMPLE@odata.bind' = paste0("/",sampleType,"('",sampleBarcode,"')"))
  
  fullBody<- jsonlite::toJSON(c(body,lotRef),auto_unbox = TRUE)

  headers <- c('Content-Type' = "application/json;odata.metadata=full",accept= "application/json")
  
  response<-CoreAPIV2::apiPOST(coreApi,resource=lotName,body=fullBody,encode = "json",headers =headers,special=NULL,useVerbose=useVerbose)
  


list(entity=httr::content(response),response=response)

}


