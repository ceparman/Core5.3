#' getEntityByBarcode - Get an entity from the Core LIMS using the ODATA API by barcode.
#'
#'\code{getEntityByBarcode} get an entity from the LIMS by barcode
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param fullMetadata - get full metadata
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::getEntityByBarcode(login$coreApi,"entityType","barcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{getEntityByBarcode}  Get an entity from the Core LIMS using the ODATA API by barcode.



getEntityByBarcode<-function (coreApi,entityType,barcode,fullMetadata=TRUE,useVerbose=FALSE)

{
  
#clean the name for ODATA
  
 entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
 resource <- entityType
  
 query   <- paste0("('",barcode,"')")

 
 # if(fullMetadata){ header<-httr::add_headers(Accept="application/json;odata.metadata=full")
 #   } else {
 #   header<-httr::add_headers(Accept="application/json;odata.metadata=minimal")  
 #   
 # }


 
 if(fullMetadata){ header<-c(Accept="application/json;odata.metadata=full")
 } else {
   header<-c(Accept="application/json;odata.metadata=minimal")  
   
 }
 
 
 out <- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,headers = header,useVerbose=useVerbose)

  

list(entity=out$content,response=out$response)

}



