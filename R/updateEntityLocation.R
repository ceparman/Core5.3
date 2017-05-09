#` updateEntityLocation - Update entity location
#'
#'\code{updateEntityLocation}- Update entity location
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param locationBarcode loaction barcode
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::updateEntityLocation(login$coreApi,"entityType","barcode","locationBarcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{updateEntityLocation} - Update entity location



updateEntityLocation<-function (coreApi,entityType,barcode,locationBarcode,useVerbose=FALSE)

{
  
#clean the name for ODATA
  
 entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
 resource <- entityType
  
 query   <- paste0("('",barcode,"')")

# Get entityType
 
 entity <- CoreAPIV2::getEntityByBarcode(coreApi,entityType,barcode,fullMetadata = FALSE,useVerbose = useVerbose)
 

 old_values<-entity$entity
 
 


 old_values[["LOCATION@odata.bind"]] <-paste0("/LOCATION" ,"('",locationBarcode,"')")
   
   
 body<-old_values
 
 resource <- paste0(entityType)
 query <- paste0("('",barcode,"')")
 
 header<-c("Content-Type"="application/json","If-Match"="*")  
 
 #update record 
 
 
response<- CoreAPIV2::apiPUT(coreApi,resource = resource, query=query,body=body,encode="raw",
                              headers = header, useVerbose=useVerbose)
 
 
  

list(entity=httr::content(response),response=response)

}



