#` updateEntityAttributes - Update entity attributes
#'
#'\code{updateEntityAttributes} Update entity assoications.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param updateValues vaules to update as list of value pairs
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::updateEntityAttributes(login$coreApi,"entityType","barcode",values)
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{updateEntityAttributes}  Update entity attributes.



updateEntityAttributes<-function (coreApi,entityType,barcode,updateValues,useVerbose=FALSE)

{
  
#clean the name for ODATA
  
 entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
 resource <- entityType
  
 query   <- paste0("('",barcode,"')")

# Get entityType
 
 entity <- CoreAPIV2::getEntityByBarcode(coreApi,entityType,barcode,fullMetadata = FALSE,useVerbose = TRUE)
 

 old_values<-entity$entity
 
 
 #check to see if all values to update are in the entity
 
 
 # replace values
 
 if ( table(names(updateValues)  %in% names(old_values))["TRUE"] != length(names(updateValues)) ) {
 
   stop(
     {print("Names of values to update don't match entity names ")
       print( names(updateValues) )
       print(names(old_values) )
     },
     call.=FALSE
   )
   
 
}
 
 namesToUpdate<- names(updateValues)
 for(i in 1:length(namesToUpdate))
   
 {
   old_values[[namesToUpdate[i]]]  <- updateValues[[i]]
   
   
 }
 
 
 
 
 body<-old_values   
 
 resource <- paste0(entityType)
 query <- paste0("('",barcode,"')")
 
 header<-c("Content-Type"="application/json","If-Match"="*")  
 
 #update record 
 
 
response<- CoreAPIV2::apiPUT(coreApi,resource = resource, query=query,body=body,encode="raw",
                              headers = header, useVerbose=useVerbose)
 
 
  

list(entity=httr::content(response),response=response)

}



