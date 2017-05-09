#` updateEntityProject - Update entity project association
#'
#'\code{updateEntityProject}- Update entity project association
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param projectBarcode one or more project barcodes to associate to the entity 
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::updateEntityProject(login$coreApi,"entityType","barcode","locationBarcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{updateEntityProject} - Update entity project associations.  Does not perserve current associations.



updateEntityProject<-function (coreApi,entityType,barcode,projectBarcodes,useVerbose=FALSE)

{
  
#clean the name for ODATA
  
 entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
 resource <- entityType
  
 query   <- paste0("('",barcode,"')")

# Get entityType
 
 entity <- CoreAPIV2::getEntityByBarcode(coreApi,entityType,barcode,fullMetadata = FALSE,useVerbose = useVerbose)
 

 old_values<-lapply(entity$entity, function(x) if((length(x) <= 1)) jsonlite::unbox(x) else x)
 
 
 
 for (i in 1:length(projectBarcodes))
{

 projectBarcodes[i] <- paste0("/PROJECT" ,"('",projectBarcodes[i],"')")
   
 }
 
 old_values[["PROJECT@odata.binding"]] <- projectBarcodes
  
 
 body<-old_values
 
 resource <- paste0(entityType)
 query <- paste0("('",barcode,"')")
 
 header<-c("Content-Type"="application/json","If-Match"="*")  
 
 #update record 
 
 
response<- CoreAPIV2::apiPUT(coreApi,resource = resource, query=query,body=body,encode="raw",
                              headers = header, useVerbose=useVerbose, unbox=FALSE)
 
 

}



