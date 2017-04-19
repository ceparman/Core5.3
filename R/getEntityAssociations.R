#` getEntityAssociations - Update entity attributes
#'
#'\code{getEntityAssociation} Get assoication for a context
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param context association context
#'@param fullMetadata - get full metadata
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::getEntityAssociation(login$coreApi,"entityType","barcode",values)
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{getEntityAssociation}  Get assoication for a context



getEntityAssociation<-function (coreApi,entityType,barcode,context,fullMetadata = TRUE, useVerbose=FALSE)

{
  
#clean the name for ODATA
  
 entityType <- CoreAPIV2::ODATAcleanName(entityType)
 context <-    CoreAPIV2::ODATAcleanName(context)
 
 resource <- entityType
  
 query   <- paste0("('",barcode,"')/",context)

 
 
 if(fullMetadata){ header<-c(Accept="application/json;odata.metadata=full")
 } else {
   header<-c(Accept="application/json;odata.metadata=minimal")  
   
 }
 
 
 out <- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,headers = header,useVerbose=useVerbose)
 
 
 
 list(entity=out$content,response=out$response)
 

}



