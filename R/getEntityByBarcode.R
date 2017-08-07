#' getEntityByBarcode - Get an entity by barcode from the Core LIMS using the ODATA API.
#'
#'\code{getEntityByBarcode} get an entity from the LIMS by barcode
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param fullMetadata - get full metadata, default is TRUE 
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
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getEntityByBarcode}  Get an entity by barcode from the Core LIMS using the ODATA API.



getEntityByBarcode <-
  function (coreApi,
            entityType,
            barcode,
            fullMetadata = TRUE,
            useVerbose = FALSE)
    
  {
    #clean the name for ODATA
    
    entityType <- CoreAPIV2::ODATAcleanName(entityType)
    
    resource <- entityType
    
    query   <- paste0("('", barcode, "')")
    
    
    
    
    if (fullMetadata) {
      header <- c(Accept = "application/json;odata.metadata=full")
    } else {
      header <- c(Accept = "application/json;odata.metadata=minimal")
      
    }
    
    
    out <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    
    
    list(entity = out$content, response = out$response)
    
  }
