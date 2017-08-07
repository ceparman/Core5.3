#' getContainerCellIds -  Gets cell ids for a container
#'
#' \code{getContainerCellIds} Gets information about container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container entity type
#' @param containerBarcode container barcode
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns $entity a array of cell IDs and  $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cellIDs<-CoreAPIV2::getContainerCellIds(login$coreApi,"384 WELL PLATE","TE1")$entity
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getContainerCellIds} -  Gets cell ids for a container



getContainerCellIds <-
  function(coreApi,
           containerType,
           containerBarcode,
           useVerbose = FALSE)
  {
    #clean the name for ODATA
    
    resource <- CoreAPIV2::ODATAcleanName(containerType)
    
    
    
    query   <-
      paste0("('",
             containerBarcode,
             "')?$expand=REV_IMPL_CONTAINER_CELL")
    
    
    
    
    header <- c(Accept = "application/json;odata.metadata=minimal")
    
    
    
    out <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    cells <-
      unlist(lapply(out$content$REV_IMPL_CONTAINER_CELL, function(x)
        x$Id))
    
    list(entity = cells, response = out$response)
    
  }
