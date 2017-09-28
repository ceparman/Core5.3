#' getContainerContents -  Gets information about container contents.
#'
#' \code{getContainerContents} Gets information about container contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container entity type
#' @param containerBarcode container barcode
#' @param fullMetadata return full metadata
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getContainerContents(login$coreApi,"VIA9","1")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getContainerContents} - Gets information about container cell contents. 

getContainerContents <-
  function(coreApi,
           containerType,
           containerBarcode,
           fullMetadata = TRUE,
           useVerbose = FALSE)
  {
    #clean the name for ODATA
    
    resource <- CoreAPIV2::ODATAcleanName(containerType)
    
    
    
    query   <-
      paste0(
        "('",
        containerBarcode,
        "')?$expand=CELLS($expand=CELL_CONTENTS($expand=SAMPLE_LOT))"
      )
    
    
    
    
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
