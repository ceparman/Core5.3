#' getCellContents -  Gets information about a single container cell contents.
#'
#' \code{getCellContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerCellId container cell number as a string
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getCellContents(login$coreApi,"1234233",fullMetadata = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getCellContents} -  Gets information about a single container cell contents.





getCellContents <-
  function (coreApi, containerCellId, useVerbose = FALSE)
  {
    #make sure containerCellNum is numeric
    
    containerCellId <- as.numeric(containerCellId)
    
    
    resource <- "CELL"
    
    query   <-
      paste0("(",
             containerCellId,
             ")?$expand=CONTENT($expand=IMPL_SAMPLE_LOT)")
    
    
    header <-
      c('Content-Type' = "application/json;odata.metadata=full", Accept = "application/json")
    
    
    
    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    
    
    response <-
      list(entity = response$content, response = response$response)
  }
