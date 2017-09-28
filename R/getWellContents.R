#' getWellContents -  Gets information about a single container well contents.
#'
#' \code{getWellContents} Gets information about container well contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container type
#' @param containerBarcode container barcode
#' @param containerWellNum container well number as a string
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getWellContents(login$coreApi,"VIA9","1",fullMetadata = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getWellContents} -  Gets information about a single container well contents.




getWellContents <-
  function (coreApi,
            containerType,
            containerBarcode,
            containerWellNum,
            useVerbose = FALSE)
  {
    #clean the name for ODATA
    
    resource <- CoreAPIV2::ODATAcleanName(containerType)
    
    #make sure containerWellNum is numeric
    
    containerWellNum <- as.numeric(containerWellNum)
    
    #first get the cellID for the well
    
    id <-
      getContainerCellIds(coreApi, containerType, containerBarcode, useVerbose = FALSE)$entity[containerWellNum]
    
    
    resource <- "CELL"
    
    query   <-
      paste0("(", id, ")?$expand=CELL_CONTENTS($expand=SAMPLE_LOT)")
    
    
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



