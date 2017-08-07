#' setCellContents -  Puts a sample lot in a container Cell.
#'
#' \code{setCellContents} Puts a sample lot in a container cell.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container type
#' @param containerBarcode container barcode
#' @param containerCellId container cell id
#' @param sampleLotType sample lot type
#' @param sampleLotBarcode barcode of lot to add to cell
#' @param amount amount to add (numeric)
#' @param amountUnit units
#' @param concentration (numeric)
#' @param concentrationUnit concentration units
#' @param useVerbose use verbose communications for debugging
#' @export
#' @return RETURN returns a list $entity contains updated container
#'         information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<- updateCellContents<-(coreApi, containerType,containerBarcode, containerCellId,
#'                            sampleLotType,sampleLotBarcode, amount, amountUnit, concentration,
#'                            concentrationUnit,useVerbose = FALSE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{setCellContents} - Puts a cell lot in a container cell.






setCellContents <-
  function (coreApi,
            containerType,
            containerBarcode,
            containerCellId,
            sampleLotType,
            sampleLotBarcode,
            amount,
            amountUnit,
            concentration,
            concentrationUnit,
            useVerbose = FALSE)
  {
    #clean the name for ODATA
    
    containerType <- CoreAPIV2::ODATAcleanName(containerType)
    
    
    containerCellId <- as.numeric(containerCellId)
    
    
    
    #get ID for lot number
    lotID <-
      CoreAPIV2::getEntityByBarcode(
        coreApi,
        entityType = sampleLotType,
        barcode = sampleLotBarcode,
        
        fullMetadata = FALSE,
        useVerbose = useVerbose
      )$entity$Id
    
    body <- list()
    
    
    cells <-
      list(c(
        list(
          cellId = jsonlite::unbox(containerCellId),
          amount = jsonlite::unbox(amount),
          amountUnit = jsonlite::unbox(amountUnit),
          
          
          
          contents = list(c(
            list(
              lotId = jsonlite::unbox(lotID),
              concentration = jsonlite::unbox(concentration),
              concentrationUnit = jsonlite::unbox(concentrationUnit)
            )
          ))
        )
      ))
    
    
    body[["cells"]] <- cells
    
    query <-
      paste0("CONTAINER('",
             containerBarcode,
             "')/pfs.Container.SetCellContents")
    
    
    header <-
      c("Content-Type" = "application/json;metadata=minimal", Accept = "application/json")
    
    response <-
      CoreAPIV2::apiPOST(
        coreApi,
        resource = query,
        body = body,
        encode = "json",
        headers = header,
        useVerbose = useVerbose
      )
    
    
    
    list(entity = httr::content(response), response = response)
    
  }
