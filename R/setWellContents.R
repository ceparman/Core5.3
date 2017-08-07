#' setWellContents -  Puts a sample lot in a container well.
#'
#' \code{setWellContents} Puts a sample lot in a container well.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container type
#' @param containerBarcode container barcode
#' @param containerWellNum container well number
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
#' cell<- updateCellContents<-(coreApi, containerType,containerBarcode, containerCellNum,
#'                            sampleLotType,sampleLotBarcode, amount, amountUnit, concentration,
#'                            concentrationUnit,useVerbose = FALSE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{setWellContents} - Puts a cell lot in a container well.






setWellContents <-
  function (coreApi,
            containerType,
            containerBarcode,
            containerWellNum,
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
    
    
    containerWellNum <- as.numeric(containerWellNum)
    
    #first get the cellID for the well
    
    cellID <-
      getContainerCellIds(coreApi, containerType, containerBarcode, useVerbose = FALSE)$entity[containerWellNum]
    
    
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
          cellId = jsonlite::unbox(cellID),
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
