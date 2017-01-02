#' getContainerContents -  Gets information about container cell contents.
#'
#' \code{getContainerlContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType Container type
#' @param containerBarcode container barcode
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains contents information.  This is list with the cell contents 
#' in cell number order. $response contains the entire http response.
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getContainerContents(login$coreApi,"VIAL","VIA9")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{getCellContents} - Gets information about container cell contents.

getContaierContents<-function (coreApi,containerType,containerBarcode,useVerbose = FALSE)
{


##need some code here  
  
list(entity=httr::content(response)$response$data,response=response)

}



