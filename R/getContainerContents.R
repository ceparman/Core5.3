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

getContainerContents<-function (coreApi,containerType,containerBarcode,useVerbose = FALSE)
{

#get the container  
#container<-CoreAPIV2::getEntityByBarcode(con$coreApi,entityType = "_384_WELL_PLATE",barcode='TE1')

#get thecontainer cells   

  cells<- CoreAPIV2::apiGET(con$coreApi,resource = "_384_WELL_PLATE",query = "('TE1')/REV_IMPL_CONTAINER_CELL" )  
    
list(entity=httr::content(response)$response$data,response=response)

}



