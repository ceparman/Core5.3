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

  lheader <- c(Accept = "application/json;odata.metadata=full")
#get the container cells     
  
cells<- CoreAPIV2::apiGET(coreApi,resource = containerType,
                          query =paste0("('",containerBarcode,"')/REV_IMPL_CONTAINER_CELL") ,headers = lheader,
                          useVerbose=FALSE)
  
content<-list()

#build list of cell contents

for ( i in 1:length(cells$content$value))
{
  
cellId <-cells$content$value[[i]]$Id   
  
print(i)  
  
}





#list(entity=httr::content(response)$response$data,response=response)

}


#api<- CoreAPIV2::coreAPI("tests/testthat/test_environments/Test%205.2.postman_environment.json")

#con<- CoreAPIV2::authBasic(api,useVerbose=TRUE)

#CoreAPIV2::getContainerContents(con$coreApi,"VIAL","VIA1",useVerbose = FALSE)

#CoreAPIV2::getContainerContents(con$coreApi,"_384_WELL_PLATE","TE1",useVerbose = FALSE)


