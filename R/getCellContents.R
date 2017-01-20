#' getCellContents -  Gets information about container cell contents.
#'
#' \code{getCellContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param cellId Cell ID returned in $ID slot of contaner cell, not to be confused with the content ID
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains contents information.  This is list with the cell contents 
#' in cell number order. $response contains the entire http response.
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getCellContents(login$coreApi,cellID,useVerbose = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{getCellContents} - Gets information about container cell contents.

getCellContents<-function (coreApi,cellId,useVerbose = FALSE)
{
  

header <- c(Accept = "application/json;odata.metadata=full")
 


content<- CoreAPIV2::apiGET(coreApi,resource = "CELL", query = paste0("(",cellId,")/CONTENT"),headers = header,
                            useVerbose=FALSE)


contentId <- as.character(content$content$value[[1]]$Id)

lotQuery <- paste0("(",contentId,")/IMPL_SAMPLE_LOT")


response<-CoreAPIV2::apiGET(coreApi,resource = "CELL_CONTENT", query = lotQuery,headers = header,
                         useVerbose=useVerbose)
  

#list(entity=httr::content(response),response=response)

response
}



