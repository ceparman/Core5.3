#' getContainerContents -  Gets information about container cell contents.
#'
#' \code{getContainerContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerType container entity type
#' @param containerBarcode container barcode
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::ggetContainerContents(login$coreApi,"VIA9","1")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{getContainerContents} - Gets information about container cell contents. This call uses the JSON API.





getContainerContents<-function (coreApi, containerType,containerBarcode,useVerbose = FALSE)
{

  
  
  #clean the name for ODATA
  
  containerType <- CoreAPIV2::ODATAcleanName(entityType)
  
  sdkCmd<-jsonlite::unbox("get")

  data<-list()


  data <- list( entityRef = list(barcode =jsonlite::unbox(containerBarcode)))  


  responseOptions<-c("CONTEXT_GET","MESSAGE_LEVEL_WARN","INCLUDE_CONTAINER_CELL","INCLUDE_CONTAINER_CELL_CONTENTS")
  logicOptions<-list()
  
  typeParam <- jsonlite::unbox(containerType)

 request<-list(request=list(sdkCmd=sdkCmd,data=data,typeParam =typeParam,
                             responseOptions=responseOptions,
                             logicOptions=logicOptions))


response<- CoreAPIV2::JSONapiCall(coreApi,request,"json",useVerbose=useVerbose)


list(entity=httr::content(response)$response$data,response=response)

}



