#' getCellContents -  Gets information about container cell contents.
#'
#' \code{getCellContents} Gets information about container cell contents.
#' @param coreApi coreApi object with valid jsessionid
#' @param containerBarcode container barcode
#' @param containerCellNum container cell number as a string
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains cell information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' cell<-CoreAPIV2::getCellContents(login$coreApi,"VIA9","1")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{getCellContents} - Gets information about container cell contents. This call uses the JSON API.





getCellContents<-function (coreApi, containerBarcode, containerCellNum,useVerbose = FALSE)
{

  sdkCmd<-jsonlite::unbox("get")

  data<-list()


  data[["cellRefs"]] <- list(c(list(cellNum = jsonlite::unbox(containerCellNum), containerRef = list(barcode =jsonlite::unbox(containerBarcode))) ) )


  responseOptions<-c("CONTEXT_GET","MESSAGE_LEVEL_WARN","INCLUDE_CONTAINER_CELL_CONTENTS")
  logicOptions<-list()
  typeParam <- jsonlite::unbox("CELL")


 request<-list(request=list(sdkCmd=sdkCmd,data=data,typeParam =typeParam,
                             responseOptions=responseOptions,
                             logicOptions=logicOptions))


response<- CoreAPIV2::JSONapiCall(coreApi,request,"json",useVerbose=useVerbose)


list(entity=httr::content(response)$response$data,response=response)

}



