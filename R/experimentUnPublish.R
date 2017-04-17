#'experimentUnPublish UnPublishes an experiment.
#'
#' \code{experimentUnPublish} UnPublishes an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment entity type
#' @param experimentBarcode barcode of the experiment
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains updated experiment information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' update<- CoreAPIV2::experimentUnPublish(login$coreApi,experimentType, exptbarcode,useVerbose = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{experimentUnPublish} - UnPublishes an experiment.





experimentUnPublish<- function(coreApi, experimentType, experimentBarcode,useVerbose = FALSE)

{

  
  
#build request
  
  sdkCmd<-jsonlite::unbox("experiment-unpublish")
  
  data<-list()
  

  
  data[["entityRef"]] <- list(barcode =jsonlite::unbox(experimentBarcode))
  
  
  
  responseOptions<-c("CONTEXT_GET","MESSAGE_LEVEL_WARN")
  logicOptions<-c("EXECUTE_TRIGGERS")
  typeParam <- jsonlite::unbox(experimentType)
  
  
  
  request<-list(request=list(sdkCmd=sdkCmd,data=data,typeParam =typeParam,
                             responseOptions=responseOptions,
                             logicOptions=logicOptions))
  

   headers <- c('Content-Type' = "application/json",Accept= "*/*",
               Cookie = paste0("AWSELB=",coreApi$awselb))
  
   response<-  CoreAPIV2::apiPOST(coreApi,resource=NULL,body=jsonlite::toJSON(request), encode="raw",
                                  headers=headers,special="json",useVerbose=useVerbose)

  

  list(entity=httr::content(response)$response$data,response=response)

}

