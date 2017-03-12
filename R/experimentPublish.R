#'experimentPublish Publishes an experiment.
#'
#' \code{experimentPublish} Publishes an experiment.
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
#' update<- CoreAPIV2::experimentPublish(login$coreApi,experimentType, exptbarcode,useVerbose = TRUE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{experimentPublish} - Publishes an experiment.





experimentPublish<- function(coreApi, experimentType, experimentBarcode,useVerbose = FALSE)

{

  
  sdk_url<-  CoreAPIV2::JSONbuildUrl(coreApi,special=NULL,useVerbose=useVerbose)
  
  cookie <- c(JSESSIONID = coreApi$jsessionId, AWSELB = coreApi$awselb )
  
#build request
  
  sdkCmd<-jsonlite::unbox("experiment-publish")
  
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
  
  
  response<-
    httr::POST(sdk_url,body = jsonlite::toJSON(request), encode="raw",
                            httr::add_headers(headers),
                    httr::verbose(data_out = useVerbose, data_in = useVerbose,
                    info = useVerbose, ssl = useVerbose)
     )
  
  

  list(entity=httr::content(response)$response$data,response=response)

}

