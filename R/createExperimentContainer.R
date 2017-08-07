#' createExperimentContainer- Creates a new experiment container by adding an exiting container to an experiment.
#' 
#' \code{createExperimentContainer} Creates a new experiment container by adding an exiting container to an experiment.
#' @param coreApi coreApi object with valid jsessionid
#' @param experimentType experiment type to get as character string
#' @param experimentBarcode experiment barcode
#' @param containerBarcode barcode of container to add to experiment 
#' @param body values for sample attributes as a  list of key-values pairs (not user in this json version)
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::createExperimentContainer(login$coreApi,"Experiment_Type",
#'      "ExperimentBarCode","Containerbarcode")
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{createExperimentContainer}Creates a new experiment container by adding an exiting container 
#'to an experiment.  Uses the JSON API not the ODATA interface.

createExperimentContainer <-
  function (coreApi,
            experimentType,
            experimentBarcode,
            containerBarcode,
            body = NULL,
            useVerbose = FALSE)
    
  {
    sdkCmd <- jsonlite::unbox("update-experiment-containers")
    
    data <- list()
    
    data[["containerRefs"]] <-
      list(c(list(barcode = jsonlite::unbox(containerBarcode))))
    
    data[["entityRef"]] <-
      list(barcode = jsonlite::unbox(experimentBarcode))
    
    
    
    responseOptions <-
      c("CONTEXT_GET",
        "MESSAGE_LEVEL_WARN",
        "INCLUDE_EXPERIMENT_CONTAINER")
    logicOptions <- list()
    typeParam <- jsonlite::unbox(experimentType)
    
    
    
    request <-
      list(
        request = list(
          sdkCmd = sdkCmd,
          data = data,
          typeParam = typeParam,
          responseOptions = responseOptions,
          logicOptions = logicOptions
        )
      )
    
    
    
    headers <- c(
      'Content-Type' = "application/json",
      Accept = "*/*",
      Cookie = paste0("AWSELB=", coreApi$awselb)
    )
    
    
    
    response <-
      CoreAPIV2::apiPOST(
        coreApi,
        resource = NULL,
        body = jsonlite::toJSON(request),
        encode = "raw",
        headers = headers,
        special = "json",
        useVerbose = useVerbose
      )
    
    
    list(entity = httr::content(response), response = response)
    
    
    
    
    #   #clean the names for ODATA
    #
    #   experimentType<- CoreAPIV2::ODATAcleanName(experimentType)
    #
    #   experimentContainerType <- paste0(experimentType,"_CONTAINER")
    #
    #   exptRef<- list('CONTAINER_EXPERIMENT@odata.bind' = paste0("/",experimentType,"('",experimentBarcode,"')"))
    #
    #   containerRef<- list('CONTAINER@odata.bind' = paste0("/CONTAINER('",containerBarcode,"')"))
    #
    #   fullBody<- jsonlite::toJSON(c(body,exptRef,containerRef),auto_unbox = TRUE)
    #
    #
    #
    #  headers <- c('Content-Type' = "application/json;odata.metadata=full")
    #
    #  response<-CoreAPIV2::apiPOST(coreApi,resource=experimentContainerType,body=fullBody,encode = "json",
    #                               headers =headers ,special=NULL,useVerbose=useVerbose)
    #
    #
    #
    # list(entity=httr::content(response),response=response)
    
  }
