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
#'to an experiment.  

createExperimentContainer <-
  function (coreApi,
            experimentType,
            experimentBarcode,
            containerBarcode,
            body = NULL,
            useVerbose = FALSE)
    
  {
   

    experimentType <- CoreAPIV2::ODATAcleanName( experimentType)
    
    
    headers <-
      c('Content-Type' = "application/json;odata.metadata=full")
    
    
    resource <- paste0(experimentType,"_CONTAINER")
    
    body <- list("EXPERIMENT@odata.bind" = paste0("/",experimentType,"('",experimentBarcode,"')"),
                 "CONTAINER@odata.bind" =  paste0("/CONTAINER('",containerBarcode,"')" ) ) 
    
    response <-
      CoreAPIV2::apiPOST(
        coreApi,
        resource =  resource,
        body = jsonlite::toJSON( body,auto_unbox = TRUE),
        encode = "raw",
        headers = headers ,
        special = NULL,
        useVerbose = useVerbose
      )
    
    
    
    list(entity = httr::content(response), response = response)
    
    
  }
