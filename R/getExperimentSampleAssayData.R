#' getExperimentSamplesAssayData - Gets assay data for an experiment sample.
#'
#'\code{getExperimentSamplesAssayData }  Gets assay data for a experiment sample identified by barcode.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param assayType assay type to get
#'@param experimentSamplebarcode experiment sample barcode of entity to get
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' experiment<-  getExperimentSamplesAssayData (login$coreApi,"assayType","barcode")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{ getExperimentSamplesAssayData }  Gets experiment samples from experiment identified by experiment barcode. 
#'Does not retieve files attached as data. Use getExperimentSampleAssayFileData to retrieve assay data that is a file.





getExperimentSamplesAssayData <-
  function (coreApi,
            assayType,
            experimentSamplebarcode,
            useVerbose = FALSE)
    
  {
    #clean the name for ODATA
    
    resource <- CoreAPIV2::ODATAcleanName("EXPERIMENT_SAMPLE")
    
    assayType <- CoreAPIV2::ODATAcleanName(assayType)
    
    query   <- paste0("('",
                      experimentSamplebarcode,
                      "')/ASSAY_DATA/pfs.",
                      assayType,
                      "_DATA")
    
    
    header <- c(Accept = "application/json")
    
    
    
    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    
    
    
    list(entity = response$content, response = response$response)
    
  }
