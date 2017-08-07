#' getExperimentSamplesAssayFileData - Gets file attached as assay data. 
#'
#'\code{getExperimentSamplesAssayFileData }  Gets file attached as assay data.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param assayType assay type to get
#'@param experimentSamplebarcode experiment sample barcode of entity to get
#'@param attrributeName  Name of the attibute that containts the file data
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity containsbinary object that in the file, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response<-  getExperimentSamplesAssayFileData(login$coreApi,"assayType","barcode","CI_FILE")
#' writeBin(response$entity, "myfile.png")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{ getExperimentSamplesAssayFileData }  Gets file attached as assay data. 
#'Use getExperimentSamplesAssayData for non-file data.


getExperimentSamplesAssayFileData <-
  function (coreApi,
            assayType,
            experimentSamplebarcode,
            attributeName,
            useVerbose = FALSE)
    
  {
    #clean the name for ODATA
    
    
    
    resource <- paste0(CoreAPIV2::ODATAcleanName(assayType), "_DATA")
    
    
    query   <- paste0(
      "('",
      experimentSamplebarcode,
      "')/",
      CoreAPIV2::attributeCleanName(attributeName),
      "/$value"
    )
    
    
    
    headers <- c(Accept = "image/png")
    
    
    
    
    resource <- CoreAPIV2::ODATAcleanName(resource)
    
    sdk_url <-
      CoreAPIV2::buildUrl(
        coreApi,
        resource = resource,
        query = query,
        special = NULL,
        useVerbose = useVerbose
      )
    
    
    cookie <-
      c(JSESSIONID = coreApi$jsessionId,
        AWSELB = coreApi$awselb)
    
    
    
    if (useVerbose) {
      response <-
        httr::with_verbose(httr::GET(sdk_url, httr::add_headers(headers)),
                           httr::set_cookies(cookie))
      
    } else  {
      response <- httr::GET(sdk_url,
                            httr::add_headers(headers),
                            httr::set_cookies(cookie))
      
    }
    
    #check for general HTTP error in response
    
    if (httr::http_error(response)) {
      stop({
        print("API call failed")
        print(httr::http_status(response))
      },
      call. = FALSE)
      
    }
    
    #not sure what is going to happen if file is returned chunked
    
    
    bin <- httr::content(response, "raw")
    
    
    
    list(entity = bin, response = response)
    
  }
