#' setExperimentSamplesAssayFileData - puts file attached as assay data in an experiment. 
#'
#'\code{setExperimentSamplesAssayFileData }  puts file attached as assay data. 
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param assayType assay type to get
#'@param experimentSamplebarcode experiment sample barcode of entity to get
#'@param attrributeName  Name of the attibute that containts the file data
#'@param filePath path to file to upload
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity containsbinary object that in the file, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response<-  setExperimentSamplesAssayFileData(login$coreApi,"assayType","barcode","filepath","CI_FILE")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{ setExperimentSamplesAssayFileData } Puts file attached as assay data in an experiment 






setExperimentSamplesAssayFileData <-
  function (coreApi,
            assayType,
            experimentSamplebarcode,
            attributeName,
            filepath,
            useVerbose = FALSE)
    
  {
    #does the file exist
    
    if (!file.exists(filepath)) {
      stop({
        print("Unable to find file on local OS")
        print(filepath)
      },
      call. = FALSE)
      
    }
    
    
    #clean the name for ODATA
    
    
    
    resource <- paste0(CoreAPIV2::ODATAcleanName(assayType), "_DATA")
    resource <- CoreAPIV2::ODATAcleanName(resource)
    
    query   <- paste0(
      "('",
      experimentSamplebarcode,
      "')/",
      CoreAPIV2::attributeCleanName(attributeName),
      "/$value"
    )
    
    
    
    headers <- c(Accept = "image/png")
    
    
    
    
    
    body <-  httr::upload_file(filepath, type = "image/png")
    
    
    
    
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
        httr::with_verbose(httr::PUT(
          sdk_url,
          body = body,
          httr::add_headers(headers),
          httr::set_cookies(cookie)
        ))
      
    } else  {
      response <- httr::PUT(sdk_url,
                            body = body,
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
    
    
    
    
    
    
    list(entity = "", response = response)
    
  }



