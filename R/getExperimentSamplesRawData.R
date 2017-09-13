#' getExperimentSamplesRawData - Gets raw data for an experiment container.
#'
#'\code{getExperimentSamplesRawData }  Gets raw data for a experiment container identified by barcode.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param experimentType experiment type for sample
#'@param assayType  assay type for sample
#'@param experimentContainerBarcode experiment sample container of entity to get
#'@param dataName assay data name to retrive as configured in the assay.
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#'@return returns a list $entity contains data frame with derived experiment sample barcodes concentration, 
#'         and assay raw data. $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response<-  getExperimentSamplesRawData (login$coreApi,"ExperimentContainerBarcode",useVerbose = FALSE)
#' rawdata <- response$entity
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{ getExperimentSamplesRawData }   Gets raw data for a experiment container identified by barcode.



getExperimentSamplesRawData <-
  function (coreApi,
            experimentContainerBarcode,
            useVerbose = FALSE)
    
  {
    
    resource <- "RAW_DATA"
    

    query   <- paste0(
      
      "?$filter=EXPERIMENT_CONTAINER/Name%20eq%20'",
      experimentContainerBarcode,
         "'"
    )
    
    
    header <- c(Accept = "application/json")
    

    
    
    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    
    dataValues <-lapply(response$content,unlist)
    
    dataValues <- lapply(dataValues, function(x) {names(x) <- NULL
                         return(x) })
    
    dataValues <- t(matrix(unlist(dataValues), ncol = length(response$content), nrow =length(response$content[[1]]) ))
   
    colnames(dataValues) <- names(response$content[[1]])
    
    dataValues <- as.data.frame(dataValues)
    
    
    
    list(entity = dataValues, response = response$response)
    
  }
