#' getExperimentSamplesIntermediateData - Gets intermediate data for an experiment sample.
#'
#'\code{getExperimentSamplesIntermediateData}  Gets intermediate data for a experiment sample identified by barcode.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param experimentType experiment type for sample
#'@param assayType  assay type for sample
#'@param experimentSamplebarcode experiment sample barcode of entity to get
#'@param dataName assay data name to retrive as configured in the assay.
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#'@return returns a list $entity contains data frame with derived experiment sample barcodes concentration, 
#'         and assay raw data. $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response<-  getExperimentSamplesIntermediateData(login$coreApi,"ExperimentType","assayType","dataName","barcode")
#' rawdata <- response$entity
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getExperimentSamplesIntermediateData}   Gets raw data for a experiment sample identified by barcode.



getExperimentSamplesIntermediateData <-
  function (coreApi,
            experimentType,
            assayType,
            dataName,
            experimentSamplebarcode,
            useVerbose = FALSE)
    
  {
    #clean the name for ODATA
    experimentType <- CoreAPIV2::ODATAcleanName(experimentType)
    assayType   <-   CoreAPIV2::ODATAcleanName(assayType)
    dataName <- CoreAPIV2::attributeCleanName(dataName)
    resource <- paste0(experimentType, "_SAMPLE")
    
    
    query   <- paste0(
      "('",
      experimentSamplebarcode,
      "')?$expand=ASSAY_DATA/pfs.ASSAY_DATA,DERIVED_FROM",
      "($expand=INTERMEDIATE_ASSAY_DATA/pfs.INTERMEDIATE_",
      assayType,
      "_DATA)"
    )
    
    
    header <- c(Accept = "application/json; odata.metadata=minimal")
    
    
    
    
    response <-
      CoreAPIV2::apiGET(
        coreApi,
        resource = resource,
        query = query,
        headers = header,
        useVerbose = useVerbose
      )
    
    
    derivedSamples <- response$content$DERIVED_FROM
    
    barcodes <- unlist(lapply(derivedSamples, function(x)
      x$Barcode))
    
    accept <-  unlist(lapply(derivedSamples, function(x)
      x$CI_ACCEPT))
    
    concentration <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_NM))
          return("")
        else
          return(x$CI_CONC_NM)
      }))
    
    
    concUnit <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CONC_UNIT))
          return("")
        else
          return(x$CI_CONC_UNIT)
      }))
    
    
    time <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_TIME))
          return("")
        else
          return(x$CI_TIME)
      }))
    
    cell <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(x$CI_CELL))
          return("")
        else
          return(x$CI_CELL)
      }))
    
    id <-
      unlist(lapply(derivedSamples, function(x) {
      
          return(x$Id)
      }))
    
    
    
    dataValues <-
      unlist(lapply(derivedSamples, function(x) {
        if (is.null(eval(parse(text= 
                               paste0("x$INTERMEDIATE_ASSAY_DATA$'",dataName,"'")
            ) ))
        )
          return("")
        else
          return(eval(parse(text= 
                              paste0("x$INTERMEDIATE_ASSAY_DATA$'",dataName,"'")
                          ) 
                      )
          )
      }))
    
    
    
    entity <-
      data.frame (
        barcodes = barcodes,
        id = id,
        concentration = concentration,
        concUnit = concUnit,
        time = time,
        cell = cell,
        accept = accept
      )
    
    entity<- entity[order(entity$concentration),]
    
    ######## can have different intermediate data must build columns
    
    
    eval(parse(text = paste0("entity$'",dataName,"'<-dataValues"  )    ))
      
    
    
    list(entity = entity, response = response$response)
    
  }
