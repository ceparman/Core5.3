#' getExperimentSamplesRawData - Gets raw data for an experiment sample.
#'
#'\code{getExperimentSamplesAssayData }  Gets raw data for a experiment sample identified by barcode.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param experimentType experiment type for sample
#'@param assayType  assay type for sample
#'@param experimentSamplebarcode experiment sample barcode of entity to get
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http call
#'@return returns a list $entity contains data frame with derived experiment sample barcodes concentration, and %I, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' experiment<-  getExperimentSamplesRawData (login$coreApi,"ExperimentType","assayType",barcode")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{ getExperimentSamplesRawData }   Gets raw data for a experiment sample identified by barcode.





getExperimentSamplesRawData <-function (coreApi,experimentType,assayType,experimentSamplebarcode,useVerbose=FALSE)

{

  
  #clean the name for ODATA
  experimentType <-CoreAPIV2::ODATAcleanName(experimentType)
  assayType   <-   CoreAPIV2::ODATAcleanName(assayType)
  
  resource <- paste0(experimentType,"_SAMPLE")
  
  
  query   <- paste0("('",experimentSamplebarcode,
                    "')?$expand=ASSAY_DATA/pfs.ASSAY_DATA,DERIVED_FROM",
                    "($expand=INTERMEDIATE_ASSAY_DATA/pfs.INTERMEDIATE_",
                    assayType,
                    "_DATA)"
                    )
  
  
  header<-c(Accept = "application/json; odata.metadata=minimal")  
  
 
  
  
  response <- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,headers = header,
                                useVerbose=useVerbose)
  
  
  derivedSamples <- response$content$DERIVED_FROM
  
  barcodes <- unlist(lapply(derivedSamples, function(x) x$Barcode))
  
  concentration <-  unlist(lapply(derivedSamples, function(x){ if(is.null(x$CI_CONC_NM)) return("") else return(x$CI_CONC_NM) }  ))

  
  concUnit <- unlist(lapply(derivedSamples, function(x){ if(is.null(x$CI_CONC_UNIT)) return("") else return(x$CI_CONC_UNIT) }  ))

  
  time <- unlist(lapply(derivedSamples, function(x){ if(is.null(x$CI_TIME)) return("") else return(x$CI_TIME) }  ))
  
  cell <- unlist(lapply(derivedSamples, function(x){ if(is.null(x$CI_CELL)) return("") else return(x$CI_CELL) }  ))

  entity <- data.frame (barcodes=barcodes,concentration = concentration, concUnit = concUnit,
                        time = time, cell = cell)
  
  ######## can have different intermediate data must build columns
  
  
  
  dataNames <- names(derivedSamples[[1]]$INTERMEDIATE_ASSAY_DATA)[-1]
  
  dataNames <- gsub("^[_$ ]","",dataNames)
  
  variableNames <-names(derivedSamples[[1]]$INTERMEDIATE_ASSAY_DATA)[-1]
  
  #remove leading underscores and ???
  
  if(length(dataNames >0)) {
  

  
  for(i in 1:length(dataNames))
   {
    
     eval(parse( text= paste0(dataNames[i], "<- unlist(lapply(derivedSamples, function(x) x$INTERMEDIATE_ASSAY_DATA$",
           "'",variableNames[i],"'))")
     ))
    
    paste0("entity$",dataNames[1]," <-", dataNames[i])
    
   }
  
  }
  
  list(entity=entity,response=response$response)

  }




