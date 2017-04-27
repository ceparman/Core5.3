#' updateExperimentSampleData - Update experiment sample assay data.
#'
#' \code{updateExperimentSampleData} Update experiment sample assay data.
#'
#' @param coreApi coreApi object with valid jsessionid
#' @param assayType entity type to get as character string
#' @param experimentSamplebarcode User provided barcode as a character string
#' @param assayAttributeValues assay attributes as a list of key-values pairs
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information,
#'        $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' newdata<-CoreAPIV2::updateExperimentSampleData(login$coreApi,assayType,
#'                   experimentSampleBarcode,assayAtributeValues)
#' CoreAPIV2::logOut(login$coreApi ) response<- CoreAPI::authBasic(coreApi)
#' }
#'@author Craig Parman
#'@description \code{updateExperimentSampleData} Update experiment sample assay data.


updateExperimentSampleData<-function (coreApi,assayType,experimentSamplebarcode,assayAttributeValues,
                                      useVerbose=FALSE)

{

  
  
  #Clean Names of assay
  
  assayType <- CoreAPIV2::ODATAcleanName(assayType)
  
  
  #Clean Names of attributes

  
  for(i in 1:length(names(assayAttributeValues)))
  {
    
    names(assayAttributeValues)[i] <- CoreAPIV2::attributeCleanName( names(assayAttributeValues)[i])
    
  }

  
  
  body<-assayAttributeValues   #needs to be unboxed
  
  resource <- paste0( assayType,"_DATA")
  query <- paste0("('",experimentSamplebarcode,"')")
  
  header<-c("Content-Type"="application/json","If-Match"="*")  
  
  response<- CoreAPIV2::apiPUT(coreApi,resource = resource, query=query,body=body,encode="raw",
                                headers = header, useVerbose=useVerbose)
  

  list(entity=httr::content(response),response=response)

}


