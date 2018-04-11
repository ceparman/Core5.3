#' createEntity - Create a new instance of a entity. Used for standard object or "Plan Old Core Objects" (POCOs)
#' 
#' \code{createEntity} Create a new instance of a entity. Used for standard object or "Plan Old Core Objects" (POCOs) 
#' @param coreApi coreApi object with valid jsessionid
#' @param entityType entity type to get as character string
#' @param body values for attributes and associations as a  list of key-values pairs
#' @param locationId location ID for initial location as character string
#' @param projectIds project comma separated list of project IDs as character string
#' @param barcode User provided barcode as a character string
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::coreApi("PATH TO JSON FILE")
#' login<- CoreAPI::authBasic(api)
#' item<-CoreAPI::createEntity(login$coreApi,"Entity_Type")
#' CoreAPI::logOut(login$coreApi )
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{createEntity} Creates a new entity instance. Required inputs are url, jsessionId and entityType.



createEntity <- function (coreApi,
                          entityType,
                          body,
                          locationId = NULL,
                          projectIds = NULL,
                          barcode = NULL,
                          useVerbose = FALSE)
  
{
  #clean the name for ODATA
  
  entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
  #scrub unused fields from the body
  body <- body[!body %in% ""]
  
  
  headers <-
    c('Content-Type' = "application/json;odata.metadata=full", accept = "application/json")
  
  response <-
    CoreAPIV2::apiPOST(
      coreApi,
      resource = entityType,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "raw",
      headers = headers ,
      special = NULL,
      useVerbose = useVerbose
    )
  
  
  
  list(entity = httr::content(response), response = response)
  
}
