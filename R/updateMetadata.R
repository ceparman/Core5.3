#`updateMetadata - Updates cached metadata so metadata is up to date.
#'
#'\code{updateMetadata} g Updates cached metadata so metadata is up to date.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns XML with all entitiy metadata
#' @export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' metadata<-CoreAPIV2::updateMetadata(login$coreApi,useverbose=TRUE)
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{updateMetadata}  Updates cached metadata so metadata is up to date. 
#'Must be run after any configuration changes.


updateMetadata<-function(coreApi,useVerbose=FALSE)

{
  
resource <-"$metadata"
query <- "?reload=1"

header <- c(Accept= "application/xml")

out <- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,headers = header,useVerbose=useVerbose)

  

list(entity=out$content,response=out$response)

}



