#' buildJsonUrl - build URL for call to Core REST API.
#'
#' \code{buildJsonUrl}  buils url for Core json REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param special flag for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN Core json REST URL
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiCall(login$coreApi,body,"json",useVerbose=FALSE)
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{buildJsonUrl} build URL for call to Core json REST API.



buildJsonUrl<-function(coreApi,special=NULL,useVerbose=FALSE)
{
  
  
  
  if (is.null(special)){
    sdk_url<-paste0(coreApi$coreUrl,"/sdk",";jsessionid=",coreApi$jsessionId)
    
  } else {
    
    switch(special,
           login = sdk_url<-paste(coreApi$coreUrl,"/sdklogin",sep=""))
    
    
  }
  
  return(sdk_url)
  
}