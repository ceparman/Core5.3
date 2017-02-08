#' JSONbuildUrl - build URL for call to Core REST API.
#'
#' \code{apiCall}  base call to Core REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param special flag for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN Core REST URL
#' @examples
#'\dontrun{
#' api<-COREAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::JSONapiCall(login$coreApi,body,"json",useVerbose=FALSE)
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{buildUrl} build URL for call to Core REST API.



JSONbuildUrl<-function(coreApi,special=NULL,useVerbose=FALSE)
{

if (is.null(special)){
 # sdk_url<-paste(coreApi$scheme,"://",coreApi$coreUrl,"/sdk",";jsessionid=",coreApi$jsessionId,sep="")
  sdk_url<-paste0(coreApi$scheme,"://",coreApi$coreUrl,":",coreApi$port,"/sdk")
  } else {

  switch(special,
         login = sdk_url<-paste0(coreApi$scheme,"://",coreApi$coreUrl,"/sdklogin"))


  }

  return(sdk_url)

}
