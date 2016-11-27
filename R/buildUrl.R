#' buildUrl - build URL for call to Core ODATA API.
#'
#' \code{apiCall}  base call to Core REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource resource path (required except for special requests)
#' @param query and additional property options (optional)
#' @param special flag for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN Core REST URL
#' @examples
#'\dontrun{
#' api<-CoreAPIV2("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiCall(login$coreApi,body,"json",useVerbose=FALSE)
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{buildUrl} build URL for call to Core REST API.



buildUrl<-function(coreApi,resource=NULL,query=NULL,special=NULL,useVerbose=FALSE)
{

if (is.null(special)){
  sdk_url<-paste(coreApi$scheme,"://",coreApi$coreUrl,":",coreApi$port,"/odata/",resource,query,sep="")

  } else {

  switch(special,
         login = sdk_url<-paste(coreApi$scheme,"://",coreApi$coreUrl,":",coreApi$port,"/odatalogin",sep="")

  )
  }

  return(sdk_url)

}

