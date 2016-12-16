#' apiGET - Do a get to the Core REST API.
#'
#' \code{apiGET}  Base call to Core REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param encode encoding to use for request option are "multipart", "form", "json", "raw"
#' @param resource entity type to get
#' @param query query string
#' @param headers  headers to be added to get.
#' @param special - passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN return the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiGET(login$coreApi,"json",resource,query)
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{apiGET}   Do a get to the Core REST API.


apiGET<-function(coreApi,resource,query,headers=NULL,special=NULL,useVerbose=FALSE)
{

 
  

     sdk_url<- CoreAPIV2::buildUrl(coreApi,resource=resource,query=query,special=special,useVerbose=useVerbose)
   response<-invisible(httr::GET(sdk_url,headers,httr::verbose(data_out = useVerbose, data_in = useVerbose,
                                                     info = useVerbose, ssl = useVerbose))
                      )
 
       
        




#check for general HTTP error in response

if(httr::http_error(response)) {

  stop(
    {print("API call failed")
      print( httr::http_status(response))
    },
    call.=FALSE
  )


}

return(response)
}
