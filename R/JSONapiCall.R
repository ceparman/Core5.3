#' JSONapiCall - Base call to Core REST API.
#'
#' \code{JSONapiCall}  Base call to Core json REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param body body for request
#' @param encode encoding to use for request option are "multipart", "form", "json", "raw"
#' @param headers  headers to add
#' @param special - passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return RETURN return the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::JSONapiCall(login$coreApi,body,encode="json",,special=NULL,useVerbose=FALSE)
#' logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{JSONapiCall}  Base call to Core json REST API.


JSONapiCall<-function(coreApi,body,encode,headers = NULL,special=NULL,useVerbose=FALSE)
{
#Check that encode parameter is proper

   if ( !(encode %in% c("multipart", "form", "json", "raw"))) {
        stop(
          {print("encode parameter not recognized")
            print( httr::http_status(response))
          },
          call.=FALSE
        )

   }



sdk_url<-  CoreAPIV2::JSONbuildUrl(coreApi,special=special,useVerbose=useVerbose)

cookie <- c(JSESSIONID = coreApi$jsessionId, AWSELB = coreApi$awselb )

        response<-invisible(httr::POST(sdk_url,body = body, encode=encode,httr::set_cookies(cookie),
                                        httr::add_headers(headers),
                                        httr::verbose(data_out = useVerbose, data_in = useVerbose,
                                                      info = useVerbose, ssl = useVerbose))
                                  )
    
  


#check for general HTTP error in response

if(httr::http_error(response)) {

  stop(
    {print("json API call failed")
      print( httr::http_status(response))
    },
    call.=FALSE
  )


}

return(response)
}
