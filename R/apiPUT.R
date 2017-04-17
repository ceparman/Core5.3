#' apiPUT - Do a PUT to the Core ODATA REST API.
#'
#' \code{apiPUT}  Do a PUT to the Core ODATA REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type for PUT
#' @param query query string
#' @param body body for request
#' @param encode encoding to use for request option are "multipart", "form", "json", "raw"
#' @param headers  headers to be added to get.
#' @param special  passed to buildUrl for special sdk endpoints
#' @param useVerbose  Use verbose communication for debugging
#' @export
#' @return Returns the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiPUT(login$coreApi,"SAMPLE",body,"json",special=NULL,useVerbose=FALSE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{apiPUT} - Base call to Core ODATA REST API.



apiPUT<-function(coreApi,resource=NULL,query=NULL,body=NULL,encode,headers=NULL,special=NULL,useVerbose=FALSE)
{

  
  
  
#clean the resource name for ODATA
  resource <- CoreAPIV2::ODATAcleanName(resource)  

#Check that encode parameter is proper

    if ( !(encode %in% c("multipart", "form", "json", "raw"))) {
         stop(
           {print("encode parameter not recognized")
             print( httr::http_status(response))
           },
           call.=FALSE
         )

    }



 sdk_url<-  CoreAPIV2::buildUrl(coreApi,resource=resource,query=query,special=special,useVerbose=useVerbose)

 cookie <- c(JSESSIONID = coreApi$jsessionId, AWSELB = coreApi$awselb )
 

 
 
response<-    httr::PUT(url = sdk_url,body = jsonlite::toJSON(body,auto_unbox = TRUE, null = "null"), httr::set_cookies(cookie),encode = "raw",
              httr::add_headers(headers),httr::verbose(data_out = useVerbose, data_in = useVerbose,
                                                       info = useVerbose, ssl = useVerbose)
              )

        
 


 
 #check for general HTTP error in response
# 
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
