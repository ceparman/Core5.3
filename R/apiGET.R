#' apiGET - Do a get to the Core ODATA REST API.
#'
#' \code{apiGET}  Base call to Core REST API.
#' @param coreApi coreApi object with valid jsessionid
#' @param resource entity type to get
#' @param query query string
#' @param headers  headers to be added to get.
#' @param special passed to buildUrl for special sdk endpoints
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return Returns the content, concatenated if a chunked response, and the entire http response for the last 
#' get if cunked content.
#' @examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' response <-CoreAPIV2::apiGET(login$coreApi,"json",resource,query)
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{apiGET} - Do a get to the Core ODATA REST API.


apiGET<-function(coreApi,resource,query,headers=NULL,special=NULL,useVerbose=FALSE)
{

 
    sdk_url<- CoreAPIV2::buildUrl(coreApi,resource=resource,query=query,special=NULL,useVerbose=useVerbose)
  
    
#header string is fomedr by: header <-c(accept = "application/json;odata.metadata=full")
# this object is then passed to apiGet    
  
more_content <- TRUE

content <- list()

while (more_content == TRUE)
 {
  
if (useVerbose){  
            response<- httr::with_verbose(httr::GET(sdk_url,httr::add_headers(.headers=headers))) 
                    
} else  {
              
      response<-httr::GET(sdk_url,httr::add_headers(.headers=headers))
  
            }                     

#check for general HTTP error in response

 if(httr::http_error(response)) {
  
  stop(
    {print("API call failed")
      print( httr::http_status(response))
    },
    call.=FALSE
  )
  
  
 }
  
#check for more data 

 if(is.null(httr::content(response)$`@odata.nextLink`)) {
  
   content <- c(content,httr::content(response))  
  
   more_content <- FALSE
  
   } else {
  
     tcontent <- httr::content(response)
#remove link to additional data
  
     tcontent$`@odata.nextLink` <- NULL   #remove more data flag.
     content <- c(content,tcontent) 
#update sdk_url to next chunk
   
      sdk_url<-  httr::content(response)$`@odata.nextLink` 
     
    }
  
 }



out <- list(content = content, response = response)   
   
return(out)
}
