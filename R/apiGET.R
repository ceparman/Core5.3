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

 
    sdk_url<- CoreAPIV2::buildUrl(coreApi,resource=resource,query=query,special=special,useVerbose=useVerbose)
  
# deal with chunked reponses best done here 

    
more_content <- TRUE

content <- list()

while (more_content == TRUE)
 {
  
  
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
  
#check for more data 

 if(is.null(content(cell3)$`@odata.nextLink`)) {
  
   content <- c(content,httr::content(response))  
  
   more_content <- FALSE
  
   } else {
  
     tcontent <- httr::content(response)
#remove link to additional data
  
     tcontent$`@odata.nextLink` <- NULL   #remove more data flag.
     content <- c(content,tcontent) 
  
    }
  
 }



out <- list(content = content, response = response)   
   
return(out)
}
