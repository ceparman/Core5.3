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

  
#clean the resource name for ODATA
  
  resource <- CoreAPIV2::ODATAcleanName(resource)
 
sdk_url<- CoreAPIV2::buildUrl(coreApi,resource=resource,query=query,special=NULL,useVerbose=useVerbose)
base_sdk_url <- sdk_url  #need if we need to build url for additional chunks  
 
cookie <- c(JSESSIONID = coreApi$jsessionId, AWSELB = coreApi$awselb )
  
#Get first response

if (useVerbose){  
           response<- httr::with_verbose(httr::GET(sdk_url,httr::add_headers(headers)),
                                         httr::set_cookies(cookie)
                                         ) 
  
     } else  {
       
     response<-httr::GET(sdk_url,httr::add_headers(headers),
                         httr::set_cookies(cookie)
                     ) 
  
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
#determine if this is a chunked response


if(!is.null(httr::headers(response)$'transfer-encoding'))
{chunked <- (httr::headers(response)$'transfer-encoding' == "chunked")

} else chunked <- FALSE

chunked <- !is.null(httr::content(response)$`@odata.nextLink`) ### added to account for chunked header when not really chunked

#two methods for chunked and not chunked
# it appears sometimes we get a content$value and sometimes we get just content 

if(!chunked){
  #not chunked response
  if(is.null( httr::content(response)[["value"]])){
  content <- httr::content(response)  
    } else content <- httr::content(response)$value  
  
} else
{
  #chunked response  
  more_content <- TRUE #flag for more chunks
  
  skiptoken <- 1  #counter for chunks
  
  content <- httr::content(response)$value
 
  
     while (more_content)  
       
     {
       #build url for next chunk
       sdk_url<-paste0(base_sdk_url,"?$skiptoken=",skiptoken)
       
       skiptoken <- skiptoken+1
       
       #get next data chunk
       
       #
       if (useVerbose){  
         response<- httr::with_verbose(httr::GET(sdk_url,httr::add_headers(headers)),
                                       httr::set_cookies(cookie)) 
         
       } else  {
           
         response<-httr::GET(sdk_url,httr::add_headers(headers),
                             httr::set_cookies(cookie))
         
       }    
       #add content 
       
       content <- c(content,httr::content(response)$value)
       
       #Is there more content ?
       
       more_content <- !is.null(httr::content(response)$`@odata.nextLink`)
     }
   
  
  
  
}

 
      
out <- list(content = content, response = response)   
   
return(out)
}

