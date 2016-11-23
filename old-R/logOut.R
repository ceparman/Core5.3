#' logOut -Log user out of the LIMS.
#'
#'\code{logOut} logs user out of the LIMS using Core API
#'
#'@param coreApi coreApi object returned during log in
#'@param useVerbose use verbose option for debuggin in http POST
#'@return returns list with $success = TRUE when sucessful, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' response<- CoreAPI::authBasic(api)
#' logOut(response$coreApi,useVerbose=TRUE )
#' }
#'@author Craig Parman
#'@description \code{logOut} logs out of the current jsession.


### Log out

logOut<-function(coreApi, useVerbose = FALSE)

{

 

  request<-list(request=list(data= list(),typeParam =jsonlite::unbox("*"), sdkCmd =jsonlite::unbox("sdk-logout")),
                responseOptions = list())

  
  response<- CoreAPIV2::apiCall(coreApi,request,"json",special="login",useVerbose=useVerbose)


  list(success= httr::http_status(response)$category,response=response)



}



