#  THIS IS NOT WPRKING  5.2 json API file attach appears to not be working 
#
#
#' attachFile - Attaches a file to an entity or file attribute.
#'
#' \code{attachFile}  Attaches a file to an entity or file attribute.
#' @param coreApi coreApi object with valid jsessionid
#' @param barcode User provided barcode as a character string
#' @param filename name to use for the attached file
#' @param filepath path to the file to attach
#' @param targetAttributeName -NOT WORKING if included the name if the attribute to attach the file to.  Must be in all caps.  NOT WORKING
#' @param useVerbose Use verbose communication for debugging
#' @export
#' @return RETURN returns a list $entity contains entity information, $response contains the entire http response
#' @examples
#'\dontrun{
#' api<-CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' newitem<-CoreAPIV2::attachFile(response$coreApi,barcode,filename,
#'          filepath,targetAttributeName="",useVerbose=FALSE)
#' CoreAPIV2::logOut(login$coreApi )
#' }
#'@author Craig Parman
#'@description \code{attachFile} Attaches a file to entity identified by barcode.


# @param targetAttributeName - if included the name if the attribute to attach the file to.  Must be in all caps.  NOT WORKING

attachFile<-function (coreApi,barcode,filename,filepath,targetAttributeName="",useVerbose=FALSE)

{




  if(!file.exists(filepath)) {

    stop(
      {print("Unable to find file on local OS")
        print( filepath)
      },
      call.=FALSE
    )

  }




  sdkCmd<-jsonlite::unbox("file-attach")

   data<-list(targetEntityBarcode = jsonlite::unbox(barcode),
              targetEntityId=jsonlite::unbox(""),
              name=jsonlite::unbox(filename),
             targetAttributeName=jsonlite::unbox(""), #  targetAttributeName=jsonlite::unbox(targetAttributeName),  NOT WORKING
             fileContentTypeOverride=jsonlite::unbox("")
   )




   responseOptions<-c("CONTEXT_GET","MESSAGE_LEVEL_WARN")
   logicOptions<-"EXECUTE_TRIGGERS"
   typeParam <- jsonlite::unbox("FILE")




   request<-list(request=list(sdkCmd=sdkCmd,data=data,typeParam =typeParam,
                              responseOptions=responseOptions,
                              logicOptions=logicOptions))


   
   headers<-c("Content-Type" = "multipart/related")
   
   
   


   form<-list(json = jsonlite::toJSON(request),
              fileData=httr::upload_file(filepath,type = "image/png")
   )

  
  
  body <- list(
    json = jsonlite::toJSON(request),
    fileData = httr::upload_file(filepath)
  )
  
  
    cookie <- c(JSESSIONID = coreApi$jsessionId, AWSELB = coreApi$awselb )
  
    

   response<- httr::POST(paste0(coreApi$scheme,"://",coreApi$coreUrl,"/sdk"),
                          body=body,
                          httr::verbose(data_out= FALSE),
                          httr::add_headers("Content-Type" = "multipart/form-data"),
                          httr::set_cookies(cookie)
                         
   )
   
   
   
   
   #check for general HTTP error in response
   
   if(httr::http_error(response)) {
     
     stop(
       {print("json API file-attach call failed")
         print( httr::http_status(response))
       },
       call.=FALSE
     )
     
     
   }

  list(entity=httr::content(response)$response$data,response=response)

}



