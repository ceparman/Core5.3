#' getEntityByBarcode - Get an entity from the LIMS by barcode.
#'
#'\code{getEntityByBarcode} get an entity from the LIMS by barcode
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::getEntityByBarcode(login$coreApi,"entityType","barcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{getEntityByBarcode} Get an entity from the LIMS by barcode and entityType.



getEntityByBarcode<-function (coreApi,entityType,barcode,useVerbose=FALSE)

{

 resource <- entityType
  
 query   <- paste0("('",barcode,"')")

  request<-list(request=list(sdkCmd=jsonlite::unbox("get"),
                             data=list(entityRef=list(name=jsonlite::unbox(""),entitiyID=jsonlite::unbox(""),barcode=jsonlite::unbox(barcode))),
                             responseOptions=c("CONTEXT_GET","MESSAGE_LEVEL_WARN"),
                             typeParam = jsonlite::unbox(entityType),
                             logicOptions=list()
                    ))


  response<- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,useVerbose=useVerbose)

  

  list(entity=httr::content(response),response=response)

  }



