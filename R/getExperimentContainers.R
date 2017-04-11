#' getExperimentContainers - Gets experiment samples from experiment identified by barcode.
#'
#'\code{getExperimentContainers}  Gets experiment samples from experiment identified by barcode.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param experimentType experiment entity type to get
#'@param barcode barcode of entity to get
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' experiment<-  CoreAPIV2::getExperimentContainers(login$coreApi,"entityType","barcode")
#' CoreAPIV2:logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{getExperimentContainers}  Gets experiment contaiers from experiment identified by experiment barcode. 
#' Gets samples attached as sample lots and in containers.



getExperimentContainers<-function (coreApi,experimentType,barcode,useVerbose=FALSE)

{

  
  #clean the name for ODATA
  
  resource <- CoreAPIV2::ODATAcleanName(experimentType)
  
  
  query   <- paste0("('",barcode,"')/REV_CONTAINER_EXPERIMENT_EXPERIMENT_CONTAINER")
  
  
  header<-c('Content-Type' = "application/json;odata.metadata=full", Accept = "application/json")  
  
  
  
  response <- CoreAPIV2::apiGET(coreApi,resource =resource, query = query,headers = header,useVerbose=useVerbose)
  
  
  

  list(entity=unlist((lapply(response$content,FUN = function(x) x$Barcode))),response=response)

  }




