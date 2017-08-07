#` updateEntityAssociations - Update entity assoications
#'
#'\code{updateEntityAssociations} Update entity assoications
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get
#'@param barcode barcode of entity to get
#'@param updateValues values to update as list of associations contex and entity type pair and barcode 
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information, $response contains the entire http response
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#'updateValues <- list(SAMPLE_ENZYME = c("ENZYME", "ENZ2"))
#' response <-CoreAPIV2::updateEntityAssociations(login$coreApi,"entityType","barcode",values)
#' udatedEntity <- response$entity
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{updateEntityAssociations}  Update entity associations.




updateEntityAssociations <-
  function (coreApi,
            entityType,
            barcode,
            updateValues,
            useVerbose = FALSE)
    
  {
    #clean the name for ODATA
    
    entityType <- CoreAPIV2::ODATAcleanName(entityType)
    
    resource <- entityType
    
    query   <- paste0("('", barcode, "')")
    
    # Get entityType
    
    entity <-
      CoreAPIV2::getEntityByBarcode(coreApi,
                                    entityType,
                                    barcode,
                                    fullMetadata = FALSE,
                                    useVerbose = TRUE)
    
    
    old_values <- entity$entity
    
    
    #check to see if all values to update are in the entity
    
    
    newAssociations <- list()
    
    namesToUpdate <- names(updateValues)
    
    for (i in 1:length(namesToUpdate))
      
    {
      name <- paste0(namesToUpdate[i], "@odata.bind")
      
      value <-
        paste0("/", updateValues[[namesToUpdate[i]]][1], "('", updateValues[[namesToUpdate[i]]][2], "')")
      
      
      newAssociations[[name]] <- value
      
    }
    
    old_values <- c(old_values, newAssociations)
    
    
    
    
    body <- old_values
    
    resource <- paste0(entityType)
    query <- paste0("('", barcode, "')")
    
    header <- c("Content-Type" = "application/json", "If-Match" = "*")
    
    #update record
    
    
    response <-
      CoreAPIV2::apiPUT(
        coreApi,
        resource = resource,
        query = query,
        body = body,
        encode = "raw",
        headers = header,
        useVerbose = useVerbose
      )
    
    
    
    
    list(entity = httr::content(response), response = response)
    
  }
