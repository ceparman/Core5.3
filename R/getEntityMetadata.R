#' getEntitMetadata - Get entity metadata.
#'
#'\code{getEntityMetadata}  Get entity metadata.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get metadata for
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list containing $attributes for attribute properties, 
#'and $association for association properties; $template contains a list that can be converted to JSON for object creation.
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::getEntityMetadata(login$coreApi,"entityType","barcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman ngsAnalytics, ngsanalytics.com
#'@description \code{getEntityMetadata} Get an entity metadata by entityType. Returns a list with three data frames
#' named attributes,associations, and template. Template can be used as the body to for createEntity functions.




getEntityMetadata <- function (coreApi, entityType, useVerbose = FALSE)
  
{
  #clean the name for ODATA
  
  entityType <- CoreAPIV2::ODATAcleanName(entityType)
  
  ## get all metadata
  
  header <- c(Accept = "application/xml")
             
              #need special GET for XML with a basic authorization header
              
              m <-
                CoreAPIV2::apiGET(
                  coreApi,
                  resource = NULL,
                  query = "$metadata",
                  headers = header,
                  useVerbose = useVerbose
                )
              
              
              doc <- XML::xmlTreeParse(m$response)
              
              xmltop = XML::xmlRoot(doc)
              
              topchildren <- XML::xmlChildren(xmltop)
              
              schemachildren <-
                XML::xmlChildren(topchildren$DataServices)[["Schema"]]
              
              
              entity <-
                schemachildren[[which(lapply(XML::xmlSApply(schemachildren, XML::xmlAttrs), function(x)
                  x[["Name"]]) == entityType)]]
              
              
              
              if (is.null(entity)) {
                stop({
                  print(paste("entity name", entityType, "not recognized"))
                  
                },
                call. = FALSE)
                
              }
              
              
              
              
              
              #Get Attribues
              
              
              properties <- entity[names(entity) == "Property"]
              
              names <-
                sapply(lapply(properties, XML::xmlAttrs), function(x)
                  x["Name"])
              types <-
                sapply(lapply(properties, XML::xmlAttrs), function(x)
                  x["Type"])
              defaults <-
                sapply(lapply(properties, XML::xmlAttrs), function(x)
                  x["DefaultValue"])
              
              
              
              if (length(types) != 0)
              {
                attributes <-
                  data.frame(
                    names = names,
                    types = types,
                    defaults = defaults,
                    stringsAsFactors = FALSE
                  )
                
              } else
                
                
              {
                attributes <- list()
              }
              
              
              
              
              
              #Get Associations
              
              
              
              navigation <- entity[names(entity) == "NavigationProperty"]
              
              names <-
                sapply(lapply(navigation, XML::xmlAttrs), function(x)
                  x["Name"])
              
              names <- paste0(names, "@odata.bind")
              
              types <-
                sapply(lapply(navigation, XML::xmlAttrs), function(x)
                  x["Type"])
              partners <-
                sapply(lapply(navigation, XML::xmlAttrs), function(x)
                  x["Partner"])
              
              
              
              
              
              if (length(types) != 0)
              {
                associations <-
                  data.frame(
                    names = names,
                    types = types,
                    partners = partners,
                    stringsAsFactors = FALSE
                  )
                
                #remove associations with out partners, not sure what they are
                
                associations <- associations[!is.na(associations$partners),] 
                
                
                forward_associations <-
                  associations[!startsWith(associations$names, "REV_"), ]
                
                association_values <- as.list(rep("", nrow(forward_associations)))
                
                names(association_values) <- forward_associations$names
              } else
              {
                association_values <- list()
                associations <- list()
              }
              
              
       
              
              #Create list object that can be used for create
              
              if (!is.null(nrow(attributes))) {
                atttribute_values <- as.list(rep("", nrow(attributes)))
                
                names(atttribute_values) <- attributes$names
              } else
                atttribute_values <- list()
              
              
              
              template <- c(atttribute_values, association_values)
              
              list(attributes = attributes,
                   associations = associations,
                   template = template)
}



