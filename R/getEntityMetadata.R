#' getEntitMetadata - Get entity metadata.
#'
#'\code{getEntityMetadata}  Get entity metadata.
#'
#'@param coreApi coreApi object with valid jsessionid
#'@param entityType entity type to get metadata for
#'@param useVerbose TRUE or FALSE to indicate if verbose options should be used in http POST
#'@return returns a list $entity contains entity information; $description - a list containing $attributes for attribute properties, 
#'and $association for association properties; $template contains a list that can be converted to JSON for object creation.
#'@export
#'@examples
#'\dontrun{
#' api<-CoreAPIV2::CoreAPI("PATH TO JSON FILE")
#' login<- CoreAPIV2::authBasic(api)
#' item<-CoreAPIV2::getEntityMetadata(login$coreApi,"entityType","barcode")
#' CoreAPIV2::logOut(login$coreApi)
#' }
#'@author Craig Parman
#'@description \code{getEntityMetadata} Get an entity metadata by entityType. Returns a list with two data frames
#' named attributes and associations.



getEntityMetadata<-function (coreApi,entityType,useVerbose=FALSE)

{
  
## get all metadata  

m<- CoreAPIV2::apiGET(coreApi,resource=NULL,headers=httr::accept_xml(),query="$metadata",useVerbose = useVerbose)  

doc<-XML::xmlTreeParse(m)

xmltop = XML::xmlRoot(doc)

topchildren<-XML::xmlChildren(xmltop)

schemachildren<-XML::xmlChildren(topchildren$DataServices)[["Schema"]]


entity<-schemachildren[[which(lapply(XML::xmlSApply(schemachildren,XML::xmlAttrs), function(x) x[["Name"]]) == entityType)]]

#Get Attribues


properties<-entity[names(entity) == "Property"]

names<-sapply(lapply(properties, XML::xmlAttrs), function(x) x["Name"])
types<-sapply(lapply(properties, XML::xmlAttrs), function(x) x["Type"])
defaults<-sapply(lapply(properties, XML::xmlAttrs), function(x) x["DefaultValue"])
attributes<-data.frame(names = names, types = types, defaults=defaults,stringsAsFactors = FALSE)

#Get Associations



navigation <-entity[names(entity) == "NavigationProperty"]

names<-sapply(lapply(navigation, XML::xmlAttrs), function(x) x["Name"])

names<-paste0(names,"@odata.bind")

types<-sapply(lapply(navigation, XML::xmlAttrs), function(x) x["Type"])
partners<-sapply(lapply(navigation, XML::xmlAttrs), function(x) x["Partner"])
associations<-data.frame(names = names, types = types, partners=partners,stringsAsFactors = FALSE)


#Create list oblect that can be used for create

atttribute_values<-as.list(rep("",nrow(attributes)))

names(atttribute_values) <- attributes$names



forward_associations <- associations[!startsWith(associations$names,"REV_"),] 

association_values<-as.list(rep("",nrow(forward_associations)))

names(association_values) <- forward_associations$names

template <- c(atttribute_values,association_values)

list(attributes=attributes,associations=associations,template=template)
}



