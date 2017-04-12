#' attributeCleanName - converts names to uppercase and replaces spaces with underscores to 
#' reference attribute names.compliant version. Used to clean names in ODATA calls.
#'
#' \code{attributeCleanName} clean attribute name.
#' @param name  string to clean 
#' @export
#' @return Returns name in ODAt compliant form
#' @examples
#'\dontrun{
#' new_name <-CoreAPIV2::attributeCleanName("My Comments")
#' new_name
#' MY_COMMENTS
#'  }
#'@author Craig Parman
#'@description \code{attriureCleanName} - onverts names to uppercase and replaces spaces with underscores to 
#' reference attribute names.compliant version. Used to clean names in ODATA calls.
attributeCleanName<-function(name)
{
  
name <- toupper(name)

name <- gsub(" ","_",name)
  

name  
  
  
}