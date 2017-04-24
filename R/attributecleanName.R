#' attributeCleanName - converts names to uppercase and replaces spaces with underscores to 
#' reference attribute names. 
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
#' reference attribute names.
#' 
attributeCleanName<-function(name)
{
  
name <- toupper(name)

name <- gsub(" ","_",name)
  

name  
  
  
}