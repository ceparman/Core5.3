#'coreAPI - Creates a object of class coreAPI that contains user and connection information.
#'@param CoreAccountInfo file with account information in json format.
#'@return Object of class coreAPI
#'@export
#'@examples
#'\dontrun{
#'api<-coreApi("/home")
#'}
#'@details{ Creates a object of class coreAPI that contains user name,
#'          password base url, and account
#'          if needed. It has slots for account, jsessionID, AWSELB, and base URL.
#'          Requires a json file that contains the user pwd and account.
#'         \code{#'Creates a object of class coreAPI that contains account information}
#'         \code{coreAPI("path to json")}.
#'
#'The json must include the fields shown below.  The account value may be set to ""
#' if the user only has access to one tenant.
#'Example json object.
#'
#'          \code{
#'
#'                [{
#'                  "user": "xxxxxxxxx",
#'                  "pwd": "xxxxxxxxx",
#'                  "coreUrl": "xxxxxxx",
#'                 "account": "xxxxxxxxx",
#'                }]
#'           }
#'}


coreAPI<- function(CoreAccountInfo)
{

  accountinfo <-jsonlite::fromJSON(CoreAccountInfo)$values
  if  (accountinfo$account == "") accountinfo$account <- NULL
   structure(
             list(user=accountinfo$value[accountinfo$key == "username"],
                  pwd = accountinfo$value[accountinfo$key == "password"],
                  account = accountinfo$value[accountinfo$key == "tenant"],
                  coreUrl = accountinfo$value[accountinfo$key == "host"],
                  port = accountinfo$value[accountinfo$key == "port"],
                  scheme = accountinfo$value[accountinfo$key == "scheme"],
                  jsessionId = NULL,
                  awselb = NULL,
                  employeeId = NULL
               )

    ,class="coreAPI"
  )


}

