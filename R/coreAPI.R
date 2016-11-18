#'coreAPI Creates a object of class coreAPI that contains user and connection information.
#'@param CoreAccountInfo file with account information in json format.
#'@return Object of class coreAPI
#'@export
#'@examples
#'\dontrun{
#'api<-coreApi("/home")
#'}
#'@details{ Creates a object of class coreAPI that contains user name,
#'          password base url, account, port.
#'          It has slots for account, jsessionID, AWSELB, and base URL.
#'          Requires a json file that is a POSTMAN environment file. 
#'         \code{#'Creates a object of class coreAPI that contains account information}
#'         \code{coreAPI("path to json")}.
#'
#'The json must include the fields shown below.  The account value may be set to ""
#' if the user only has access to one tenant.
#'Example json object.
#'
#'          \code{
#'                 "id": "c3e550c7-b06a-4897-86a9-ae240ec0104b",
#'                 "name": "Test 5.2",
#'                 "values": [
#'                   {
#'                     "key": "tenant",
#'                     "value": "R-Integration_Baseline",
#'                     "type": "text",
#'                     "enabled": true
#'                   },
#'                   {
#'                 "key": "scheme",
#'                 "value": "http",
#'                 "type": "text",
#'                 "name": "scheme",
#'                 "enabled": true,
#'                 "hovered": false
#'                 },
#'                 {
#'                 "key": "host",
#'                 "value": "qakms-test.coredev.cloud",
#'                 "type": "text",
#'                 "name": "host",
#'                 "enabled": true#'                 ,
#'                 "hovered": false
#'                 },
#'                 {
#'                 "key": "context",
#'                 "value": "",
#'                 "type": "text",
#'                 "enabled": true
#'                 },
#'                 {
#'                 "key": "username",
#'                 "value": "apitester",
#'                 "type": "text",
#'                 "name": "nameadmin",
#'                 "enabled": true,
#'                 "hovered": false
#'                   },
#'                 {
#'                     "key": "password",
#'                     "value": "passtests",
#'                     "type": "text",
#'                     "name": "passwordadmin",
#'                     "enabled": true,
#'                 "hovered": false
#'                 },
#'                 {
#'                     "key": "port",
#'                     "value": "80",
#'                     "type": "text",
#'                     "enabled": true
#'                 }
#'                   ],
#'                 "timestamp": 1479400209811,
#'                 "_postman_variable_scope": "environment",
#'                 "_postman_exported_at": "2016-11-18T18:29:23.546Z",
#'                 "_postman_exported_using": "Postman/4.8.3"
#'                 }
#'}


coreAPI<- function(CoreAccountInfo)
{

  accountinfo <-jsonlite::fromJSON(CoreAccountInfo)$values
 # if  (accountinfo$account == "") accountinfo$account <- NULL
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

