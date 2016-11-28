
context("Tests for authentication")



rm(list=ls())

verbose <<- FALSE


#setup to test against multiple environments

environments<<-list.files("test_environments","*.json",full.names=TRUE)


for (i in 1:length(environments))
  {



     test_that(paste("test login parameters for environment", environments[1]),
            {

             
              api <- CoreAPIV2::coreAPI(environments[i])

          

              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              print(con)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
  
  
  
  test_that("single account with bad password returns error",
                       {
                         api <- CoreAPIV2::coreAPI(environments[i])
                        bapi<- api
                        bapi$pwd <-"badpassword"
                        expect_error(CoreAPIV2::authBasic(bapi,useVerbose = verbose))
                       
               })
             


}


#   con<- CoreAPIV2::authBasic(api,useVerbose=TRUE)
#   
#   
#   
#   
#   logout<-CoreAPIV2::logOut(api,useVerbose = TRUE)
#   
#   
# }
# 
# 
# 
#  test_that("test sourcing login parameters",
#            {
#             expect_match(tapi$coreUrl,"http://experience.platformforscience.com",all=TRUE)
#            })
# 
# 
#  test_that("single account successful login",
# 
#            {
#             response<- CoreAPIV2::authBasic(tapi)
#             expect_that(is.null(response$coreApi$jsessionId),equals(FALSE))
#             expect_match(CoreAPIV2::logOut(response$coreApi)$success,"Success" )
#             }
#            )
# 
# bapi<-tapi
# bapi$pwd <-"bad"
# 
#  test_that("single account with bad password return NULL jsessionID",
#            {
#              response<- CoreAPIV2::authBasic(bapi)
#              expect_that(is.null(response$coreApi$jsessionId),equals(TRUE))}
#   )
# 
#  mapi<-CoreAPIV2::coreAPI(CoreAccountInfo = "testfiles/multiaccount.json")
# 
#  test_that("login with multi account user",
#            {
# 
#              response<- CoreAPIV2::authBasic(mapi)
#              expect_that(is.null(response$coreApi$jsessionId),equals(FALSE))
#              expect_match(CoreAPIV2::logOut(response$coreApi)$success,"Success" )
#              }
#  )
# 
# 
# 
#  test_that("single account sucessful login with verbose output",
# 
#            {
#              response<- CoreAPIV2::authBasic(tapi,useVerbose=FALSE)
#              expect_that(is.null(response$coreApi$jsessionId),equals(FALSE))
#              expect_match(CoreAPIV2::logOut(response$coreApi)$success,"Success" )
# 
#              }
#  )
# #
# 
