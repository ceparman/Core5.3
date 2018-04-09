
context("Tests for authentication")



rm(list=ls())

verbose <- FALSE


#setup to test against multiple environments

environments<<-list.files("test_environments","5-2-2.json",full.names=TRUE)

print(environments)
for (i in 1:length(environments))
  {



     test_that(paste("test login parameters for environment", environments[i]),
            {
              verbose <- FALSE
             
              api <- CoreAPIV2::coreAPI(environments[i])

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
        
              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              
              
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
  
  
  
  test_that("single account with bad password returns error",
                       {
                         verbose <- FALSE
                         api <- CoreAPIV2::coreAPI(environments[i])
                        bapi<- api
                        bapi$pwd <-"badpassword"
                        expect_error(CoreAPIV2::authBasic(bapi,useVerbose = verbose))
                       
               })
             


}






context("Tests metadata update")



rm(list=ls())

verbose <- FALSE


#setup to test against multiple environments

instance<<-"test_environments/5-2-2.json"

  
  
  test_that(paste("test metadata update", instance),
            {
              verbose <- FALSE
              
              api <- CoreAPIV2::coreAPI(instance)
              

              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
              
              metadata<- CoreAPIV2::updateMetadata(con$coreApi,useVerbose=TRUE)
              print(httr::http_status(metadata$response))
              expect_match(httr::http_status(metadata$response)$category ,"Success" )
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")
              
            })
  





