
context("Tests get getEntityMetadata")

 

rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



     test_that(paste("test getEntityMetadata for entity with attributes and associations 1", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              out<-CoreAPIV2::getEntityMetadata(con$coreApi,"PATIENT_SAMPLE",useVerbose = FALSE)
              
              expect_match(out$attributes$names[1], "SOURCE_LAB")
            
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
     
     test_that(paste("test getEntityMetadata for entity without attributes and associations 2", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 out<-CoreAPIV2::getEntityMetadata(con$coreApi,"SAMPLE",useVerbose = FALSE)
                 
                 expect_equal(is.list(out$attributes),TRUE)
                 
                 expect_equal(length(out$attributes),0)
                 
                 expect_equal(length(out$associations),3)
               
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
               })
     
     
     
     test_that(paste("test getEntityMetadata errors out if entityType is not correct", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 expect_error(CoreAPIV2::getEntityMetadata(con$coreApi,"Sample",useVerbose = FALSE))
                 
               
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
               })
     
  