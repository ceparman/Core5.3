
context("Tests for gets")



rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test login parameters for environment", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE","PS1",fullMetadata=FALSE,useVerbose=verbose)$entity

              expect_match(b$Barcode,"PS1",all=verbose)
              
              expect_match(b$SOURCE_LAB,"New York Medical Center",all=verbose)
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE","PS1",fullMetadata=TRUE,useVerbose=verbose)$entity
              
              expect_match(b$Barcode,"PS1",all=verbose)
              
              expect_match(b$SOURCE_LAB,"New York Medical Center",all=verbose)
              
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              
              
              
              
              
              
              })
  
 
     instance <<- "test_environments/Beer.postman_environment.json"
     
     
     
     test_that(paste("test login parameters for environment", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"BEER","BEER1",fullMetadata=FALSE,useVerbose=verbose)$entity
                 
                 expect_match(b$Barcode,"BEER1",all=verbose)
                 
                 expect_match(b$Name,"Sarges Best Dark Lager",all=verbose)
                 
                 b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"BEER","BEER1",fullMetadata=TRUE,useVerbose=verbose)$entity
                 
                 expect_match(b$Barcode,"BEER1",all=verbose)
                 
                 expect_match(b$Name,"Sarges Best Dark Lager",all=verbose)
                 
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
               })
     
     
     
