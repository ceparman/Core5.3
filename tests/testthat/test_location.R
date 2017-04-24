
context("Tests for location get and update")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



     test_that(paste("test log in and get an entity", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE","PS1",fullMetadata=FALSE,useVerbose=verbose)$entity
 
              expect_match(b$Barcode,"PS1",all=verbose)
              
              lc<-CoreAPIV2::updateEntityLocation(coreApi,"PATIENT_SAMPLE","PS1","LC1",useVerbose=FALSE)
             
              expect_match( httr::http_status(lc$response)$category,"Success")
              
            
                            
              lc1<-getEntityLocation(coreApi,"PATIENT_SAMPLE","PS1",useVerbose=FALSE)
              
              expect_match(lc1$entity[[1]]$Barcode,"LC1")
              
              lc<-CoreAPIV2::updateEntityLocation(coreApi,"PATIENT_SAMPLE","PS1","LC2",useVerbose=TRUE)
              expect_match( httr::http_status(lc$response)$category,"Success")
              
              
              lc1<-getEntityLocation(coreApi,"PATIENT_SAMPLE","PS1",useVerbose=FALSE)
              
              expect_match(lc1$entity[[1]]$Barcode,"LC2")
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

        
              
              })
  
     
    