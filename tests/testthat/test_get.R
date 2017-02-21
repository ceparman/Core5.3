
context("Tests for gets")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test log in and get an entity", instance),
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
  
     
     test_that(paste("test login and get an entity that returns chunked response", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 lheader <- c(Accept = "application/json;odata.metadata=full")
                 
                 cells<- CoreAPIV2::apiGET(con$coreApi,resource = "384 WELL PLATE",query ="('TE1')/REV_IMPL_CONTAINER_CELL" ,headers = lheader,
                                           useVerbose=FALSE)
                 
                 expect_equal(length(cells$content),384,all=verbose)
                 
                 expect_equal(length(cells$content[[1]]),12,all=verbose)
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     test_that(paste("test login and get container and cell contents", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 lheader <- c(Accept = "application/json;odata.metadata=full")
                 
                 
                 container<-CoreAPIV2::getEntityByBarcode(con$coreApi,entityType = "_384_WELL_PLATE",
                                                          barcode='TE1',fullMetadata = TRUE,useVerbose = TRUE)
                 expect_match(container$entity$CONTAINER_FORMAT,"384 Well",all=verbose)
                 
                 
                
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     
     
    