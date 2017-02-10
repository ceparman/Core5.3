
context("Tests for container operations")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test log in and get an single well container", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"VIAL","VIA1",fullMetadata=FALSE,useVerbose=verbose)$entity

              expect_match(b$Barcode,"VIA1",all=verbose)
              
              
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
                 
                 
                
                 
                 p<- CoreAPIV2::getCellContents(con$coreApi,"TE1","1",useVerbose = TRUE)
                 
              
                 expect_match(p$entity$cells[[1]]$cellContents[[1]]$lotBarcode,"PS1-1",all=verbose)
                
                
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     
     
    