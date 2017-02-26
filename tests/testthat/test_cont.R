
context("Tests for container operations")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test log in and get an single well container", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"VIAL","VIA1",fullMetadata=FALSE,useVerbose=verbose)$entity

              expect_match(b$Barcode,"VIA1",all=verbose)
              
              cc<-CoreAPIV2::getContainerContents(con$coreApi,"VIAL","VIA1")   
              
              expect_match(cc$entity$REV_IMPL_CONTAINER_CELL[[1]]$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,
                           "PS2-1",all=verbose)
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

        
              
              })
  
     
    
     
     test_that(paste("test login and get container and cell contents for multi well plate", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 lheader <- c(Accept = "application/json;odata.metadata=full")
                 
                 
                 container<-CoreAPIV2::getEntityByBarcode(con$coreApi,entityType = "384 WELL PLATE",
                                                          barcode='TE1',fullMetadata = TRUE,useVerbose = TRUE)
                 expect_match(container$entity$CONTAINER_FORMAT,"384 Well",all=verbose)
                 
                 
                
                 
                 p<- CoreAPIV2::getWellContents(con$coreApi,"384 WELL PLATE","TE1","1",useVerbose = FALSE)
                 
              
                 expect_match(p$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Barcode,"PS1-1",all=verbose)
                
                 cc<-CoreAPIV2::getContainerContents(con$coreApi,"384 WELL PLATE","TE1")   
                 
                 expect_match(cc$entity$REV_IMPL_CONTAINER_CELL[[1]]$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,
                              "PS1-1",all=verbose)
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     test_that(paste("test  get container IDs for multi well plate and Vial", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 
                 cells<-CoreAPIV2::getContainerCellIds(con$coreApi,containerType = "384 WELL PLATE",
                                                       containerBarcode = 'TE1',useVerbose = FALSE)
                 
                 expect_equal(cells$entity[1],18535078,all=verbose)
                 
                 cells<-CoreAPIV2::getContainerCellIds(con$coreApi,containerType = "VIAL",
                                                       containerBarcode='VIA1',useVerbose = TRUE)
                 
                 expect_equal(cells$entity[1],18161996,all=verbose)
                 
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     
     
    