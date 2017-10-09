
context("Tests for container operations")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <- "test_environments/5-2-2.json"



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
              
              expect_match(cc$entity$CELLS[[1]]$CELL_CONTENTS[[1]]$SAMPLE_LOT$Name,
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
                 
              
                 expect_match(p$entity$CELL_CONTENTS[[1]]$SAMPLE_LOT$Barcode,"PS1-1",all=verbose)
                
                 cc<-CoreAPIV2::getContainerContents(con$coreApi,"384 WELL PLATE","TE1")   
                 
                 expect_match(cc$entity$CELLS[[1]]$CELL_CONTENTS[[1]]$SAMPLE_LOT$Name,
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
         I        
                 cells<-CoreAPIV2::getContainerCellIds(con$coreApi,containerType = "VIAL",
                                                       containerBarcode='VIA1',useVerbose = TRUE)
                 
                 expect_equal(cells$entity[1],18161996,all=verbose)
                 
                 c<-CoreAPIV2::getCellContents(con$coreApi,cells$entity[1],useVerbose = TRUE)
                 
                 expect_equal(c$entity$Id,18161996,all=verbose)
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     
     test_that(paste("test create container, setContents, and update contents", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 vial<-CoreAPIV2::createEntity(con$coreApi,"VIAL",body=jsonlite::fromJSON("{}"))$entity  
                 
                 barcode<-vial$Barcode
                 
                 
                 setCont<-CoreAPIV2::setWellContents(coreApi = con$coreApi,containerType = 'VIAL',
                                                     containerBarcode = barcode,
                                                     containerWellNum = "1",sampleLotType = "PATIENT SAMPLE LOT",
                                                     sampleLotBarcode = "PS1-1",amount = 1,
                                                     amountUnit = "ml",concentration =1,concentrationUnit = "nM",useVerbose=TRUE)
                 
                 wc1<- CoreAPIV2::getWellContents(con$coreApi, "VIAL", barcode, 1, useVerbose = FALSE)
                 
                 
                 expect_equal(wc1$entity$CI_AMOUNT,1,all=verbose)
                
                 expect_equal(wc1$entity$CELL_CONTENTS[[1]]$CI_CONC,1,all=verbose)
                 
                
                 wc1<-CoreAPIV2::updateCellContents(coreApi = con$coreApi,containerType = 'VIAL',
                                                  containerBarcode = barcode,
                                                  containerCellNum = 0,
                                                  sampleLotBarcode = "PS1-1",amount = 2,
                                                  amountUnit = "ml",concentration =2,concentrationUnit = "nM",useVerbose=TRUE)
                 
                 expect_equal( wc1$entity$cells[[1]]$amount,2,all=verbose)
                 
                 expect_equal(wc1$entity$cells[[1]]$cellContents[[1]]$concentration,"0.002",all=verbose)
                 
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
                 
               })
     
     
     
     
    