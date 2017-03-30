
context("Tests for creating a plate and populating it")

 

rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



     test_that(paste("create and fill plate with setWellContents", instance),
            {
 #log in
              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              
              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
  #create plate            
              
              return<-CoreAPIV2::createEntity(con$coreApi,"384 WELL PLATE",body=jsonlite::fromJSON("{}"))  
         
#fill the plate with concentration series              
 
    containerBarcode <-return$entity$Barcode          
    containerType <- "384 WELL PLATE"
    sampleLotType <-  "PATIENT SAMPLE LOT"
    sampleLotBarcode <- "PS2-1"
    amount <- 1
    amountUnit <- "ml"
    concentrationUnit <- "mM"
    
      for (i in 1:16){  #                  loop over rows
        
          for (j in 1:2){              #loop over 2 columns
          
          containerWellNum <- (i-1)*24 + j
          
     
          concentration <- j*2  
            
            CoreAPIV2::setWellContents(con$coreApi, containerType,containerBarcode, containerWellNum,
                                       sampleLotType,sampleLotBarcode, amount, amountUnit, concentration,
                                       concentrationUnit,useVerbose = FALSE)
            
            
          }
        
        
      }                          
              
             
              
               
              wc1<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 1, useVerbose = FALSE)
              wc2<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 2, useVerbose = FALSE)
              

              expect_equal(wc1$entity$CI_AMOUNT,1,all=verbose)
              expect_equal(wc2$entity$CI_AMOUNT,1,all=verbose)
              
              
              expect_equal(wc1$entity$CONTENT[[1]]$CI_CONC,2,all=verbose)
              expect_equal(wc2$entity$CONTENT[[1]]$CI_CONC,4,all=verbose)
              
              
              
              expect_match(wc1$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
              expect_match(wc2$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
              
              
              
            
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
     
     
     
     test_that(paste("create and fill plate with setCellContents", instance),
               {
                 #log in
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 #create plate            
                 
                 return<-CoreAPIV2::createEntity(con$coreApi,"384 WELL PLATE",body=jsonlite::fromJSON("{}"))  
                 
                
                 
                 #fill the plate with concentration series              
                 
                 containerBarcode <-return$entity$Barcode          
                 containerType <- "384 WELL PLATE"
                 sampleLotType <-  "PATIENT SAMPLE LOT"
                 sampleLotBarcode <- "PS2-1"
                 amount <- 1
                 amountUnit <- "ml"
                 concentrationUnit <- "mM"
                 
                 #Get cell IDs
                 
                 ids<-CoreAPIV2::getContainerCellIds(con$coreApi,containerType = containerType,
                                                     containerBarcode = containerBarcode,useVerbose = TRUE)$entity
                 
                 
                 
                 for (i in 1:16){  #                  loop over rows
                   
                   for (j in 1:2){              #loop over 2 columns
                     
                     containerWellNum <- (i-1)*24 + j
                     
                    
                     concentration <- j*2  
                     cellId <- ids[containerWellNum]
                     CoreAPIV2::setCellContents(con$coreApi, containerType,containerBarcode, cellId,
                                                sampleLotType,sampleLotBarcode, amount, amountUnit, concentration,
                                                concentrationUnit,useVerbose = FALSE)
                     
                     
                   }
                   
                   
                 }                          
                 
                 
                 
                 
                 wc1<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 1, useVerbose = FALSE)
                 wc2<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 2, useVerbose = FALSE)
                 
                 
                 expect_equal(wc1$entity$CI_AMOUNT,1,all=verbose)
                 expect_equal(wc2$entity$CI_AMOUNT,1,all=verbose)
                 
                 
                 expect_equal(wc1$entity$CONTENT[[1]]$CI_CONC,2,all=verbose)
                 expect_equal(wc2$entity$CONTENT[[1]]$CI_CONC,4,all=verbose)
                 
                 
                 
                 expect_match(wc1$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
                 expect_match(wc2$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
                 
                 
                 
                 
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
               })
     
     