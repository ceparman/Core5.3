
context("Tests for creating a plate and populating it")

 

rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test login parameters for environment", instance),
            {
 #log in
              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              
              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
  #create plate            
              
              return<-CoreAPIV2::createEntity(con$coreApi,"384 WELL PLATE",body=fromJSON("{}"))  
         
#fill the plate with concentration series              
 
    containerBarcode <-return$entity$Barcode          
    containerType <- "384 WELL PLATE"
    sampleLotType <-  "PATIENT SAMPLE LOT"
    sampleLotBarcode <- "PS2-1"
    amount <- 1
    amountUnit <- "ml"
    concentrationUnit <- "mM"
    
      for (i in 1:16){  #                  loop over rows
        
          for (j in 1:24){              #loop over columns
          
          containerWellNum <- (i-1)*24 + j
          
          print( containerWellNum )
          concentration <- j*2  
            
            CoreAPIV2::setWellContents(con$coreApi, containerType,containerBarcode, containerWellNum,
                                       sampleLotType,sampleLotBarcode, amount, amountUnit, concentration,
                                       concentrationUnit,useVerbose = FALSE)
            
            
          }
        
        
      }                          
              
             
              
               
              wc1<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 1, useVerbose = FALSE)
              wc384<- CoreAPIV2::getWellContents(con$coreApi, containerType, containerBarcode, 384, useVerbose = FALSE)
              

              expect_equal(wc1$entity$CI_AMOUNT,1,all=verbose)
              expect_equal(wc384$entity$CI_AMOUNT,1,all=verbose)
              
              
              expect_equal(wc1$entity$CONTENT[[1]]$CI_CONC,2,all=verbose)
              expect_equal(wc384$entity$CONTENT[[1]]$CI_CONC,48,all=verbose)
              
              
              
              expect_match(wc1$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
              expect_match(wc384$entity$CONTENT[[1]]$IMPL_SAMPLE_LOT$Name,sampleLotBarcode,all=verbose)
              
              
              
            
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
     
     