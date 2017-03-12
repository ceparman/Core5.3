
context("Tests for experiments")



rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



test_that(paste("test login,create sample and lot", instance),
          {
            
            
            account<-CoreAPIV2::coreAPI(instance)
            
            con<-CoreAPIV2::authBasic(account)
            
            coreApi<-con$coreApi
        
            useVerbose <- TRUE
            
            experimentType <-"SIMPLE EXPERIMENT"
            
            protocolType <-"SIMPLE EXPERIMENT PROTOCOL" 
            assayType <-"SIMPLE ASSAY"
            
            assayBarcode <- "SA1"
            
            protocolBarcode <-"ZPX1"
            
            body <- list(EXPT_ATTRIBUTE = "12")
            
            expt<-CoreAPIV2::createExperiment(coreApi,experimentType,assayType,assayBarcode,protocolType,
                                              protocolBarcode,body=body,useVerbose=FALSE)
            
            expect_that( httr::http_status(expt$response)$reason,equals("Created"))
            
            experimentBarcode <- expt$entity$Barcode
            
           
            expect_that( expt$entity$PUBLISHED,equals(FALSE))
            
            response<-experimentPublish(coreApi, "SIMPLE EXPERIMENT", "SPX18",useVerbose = FALSE)
            response$entity$values$PUBLISHED
            
            expect_that( expt$entity$PUBLISHED,equals(TRUE))
            
            
            response<-experimentUnPublish(coreApi, "SIMPLE EXPERIMENT", "SPX18",useVerbose = FALSE)
            
            expect_that( expt$entity$PUBLISHED,equals(FALSE))
            
            
           
            
          })





  