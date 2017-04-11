
context("Tests for experiments")



rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



test_that(paste("test login,create sample and lot", instance),
          {
            
            #log in
            
            account<-CoreAPIV2::coreAPI(instance)
            
            con<-CoreAPIV2::authBasic(account)
            
            coreApi<-con$coreApi
        
            useVerbose <- TRUE
            
            #create Experiment
            
            experimentType <-"SIMPLE EXPERIMENT"
            
            protocolType <-"SIMPLE EXPERIMENT PROTOCOL" 
            assayType <-"SIMPLE ASSAY"
            
            assayBarcode <- "SA1"
            
            protocolBarcode <-"ZPX1"
            
            body <- list(EXPT_ATTRIBUTE = "12")
            
            expt<-CoreAPIV2::createExperiment(coreApi,experimentType,assayType,assayBarcode,protocolType,
                                              protocolBarcode,body=body,useVerbose=useVerbose)
            
            expect_that( httr::http_status(expt$response)$reason,equals("Created"))
            
            experimentBarcode <- expt$entity$Barcode
           
            #test publishing  
           
            expect_that( expt$entity$PUBLISHED,equals(FALSE))
            
            body<-list(CI_CONC_NM = 10.22,CI_CONC_UNIT = "mM" )
            
            
            #add a sampe lot
            
            es<-CoreAPIV2::createExperimentSample(coreApi,experimentType,experimentBarcode,
                                              "PS1-1",body=body,useVerbose=useVerbose)
            
            
            
            expect_that(httr::http_status(es$response)$message  ,equals("Success: (201) Created"))
            
            #check that sample lot is there
            
            es<-CoreAPIV2::getExperimentSamples(coreApi,experimentType,experimentBarcode ,useVerbose=FALSE)
            
            
            expect_that(length(es$entity)  ,equals(1))
           
             #add single well container
            
            ec<-CoreAPIV2::createExperimentContainer(coreApi,experimentType,experimentBarcode,"VIA1",body=NULL,useVerbose=FALSE)
            
            expect_that(httr::http_status(ec$response)$category,equals("Success"))
            
            #add multi well container
            
            ec<-CoreAPIV2::createExperimentContainer(coreApi,experimentType,experimentBarcode,"TE1",body=NULL,useVerbose=FALSE)
            
            
            expect_that(httr::http_status(ec$response)$category,equals("Success"))
            
            
            
            response<-CoreAPIV2::experimentPublish(coreApi, "SIMPLE EXPERIMENT",  experimentBarcode,useVerbose = FALSE)
            
            updatedExpt<-CoreAPIV2::getEntityByBarcode(coreApi,entityType = "SIMPLE EXPERIMENT",barcode =  experimentBarcode)
            
            
            response$entity$values$PUBLISHED
            
            expect_that( updatedExpt$entity$PUBLISHED,equals(TRUE))
            
            
            
            #check that all samples are there
            
            es<-CoreAPIV2::getExperimentSamples(coreApi,experimentType,experimentBarcode ,useVerbose=useVerbose)
            
            
            expect_that(length(es$entity)  ,equals(6))
            
            #check for the containers
            
            
            ec<-CoreAPIV2::getExperimentContainers (coreApi,experimentType,experimentBarcode,useVerbose=useVerbose)
            
            expect_that(length(ec$entity)  ,equals(2))
            
            
            #publish and wrap up
            
          
            
            response<-CoreAPIV2::experimentUnPublish(coreApi, "SIMPLE EXPERIMENT",  expt$entity$Barcode,useVerbose = FALSE)

            
            updatedExpt<-CoreAPIV2::getEntityByBarcode(coreApi,entityType = "SIMPLE EXPERIMENT",barcode = expt$entity$Barcode)
            
            
            
  
            expect_that( updatedExpt$entity$PUBLISHED,equals(FALSE))
            
          
            
           
            
          })





  