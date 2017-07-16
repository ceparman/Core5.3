
context("Tests for raw data")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/dose.json"



     test_that(paste("test get expt., expt. sample, raw and intermediate data", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              #get the experiment
              
              expt <- CoreAPIV2::getEntityByBarcode(con$coreApi,"BIOCHEMICAL DOSE RESPONSE EXPERIMENT",barcode = "BDR16",useVerbose = verbose)
              
              expect_match(expt$entity$Barcode,"BDR16",all=verbose)
              
              exptSamples <- CoreAPIV2::getExperimentSamples(con$coreApi,"BIOCHEMICAL DOSE RESPONSE EXPERIMENT",barcode = "BDR16",useVerbose = verbose)
              
              exptSampleBarcode = exptSamples$entity[1]
              
              #getRaw data
              
              rawData <- CoreAPIV2::getExperimentSamplesRawData(con$coreApi,experimentType = "BIOCHEMICAL DOSE RESPONSE EXPERIMENT",
                                                                assayType = "BIOCHEMICAL_DOSE_RESPONSE_ASSAY",
                                                                experimentSamplebarcode =  exptSampleBarcode,useVerbose = verbose)
              
              
              expect_equal(nrow(rawData$entity),10,all=verbose)
              
              #intermediate data is in assay data
              
              ad<-CoreAPIV2::getExperimentSamplesAssayData(api,"BIOCHEMICAL_DOSE_RESPONSE_ASSAY",
                                                       experimentSamplebarcode = exptSamples$entity[1])
              
              
              expect_match(ad$entity[[1]]$Barcode,exptSamples$entity[1],all=verbose)
              
              logout<-CoreAPIV2::logOut(con$coreApi,useVerbose = verbose)
              expect_match(logout$success,"Success")

        
              
              })
  
     
    