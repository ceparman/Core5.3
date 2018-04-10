
context("Tests for raw and intermediate data")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/beer2.json"



     test_that(paste("test get expt., expt. sample, raw and intermediate data", instance),
            {

              verbose <- TRUE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              
              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              #get the experiment
              
              expt <- CoreAPIV2::getEntityByBarcode(con$coreApi,"BITTERNESS EXPERIMENT",barcode = "BTXP2",useVerbose = verbose)
              
              expect_match(expt$entity$Barcode,"BTXP2",all=verbose)
              
              exptSamples <- CoreAPIV2::getExperimentSamples(con$coreApi,"BITTERNESS EXPERIMENT",barcode = "BTXP2",
                                                               useVerbose = verbose)
              
              exptSampleBarcode = exptSamples$entity[1]
              # get expt. container
              exptContainer <- CoreAPIV2::getExperimentContainers(con$coreApi,"BITTERNESS EXPERIMENT",barcode = "BTXP2",
                                                                  useVerbose = verbose)
              
              #get raw data
              
              rawData <- getExperimentSamplesRawData(con$coreApi, exptContainer$entity[1],useVerbose = FALSE)
                  
              
              expect_equal(nrow(rawData$entity),96,all=verbose)
              
              #update raw data
              
              
              rdUpdate <-
                updateExperimentSampleRawData(
                  con$coreApi,
                  exptContainer$entity[1],
                  1,
                  values = list(DATA_VALUE = 100 ,CI_ACCEPT = FALSE),
                  useVerbose = FALSE
                )
                  
              expect_equal(rdUpdate$entity$DATA_VALUE,100,all=verbose)
              expect_equal(rdUpdate$entity$CI_ACCEPT,FALSE,all=verbose)
              
              #change it back to original value
              
              
              rdUpdate2 <-
                updateExperimentSampleRawData(
                  con$coreApi,
                  exptContainer$entity[1],
                  1,
                  values = list(DATA_VALUE = as.numeric(rawData$entity[rawData$entity$CI_CELL ==1,]$DATA_VALUE),
                                 CI_ACCEPT =as.logical(rawData$entity[rawData$entity$CI_CELL ==1,]$CI_ACCEPT)),
                  useVerbose = FALSE
                )
              
              expect_equal(rdUpdate2$entity$DATA_VALUE,
                           as.numeric(rawData$entity[rawData$entity$CI_CELL ==1,]$DATA_VALUE) ,all=verbose)
              expect_equal(rdUpdate2$entity$CI_ACCEPT,
                           as.logical(rawData$entity[rawData$entity$CI_CELL ==1,]$CI_ACCEPT),all=verbose)
              
              
              
              
              
              #get intermediate data
              
              intermediateData <- CoreAPIV2::getExperimentSamplesIntermediateData(con$coreApi,experimentType = "BITTERNESS EXPERIMENT",
                                                                assayType = "BTNA1",
                                                                experimentSamplebarcode =  exptSampleBarcode,
                                                                dataName = "%i", useVerbose = verbose)
              
              #This line may need to be changed since we changed the tennent and exp type
              expect_equal(nrow(rawData$entity),96,all=verbose)
              
              #assay data is in assay data
                
              ad<-CoreAPIV2::getExperimentSamplesAssayData(coreApi = con$coreApi,assayType = "BITTERNESS EXPERIMENT",
                                                        experimentSamplebarcode = exptSamples$entity[1])
              
              
              expect_match(ad$entity[[1]]$Barcode,exptSamples$entity[1],all=verbose)
              
              logout<-CoreAPIV2::logOut(con$coreApi,useVerbose = verbose)
              expect_match(logout$success,"Success")

        
              
              })
  
     
    