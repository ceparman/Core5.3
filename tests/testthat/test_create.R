
context("Tests for create")

 

rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test login,create sample and lot", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              out<-CoreAPIV2::getEntityMetadata(con$coreApi,"PATIENT_SAMPLE",useVerbose = FALSE)
              
              
              body<-out$template
              
              body[["SOURCE_LAB"]] <- "ACME"
              
              body[["NUMBER"]] <- 3
              
              body[["REQUESTOR"]] <- "Dr Strange"
              
              
              body[["FILE"]] <- " "
              
              
              body[["SAMPLE_ENZYME@odata.bind"]] <- "/ENZYME('ENZ1')"
              
              return<-CoreAPIV2::createEntity(coreApi = con$coreApi,entityType = "PATIENT_SAMPLE",body=body)
              
              barcode<-return$entity$Barcode
              
               
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE",barcode,useVerbose=verbose)$entity

              expect_match(b$Barcode,barcode,all=verbose)
              
              expect_match(b$SOURCE_LAB,"ACME",all=verbose)
              
              
          #create sample lot
              
              sl<-CoreAPIV2::createSampleLot(con$coreApi,sampleType="PATIENT_SAMPLE",sampleBarcode=b$Barcode,body=NULL,
                                         useVerbose=TRUE)
              
              expect_equal(is.numeric(sl$entity$Sequence),TRUE,all=verbose)
              
              expect_equal( httr::status_code(sl$response),201,all=verbose)
              
              
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
     
     test_that(paste("create object that requires cleaned name for ODATA", instance),
               {
                 
                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)
                 
                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
                 
                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
                 
                 
                 out<-CoreAPIV2::getEntityMetadata(con$coreApi,"384 WELL PLATE",useVerbose = FALSE)
                 
                 
                 body<-out$template
                 return<-CoreAPIV2::createEntity(con$coreApi,"384 WELL PLATE",body=body)
                 
                 barcode<-return$entity$Barcode
                 
                 
                 b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"384 WELL PLATE",barcode,useVerbose=verbose)$entity
                 
                 expect_match(b$Barcode,barcode,all=verbose)
                 
               
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")
                 
               })
     
     
  