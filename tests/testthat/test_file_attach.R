
context("Tests for create and attach a file to the entity")

 

rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



     test_that(paste("create a sample and attach a file", instance),
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
              
              
              body[["FILE"]] <- NULL
              
              body[["IMAGE_FILE"]] <- NULL
              
              body[["SAMPLE_ENZYME@odata.bind"]] <- "/ENZYME('ENZ1')"
              
              return<-CoreAPIV2::createEntity(con$coreApi,"PATIENT_SAMPLE",body=body)
              
              barcode<-return$entity$Barcode
              
               
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE",barcode,useVerbose=verbose)$entity

              expect_match(b$Barcode,barcode,all=verbose)
             
              expect_match(b$SOURCE_LAB,"ACME",all=verbose)
              
              
              filepath <- "test_files/trouble.PNG"
              filename <- "pic.PNG"
              
              r<-CoreAPIV2::attachFile(con$coreApi,b$Barcode,filename,filepath,targetAttributeName="",useVerbose=FALSE)
             
             
              
              expect_equal(grep(pattern = "pic.PNG.[0-9]",r$entity$name),1)
              
              expect_equal( httr::status_code(r$response),200)
              
              
              logout<-CoreAPIV2::logOut(con$coreApi,useVerbose = verbose)
              expect_match(logout$success,"Success")
              
             
              })
     
    