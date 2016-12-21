
context("Tests for gets")



rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/Test%205.2.postman_environment.json"



     test_that(paste("test login parameters for environment", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              out<-CoreAPIV2::getEntityMetadata(con$coreApi,"PATIENT_SAMPLE",useVerbose = TRUE)
              
              
              body<-out$template
              
              body[["SOURCE_LAB"]] <- "ACME"
              
              body[["NUMBER"]] <- 3
              
              body[["REQUESTOR"]] <- "Dr Strange"
              
              
              body[["FILE"]] <- " "
              
              
              body[["SAMPLE_ENZYME@odata.bind"]] <- "/ENZYME('ENZ1')"
              
              return<-CoreAPIV2::createEntity(con$coreApi,"PATIENT_SAMPLE",body=body)
              
              barcode<-return$entity$Barcode
              
               
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE",barcode,useVerbose=verbose)$entity

              expect_match(b$Barcode,barcode,all=verbose)
              
              expect_match(b$SOURCE_LAB,"ACME",all=verbose)
              logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
              expect_match(logout$success,"Success")

              })
  
 
     instance <<- "test_environments/Beer.postman_environment.json"



     test_that(paste("test login parameters for environment", instance),
               {

                 verbose <- FALSE
                 api <- CoreAPIV2::coreAPI(instance)


                 con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

                 expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
                 expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))


                 
                 out<-CoreAPIV2::getEntityMetadata(con$coreApi,"BEER",useVerbose = FALSE)
                 
                 
                 body<-out$template
                 
                 body[["CI_BRAND"]] <- "ACME"
                 
                 body[["CI_TIER"]] = "Premium"
                 body[["CI_COLOR"]] = "Dark"
                 body[["CI_TYPE"]] = "Russian Imperial Stout"
                 body[["CI_TARGET_ABV"]] = 10.3
                 body[["CI_TARGET_SED_G_L"]] = 0.79
                 body[["BEER_HOPS@odata.bind"]] = "/HOPS('HOP7')"
                 body[["BEER_FININGS@odata.bind"]]= c("/FININGS('FNG6')", "/FININGS('FNG5')")
                 
                 body[["BEER_MALT@odata.bind"]]= "/MALT('MALT2')"
                 body[["BEER_YEAST@odata.bind"]]= "/YEAST('YST4')"
                 
                 #return<-apiPOST(con$coreApi,resource="BEER",body=body,encode = "json",headers=httr::content_type_json(),special=NULL,useVerbose=TRUE)
                 
                 return<-CoreAPIV2::createEntity(con$coreApi,"BEER",body=body)
                 
                 
                 barcode<-return$entity$Barcode
                 
                 
            
                 b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"BEER",barcode,useVerbose=verbose)$entity

                 expect_match(b$Barcode,barcode,all=verbose)

                 expect_match(b$CI_TYPE,"Russian Imperial Stout",all=verbose)
                 logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
                 expect_match(logout$success,"Success")

               })



