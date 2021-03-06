
context("Tests for project get and update")


rm(list=ls())

verbose <- FALSE

#setup to work with environment

instance <<- "test_environments/5-2-2.json"



     test_that(paste("test log in and get and set entity project", instance),
            {

              verbose <- FALSE
              api <- CoreAPIV2::coreAPI(instance)

          
              con<- CoreAPIV2::authBasic(api,useVerbose=verbose)

              expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)
              expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))
              
              
              b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE","PS534",fullMetadata=FALSE,useVerbose=verbose)$entity
 
              expect_match(b$Barcode,"PS534",all=verbose)
              
              p<-CoreAPIV2::updateEntityProject(con$coreApi,"PATIENT_SAMPLE","PS534","PJ1",useVerbose=FALSE)
                 
 
              expect_match( httr::http_status(p$response)$category,"Success")
              
              
                            
              p1<-CoreAPIV2::getEntityProject(con$coreApi,"PATIENT_SAMPLE","PS534",useVerbose=FALSE)
              
              expect_match(p1$entity[[1]]$Barcode,"PJ1")
              
              
              p2<-CoreAPIV2::updateEntityProject(con$coreApi,"PATIENT_SAMPLE","PS534",c("PJ1","PJ2"),useVerbose=TRUE)
             
            
              
              expect_match( httr::http_status(p2$response)$category,"Success")
              
              
             p3<-CoreAPIV2::getEntityProject(con$coreApi,"PATIENT_SAMPLE","PS534",useVerbose=FALSE)
              
             expect_match(p3$entity[[1]]$Barcode,"PJ1")
             expect_match(p3$entity[[2]]$Barcode,"PJ2")
             
            
              
              logout<-CoreAPIV2::logOut(con$coreApi,useVerbose = verbose)
              expect_match(logout$success,"Success")

        
              
              })
  
     
    