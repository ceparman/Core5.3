


context("Tests for create sample, create sample lot, create container, put lot in container cell")



rm(list=ls())

verbose <- FALSE   

#setup to work with environment

instance <<- "test_environments/5-2-2.json"




test_that("full sample to container cycle",
          {
            #log in
            
            verbose <- FALSE
            api <- CoreAPIV2::coreAPI(instance)
            
            
            con<- CoreAPIV2::authBasic(api,useVerbose=verbose)
            
            expect_match(api$coreUrl,con$coreApi$coreUrl,all=verbose)  #1
            expect_that(is.null(con$coreApi$jsessionId),equals(FALSE))  #2
            
            
            out<-CoreAPIV2::getEntityMetadata(con$coreApi,"PATIENT_SAMPLE",useVerbose = verbose)
            
            
            body<-out$template
            
            
            body[["SOURCE_LAB"]] <- "ACME"
            
            body[["NUMBER"]] <- 3
            
            body[["REQUESTOR"]] <- "Dr Strange"
            
            
            body[["CI_FILE"]] <- NULL
            body[["IMAGE_FILE"]] <- NULL
            
            body[["SAMPLE_ENZYME@odata.bind"]] <- "/ENZYME('ENZ1')"
            
            return<-CoreAPIV2::createEntity(coreApi = con$coreApi,entityType = "PATIENT_SAMPLE",
                                            body=body,useVerbose = verbose)
            
            barcode<-return$entity$Barcode
            
            
            b<-CoreAPIV2::getEntityByBarcode(con$coreApi,"PATIENT_SAMPLE",barcode,useVerbose=verbose)$entity
            
            expect_match(b$Barcode,barcode,all=verbose) #3
            
            expect_match(b$SOURCE_LAB,"ACME",all=verbose)  #4
            
            
            #create sample lot
            
            sl<-CoreAPIV2::createSampleLot(con$coreApi,sampleType="PATIENT_SAMPLE",sampleBarcode=b$Barcode,body=NULL,
                                           useVerbose=verbose)
            
            expect_equal(is.numeric(sl$entity$Sequence),TRUE,all=verbose) #5
            
            expect_equal( httr::status_code(sl$response),201,all=verbose) #6
            

            #create container

            cont <-CoreAPIV2::createEntity(con$coreApi,"VIAL",body=jsonlite::fromJSON("{}"))  

            cont_barcode <- cont$entity$Barcode

            #fill container


            
            
            
             setCont<-CoreAPIV2::setWellContents(coreApi = con$coreApi,containerType = 'VIAL',
                                                 containerBarcode = cont_barcode,
                                                 containerWellNum = "1",sampleLotType = "PATIENT SAMPLE LOT",
                                                 sampleLotBarcode = "PS1-1",amount = 1,
                                                 amountUnit = "ml",concentration =1,concentrationUnit = "nM",
                                                 useVerbose=verbose)
             
            # #Get cell contents
             
             wc1<- CoreAPIV2::getWellContents(con$coreApi, "VIAL", cont_barcode, 1, useVerbose = verbose)
             
             
             expect_equal(wc1$entity$CI_AMOUNT,1,all=verbose) #7
             
             expect_equal(wc1$entity$CONTENT[[1]]$CI_CONC,1,all=verbose) #8
            # 
            # 
           
        
          #   # create new container
           
             cont2 <- CoreAPIV2::createEntity(con$coreApi,"VIAL",body=jsonlite::fromJSON("{}")) 
             cont2_barcode <- cont2$entity$Barcode
          # 
             #get cell IDs
          
           
           contWell <- CoreAPIV2::getWellContents(con$coreApi,containerType = "VIAL",cont_barcode,"1",
                                                 useVerbose = verbose)
          sourceCellID <-contWell$entity$Id
       
          print(sourceCellID)    
            cont2Well<- CoreAPIV2::getWellContents(con$coreApi,containerType = "VIAL",cont2_barcode,"1",
                                                     useVerbose = verbose)
          
          destWellID <- cont2Well$entity$Id
             
             
           

          # 
          #   #Get amounts move all
          # 
          amount <- contWell$entity$CI_AMOUNT
          # 
          amountUnit <- contWell$entity$CI_AMOUNT_UNIT
          # 
          # 
          # 
         concentration <- contWell$entity$CONTENT[[1]]$CI_CONC
          # 
          # 
        concentrationUnit <- contWell$entity$CONTENT[[1]]$CI_CONC_UNIT
          # 
          #   #now transfer contents
          # 
          #
        
         ## stuck here - both JSOM and ODATA API don't support this
        
         #tc<-transferCellContents(con$coreApi,sourceCellID,destCellID,amount,concentration,
        #                                   amountUnit, concentrationUnit,useVerbose = FALSE)
          # 
          
          logout<-CoreAPIV2::logOut(api,useVerbose = verbose)
          expect_match(logout$success,"Success")
          })
          # 
          #   expect_equal(object=tc$entity$cells[[1]]$cellContents[[1]]$lotBarcode, expected=lot_barcode)
          # 
          #   #test lineage
          # 
          # 
          #  lineage<-CoreAPI::getContainerLineage(r$coreApi,cont2$entity$barcode)
          # 
          #  expect_equal(object=lineage$entity$parents[[1]]$name,
          #               expected=cont$entity$barcode )
          # 
          #   out<-CoreAPI::logOut(r$coreApi)
          # })
          # 
          # 
          # 
          # 
          # 
