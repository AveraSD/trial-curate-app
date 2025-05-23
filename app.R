library(shiny)
library(reactable)
library(bslib)
library(readxl)
library(here)
library(mongolite)
library(jsonlite)
library(httr)
library(glue)
library(tidyverse)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(shinyjs)
library(config)
library(data.table)
library(dplyr)
library(here)
library(tidyr)
library(fs)
# NCT03037385

# source necessary files
source(here("R", "curateGlobal.R")) 
source(here("R", "queryNCT.R"))
source(here("R", "curateUI.R"))
source(here("R", "curateServer.R"))
source(here("R", "add_trials.R"))

##### UI ######
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), 
  useShinyjs(),
  navbarPage(
    title = "TrialCurate", 
    
    tabPanel("Add Trial",
             
             tabsetPanel(
               id = 'inNav',
               
               # Curate Query
               tabPanel("NCT ID", 
                        input_form),
               
               # Disease 
               tabPanel("Disease",
                        dise_form),
               
               # Biomarker 
               tabPanel("Biomarker",
                        biom_form),
               
               # Documentation
               tabPanel("Documents",
                        doc_form),
               
               # View trial before submitting to database
               tabPanel("View Trial",
                        dis_form )
               
             )),
    # tabPanel(
    #   "Browse Trials"
    # ),
    
    theme = bs_theme(version = 5, 
                     bootswatch = "cosmo",
                     primary = "#246725")
    
  )
)


# Define server logic 
server <- function(input, output, session) {
  #validation check starts
  observeEvent(input$inNav, {
    # Check if the user is leaving Tab 3 without completing info1 or info2
    #  if (input$inNav == "Biomarker" && (input$lineTx == "" || input$armStatus == "")) {
    if (input$inNav == "Documents" && (is.null(input$lineTx) || input$lineTx == "" || is.null(input$armStatus) || input$armStatus == "" || is.null(input$selec) || input$selec == "" ||
                                       is.null(input$gene1) || input$gene1 == "" || is.null(input$typ) || input$typ == ""  || is.null(input$var) || input$var == "" || is.null(input$func) || input$func == "" || is.null(input$gene2) || input$gene2 == "")) {
      shinyjs::alert("Please complete Line of Therapy, Arm status and Biomarker information before proceeding. If no information is available for any of these, then select Not available and click green plus button.")
      
      # Automatically send the user back to Tab 3 if fields are incomplete
      updateTabsetPanel(session, "inNav", selected = "Biomarker")
    }
  })
  #validation check ends
  
  
  #updateSelectizeInput(session, 'var', choices = allVar, server = TRUE)
  #updateSelectizeInput(session, 'gene1', choices = allgenes, server = TRUE)
  ##### Panel 1: NCT ID + query trial
  
  # action after clicking submit
  displatAPI <- eventReactive(input$submit, {
    output$firsthalf <- renderUI({firsthalfUI })
    
    # query NCT
    outAPI <- queryNct(input$info_NCT)
  })
  
  # Display the Query Information Table
  output$responses <- renderReactable({
    info <- displatAPI() %>% 
      select(!(arm)) %>% 
      select(!(criteria)) %>% 
      select(!(link))
    infoquery <- tibble(Information = row.names(t(info)) %>% 
                          str_replace_all("_", " ") %>% 
                          str_to_title(), 
                        Details = t(info))
    
    reactable(infoquery, 
              resizable = TRUE, 
              rownames = FALSE,
              columns = list(
                Information = colDef(width = 200),
                Details = colDef(minWidth = 800)
              ),
              bordered = TRUE, 
              #striped = TRUE, 
              pagination = FALSE)  
  })
  
  # Display the Arm table 
  output$armsOnly <- renderReactable({
    disAd$armDf <- displatAPI()[[11]][[1]] %>% as.data.frame() %>% select(cohortlabel, drug, arm_type)
    armquery <- displatAPI()[[11]][[1]] %>% as.data.frame() %>% select(cohortlabel, drug, arm_type)
    reactable(armquery, 
              compact = TRUE,
              bordered = TRUE, 
              #striped = TRUE, 
              columns = list(
                cohortlabel = colDef(name = "Cohort Label"),
                drug = colDef(name = "Drug(s)"),
                arm_type = colDef(name = "Arm Type")
              ))
  })
  
  
  ##### Panel 2: Disease
  # TABLE 1: Open the Disease Table and display the UI on Next Hit 
  observeEvent(input$next1,{
    updateTabsetPanel(session, "inNav",selected = "Disease")
    output$secondhalf = renderUI({
      secondhalfUI
    })
  })
  
  # disease dropdown selection
  observeEvent(
    input$dise,
    updateSelectInput(session, "lev2", "Level2", 
                      #choices = oncotree$level_2[oncotree$level_1==input$dise]))
                      choices = oncotree_addrows1$level_2[oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev2,
    updateSelectInput(session, "lev3", "Level3",
                      choices = oncotree_addrows1$level_3[oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev3,
    updateSelectInput(session, "lev4", "Level4",
                      choices = oncotree_addrows1$level_4[oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev4,
    updateSelectInput(session, "lev5", "Level5",
                      choices = oncotree_addrows1$level_5[oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  
  observeEvent(
    input$lev5,
    updateSelectInput(session, "lev6", "Level6", 
                      choices = oncotree_addrows1$level_6[oncotree_addrows1$level_5==input$lev5 & oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev6,
    updateSelectInput(session, "lev7", "Level7", 
                      choices = oncotree_addrows1$level_7[oncotree_addrows1$level_6==input$lev6 & oncotree_addrows1$level_5==input$lev5 & oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  
  # event for clearing the disease table 
  
  observeEvent(input$clr_Dis,{
    disAd$indisAd = tibble()
  })
  
  
  # TABLE A: Display the selected Disease and selection 
  observeEvent(input$addDis, {
    
    # to get the maintype for trial match 
   # print(input$lev3)
   actual_type =  oncotree_addrows1 %>% filter(level_1 %in% input$dise & level_2 %in% input$lev2 & level_3 %in% c(input$lev3) & level_4 %in% c(input$lev4) ) %>% dplyr::select(1:8)
   actual_type = apply(actual_type, 1, function(x) paste(x[x != "."], collapse = ", "))
   print(actual_type) 
   
   
   #Adding complete disease information for disease
   addComDis <- tibble(complete_disease = as.character(actual_type))
   disAd$disStr <- disAd$disStr %>% bind_rows(addComDis)
   
   # to get all the other information from disease input 
   allInputDise <- c(input$dise,input$lev2,input$lev3,input$lev4,input$lev5,input$lev6,input$lev7)
    lastInput <- allInputDise[allInputDise != "."]
    lenlast <- length(lastInput)
    
    
    #Adding stage information for disease
    addDisBtn <- tibble(code = lastInput[lenlast], 
                       selection = input$certir,
                        stage = as.character(input$levl_stage))
    
    disAd$indisAd <- disAd$indisAd %>% bind_rows(addDisBtn)
    output$dt_dise <- renderDT({
      disTb_out=disAd$indisAd %>% group_by(code,selection) %>% 
        summarise(
          stage = paste0(stage,collapse = ";")
        )
      datatable(disTb_out, 
                filter = 'none', 
                selection = 'none', 
             #   editable = TRUE,
                options = list(dom = 't'))
    })
  })
  
  
  ##### Panel 3: Biomarker
  # Open the Biomarker Tab and display the UI on Move to Biomarker
  observeEvent(input$disDis,{
    updateTabsetPanel(session, "inNav", selected = "Biomarker")
    output$bioDis <- renderUI({
      biom_display
    })
  })
  
  ## ADD LoT and Status
  # observe and open modal to add common LoT and Status for all cohort arms
  observeEvent(input$add_allArmLotStatus,{
    output$TEXTA_arminfo <- renderText({
      "Enter Line of Therapy and recruitment status"
    })
    modal_arminfo(lineTx = "", armStatus = "")
    disAd$add_or_edit_arminfo <- 1
    
  })
  
  # TABLE 1: table shows cohort arms to enter line of therapy and arm status
  output$dt_table_arm <- renderDataTable({
    butns_arminfo <- create_btns_arminfo(nrow(disAd$armDf))
    arm_info <- disAd$armDf %>% as.data.frame() %>% select(cohortlabel, drug, arm_type) %>%
      rownames_to_column(var = "ArmID") %>% 
      bind_cols(tibble("armadd" = butns_arminfo)) 
    datatable(arm_info, 
              escape = F,
              selection ='single',
              rownames = FALSE,
              #editable = TRUE,
              colnames = c('Arm #' = 'ArmID',
                           'Cohort(s)' = 'cohortlabel',
                           'Drugs(s)' = 'drug',
                           'Arm Type' = 'arm_type',
                           'Add Arm Info' = 'armadd'),
              options = list(processing = FALSE, 
                             dom = 't',pageLength = 15)
    )
  })
  
  
  
  # event for clearing arm selection 
  observeEvent(input$clr_Arm,{
    disAd$armDfInfo = tibble()
  })
  
  
  # TABLE A: when specific row is selected - add LoT & Status
  observeEvent(input$current_id, {
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "arminfo")) {
      selRow <- disAd$armDf[input$dt_table_arm_rows_selected, ]
      cohotLb <- selRow[["cohortlabel"]]
      output$TEXTA_arminfo <- renderText({
        cohotLb
      })
      modal_arminfo(lineTx = "", armStatus = "")
    }
  })
  
  observeEvent(input$final_edit_arminfo, {
    shiny::removeModal()
    armdf <- disAd$armDf %>% rownames_to_column(var = "ArmID")
    selarm <- armdf[input$dt_table_arm_rows_selected, ]
    cohortID <- selarm[["ArmID"]]
    cohort <- selarm[["cohortlabel"]]
    dt_row <- tibble(
      armID = cohortID,
      cohortlabel = cohort,
      lineTx = input$lineTx,
      armStatus = input$armStatus
    )
    disAd$armDfInfo <- disAd$armDfInfo %>% bind_rows(dt_row) %>% distinct()
    
    
    
    #disAd$armDfInfo <- inner_join(disAd$armDfInfo, dt_row, by = "cohortlabel")
    output$dt_table_arm_display <- renderDT({
      disAd$Armpt1Tb_out=disAd$armDfInfo %>% group_by(armID,cohortlabel,armStatus) %>% 
        summarise(
          lineTx = paste0(lineTx,collapse = ";")
        )
      datatable(disAd$Armpt1Tb_out, 
                rownames = F,
                options = list(dom = 't'))
    })
    
    proxy <- dataTableProxy("dt_table_arm_display")
    proxy %>% selectRows(NULL)
  })
  
  # TABLE A: when (+Add common LoT & Status) is chosen
  observeEvent(input$final_edit_arminfo, {
    shiny::req(disAd$add_or_edit_arminfo == 1)
    shiny::removeModal()
    armdf <- disAd$armDf %>% rownames_to_column(var = "ArmID")
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(e),
        lineTx = input$lineTx,
        armStatus = input$armStatus,
        cohortlabel = as.character(armdf[e,2]))
      
      disAd$armDfInfo <- disAd$armDfInfo %>% bind_rows(dt_rowall) %>% distinct()
    }
    output$dt_table_arm_display <- renderDT({
      disAd$Armpt1Tb_out=disAd$armDfInfo %>% group_by(armID,cohortlabel,armStatus) %>% summarise(lineTx = paste0(lineTx,collapse = ";"))
      datatable(disAd$Armpt1Tb_out, 
                rownames = FALSE,
                colnames = c('Arm #' = 'armID',
                             'Cohort' = 'cohortlabel'),
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_table_arm_display")
    proxy %>% selectRows(NULL)
  })
  
  ## ADD biomarker
  # observe and open modal to add common biomarker to cohort arms
  observeEvent(input$add_allBio,{
    output$TEXTA <- renderText({
      "Enter biomarkers common to all cohort arms"
    })
    modal_biomarker(selec = "", gene1 = "",   typ = "", var = "", func = "", gene2 = "")
    disAd$add_or_edit <- 1
    
  })
  
  # TABLE 2: table shows cohort arms to enter corresponding biomarkers
  output$dt_table <- renderDataTable({
    butns_biomarker <- create_btns_biomarker(nrow(disAd$armDf))
    armterm <- disAd$armDf %>% as.data.frame() %>% select(cohortlabel, drug, arm_type) %>%
      rownames_to_column(var = "ArmID") %>% 
      bind_cols(tibble("Buttons" = butns_biomarker))
    datatable(armterm, 
              escape = F,
              selection ='single',
              rownames = FALSE,
              colnames = c('Arm #' = 'ArmID',
                           'Cohort(s)' = 'cohortlabel',
                           'Drugs(s)' = 'drug',
                           'Arm Type' = 'arm_type',
                           'Add Biomarker' = 'Buttons'),
              options = list(processing = FALSE, 
                             dom = 't',pageLength = 15)
    )
  })
  
  #event for clearing Biomarker arm selection 
  observeEvent(input$clr_Bio,{
    disAd$dfAdd = tibble()
  })
  
  # TABLE B: when specific row is selected - add biomarker
  observeEvent(input$current_id,{
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit")) {
      selRow <- disAd$armDf[input$dt_table_rows_selected,]
      cohotLb <- selRow[["cohortlabel"]]
      output$TEXTA <- renderText({
        cohotLb
      })
      modal_biomarker(selec = "", gene1 = "",  typ = "", var = "",  func = "" , gene2 = "")
    }
  })
  
  
  
  
  observeEvent(input$final_edit, {
    shiny::removeModal()
    armdf <- disAd$Armpt1Tb_out
    #armdf <- disAd$armDfInfo %>% group_by(armID,cohortlabel,armStatus) %>% summarise(lineTx = paste0(lineTx,collapse = ";"))
    #selarm <- armdf[input$dt_table_rows_selected,] # change to select by cohortlabel
    selarm <- armdf %>% filter(cohortlabel %in% armdf[input$dt_table_rows_selected, "cohortlabel"])
    armID <- selarm[["armID"]]
    cohortlabel <- selarm[["cohortlabel"]]
    lineTx <- selarm[["lineTx"]]
    armSt <- selarm[["armStatus"]]
    dt_row <- tibble(
      armID = armID,
      cohortlabel = cohortlabel,
      lineTx = lineTx,
      armStatus = armSt,
      Gene = input$gene1,
      Gene2 = input$gene2,
      Type = input$typ, 
      Variant = input$var,
      Selection = input$selec,
      Function = input$func)
    
    disAd$dfAdd <- disAd$dfAdd %>% bind_rows(dt_row) %>% distinct()
    
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = F,
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_biomark")
    proxy %>% selectRows(NULL)
  })
  
  
  # TABLE B: when (+Add common biomarker) is chosen
  observeEvent(input$final_edit, {
    shiny::req(disAd$add_or_edit == 1)
    shiny::removeModal()
    armdf <- disAd$Armpt1Tb_out
    #armdf <- disAd$armDfInfo %>% group_by(armID,cohortlabel,armStatus) %>% summarise(lineTx = paste0(lineTx,collapse = ";"))
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(armdf[e, "armID"]),
        cohortlabel = as.character(armdf[e, "cohortlabel"]),
        lineTx = as.character(armdf[e, "lineTx"]),
        armStatus = as.character(armdf[e, "armStatus"]),
        Gene = input$gene1,
        Gene2 = input$gene2,
        Type = input$typ, 
        Variant = input$var, 
        Selection = input$selec, 
        Function = input$func)
      
      disAd$dfAdd <- disAd$dfAdd %>% bind_rows(dt_rowall) %>% distinct()
    }
    
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = FALSE,
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_biomark")
    proxy %>% selectRows(NULL)
    add_or_edit = NULL
  })
  
  
  ### remove edit modal when close or submit button is clicked
  observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  observeEvent(input$dismiss_modal_arminfo, {
    shiny::removeModal()
  })
  
  ##### Panel 4: Documentation 
  # Open the Document Tab and display the UI on Update
  observeEvent(input$bioMrk,{
    updateTabsetPanel(session, "inNav", selected = "Documents")
    output$DisDoc <- renderUI({
      docuOut 
    })
    output$doc_link <- renderText({input$doc})
    
  })
  

  # docInput = eventReactive(input$doc_fileType,{
  #   if(input$doc_fileType == "Flat File") {
  #       docs = input$doc
  #     } else
  #     {
  #       tagvar = tags$a(href=input$doc,)
  #       docs = as.character(tagvar)
  #     }
  #   print(docs)
  # })
  
  
  ##### Panel 5: View Trial
  observeEvent(input$move_brow,{
    updateTabsetPanel(session, "inNav", selected = "View Trial")
    output$DisBrow = renderUI({
      browserOut
    })
  })
  
  # Display the Query Information Table
  output$displayBio <- renderReactable({
    
    # save all the variables to their appropiate values 
    infoDis <- displatAPI() %>% 
      select(!(arm))
    
    # save the arm info from query output
    armTb <- left_join(disAd$armDf, disAd$Armpt1Tb_out, by = "cohortlabel")
    armTb <- armTb %>% rownames_to_column(var = "ArmID")
    armTb <- tibble(
      ArmID = armTb$ArmID,
      cohortlabel = armTb$cohortlabel,
      drug = armTb$drug,
      arm_type = armTb$arm_type,
      line_of_therapy = armTb$lineTx,
      arm_hold_status = armTb$armStatus
    )
    
    # save the disease info entered
    tempDisease=disAd$indisAd %>% group_by(code,selection) %>%  summarise(stage = paste0(stage,collapse = ";"))
    DisTab <- as_tibble(tempDisease)
    
    DisComplt = as_tibble(disAd$disStr)
    
    # save the biomarker info entered
    
    bioMarkTb <- as_tibble(disAd$dfAdd)
    
    tb_add <- bioMarkTb %>%  mutate(summary = "") %>% mutate( summary = paste0(Gene," ",Gene2, " ",Variant, " ",Type, " ", Function) )
    tb_add$summary <- gsub( "Not available", "", as.character(tb_add$summary) )
   # tb_add <- tb_add %>% mutate(summary = case_when(!is.na(summary) ~ summary, summary == '\\s+' ~ paste0("absent") ) )
    #%>% mutate( summary = gsub( "Not available", "", summary) )
      #   case_when(
      #   # Mutation variant based
      #   Gene != "Not available" & Type != "Not available" & Variant != "Not available" & Function != "Not available"~ paste(Gene, Variant, Function, .sep = " "),
      #   Gene != "Not available" & Type != "Not available" & Variant != "Not available" & Function == "Not available"~ paste(Gene, Variant, Type, .sep = " "),
      #   
      #   # Expression, fusion, CNA, etc  
      #   Gene != "Not available" & Type != "Not available" & Variant == "Not available" & Function != "Not available" ~ paste(Gene, Type, Function, .sep = " "),
      #   Gene != "Not available" & Type != "Fusion" & Variant == "Not available" & Function == "Not available" ~ paste(Gene, Type, .sep = " "),
      #   Type == "Fusion" & Gene != "Not available" & Gene2 != "Not available" & Variant == "Not available" ~ paste0(Gene,"-", Gene2," ", Type),
      #   Type == "Fusion" & Gene != "Not available" & Gene2 == "Not available" & Variant == "Not available" ~ paste0(Gene," ", Type),
      #   
      #   # without gene 
      #   Gene == "Not available" & Type != "Not available" & Variant == "Not available" ~ paste(Type, Function, .sep = " "),
      #   
      #   # when empty 
      #   Gene == "Not available" & Type == "Not available" & Variant == "Not available" & Function == "Not available" ~ paste("absent")
      #   
      # )
#    )
    #colnames(tb_add)
    print(tb_add$summary)
    tb_add = tb_add[,c(1:2,5:11)]
    
    # adding the biomarker tibble to the respective cohort 
    alltoAmBK = left_join(armTb, tb_add, by = c('ArmID' = 'armID'))
    #print(colnames(alltoAmBK))
    colnames(alltoAmBK) = c("ArmID", "cohortlabel", "drug" ,"arm_type" ,"line_of_therapy", "arm_hold_status",          
                            "cohort", "Gene" , "Gene2", "Type" , "Variant","Selection", "Function" ,"summary" )
    
    armForBioMk = alltoAmBK %>% group_by(ArmID, cohortlabel, drug ,arm_type ,line_of_therapy, arm_hold_status ) %>% nest()
    armForBioMk = setnames(armForBioMk, "data", "biomarker")
    
    
    # ----------------------------------------------------------------------------------------- # 
    # making the data tibble for each trial entry
    
    # final tibble to display  
    disBrw2 <<- tibble(
      info = tibble(NCT = input$info_NCT,
                    Protocol_No = input$info_protNo,
                    jit = input$info_jit,
                    trial_name = input$info_trial_name,
                    
                    Principal_Investigator = input$info_Principal_Investigator,
                    
                 #   disease_category = input$info_disease_cat - commented oct' 27th
                 #   disease_category = paste0(input$info_disease_cat, collapse = ",") 
                 #changing to semicolon for collapsing
                 disease_category = paste0(input$info_disease_cat, collapse = ";")
                 
      ),
      disease = tibble(summary = input$disSum,
                       details = list(DisTab),
                       disease_complete = list(DisComplt)
      ),
      query = tibble(nct = input$info_NCT,
                     title = infoDis$title,
                     current_status = infoDis$current_status,
                     status_verif_date = infoDis$status_verif_date,
                     last_update_date = infoDis$last_update_date,
                     trial_hold_status = input$trHold,
                     sponsor = infoDis$sponsor,
                     brief_summary = infoDis$brief_summary,
                     conditions = infoDis$conditiions,
                     type = infoDis$type,
                     phase = infoDis$phase,
                     arm = list(armForBioMk),
                     docs = HTML(paste(a("Protocol",href=input$doc, target="_blank"))),
                     doclastupdate = input$dt,
                  #   locations = input$loct,   << original working for textinput multiple or selectinput single selection
                     locations = paste0(input$loct,collapse = ";"),
                     min_age = infoDis$min_age,
                     gender = infoDis$gender,
                     link = infoDis$link
      )
    )
    
    #"<a href=\\", input$doc, "\\", "target=\"_blank\">site-documentation</a>"
    
    view_trial_table <- reactable(disBrw2 %>%
                                    unnest(c(info, disease, query)) %>%
                                  #  select(NCT:trial_name),
                           #displaying also the new variable disease category       
                                  select(NCT:disease_category),
                                  resizable = TRUE,
                                  style = list(minWidth = 800),
                                  fullWidth = TRUE,
                                  defaultExpanded = TRUE,
                                  details = function(index) {
                                    
                                    tab <- disBrw2 %>%
                                      unnest(c(info, disease, query)) %>%
                                      select(-(NCT:nct), -arm)
                                    
                                    tab_arms <- disBrw2 %>%
                                      unnest(c(info, disease, query)) %>%
                                      select(arm) %>%
                                      unnest(arm) %>%
                                      select(-cohortlabel) %>%
                                      unnest(biomarker) %>%
                                      select(-c(ArmID))
                                    #line_of_therapy, arm_hold_status
                                    
                                    tab_disease <- disBrw2$disease %>% unnest(c(details,disease_complete) )
                                    # tab_disease <- disBrw2 %>%
                                    #   unnest(c(info, disease, query)) %>%
                                    #   select(summary:details) %>%
                                    #   unnest(details)
                                    
                                    htmltools::div(style = "padding: 16px",
                                                   reactable(tab %>% t(),
                                                             #columns = c("Key", "Value"),
                                                             #rownames = FALSE,
                                                             outlined = TRUE,
                                                             pagination = FALSE),
                                                   reactable(tab_arms, outlined = TRUE),
                                                   reactable(tab_disease, outlined = TRUE)
                                    )
                                  }
    )
    
    disAd$rsdf <- disBrw2
    view_trial_table
    
  }
  )
  
  resetAll <- function() {
    reset("info_NCT")
    reset("info_jit")
    reset("info_trial_name")
    #firsthalfUI <- NULL
    reset("infoquery")
    
    #responses <- NULL
    reset("armsOnly")
    disAd$indisAd = tibble() # disease  
    disAd$disStr = tibble()
    disAd$armDf = tibble() # cohort 
    disAd$armDfInfo = tibble() # cohort + arm info
    #disAd$armDfBiomarker = tibble() # cohort + arm info + biomarker
    disAd$dfAdd = tibble() # cohort + biomarker
    add_or_edit = NULL  # confirming the button selection
    add_or_edit_arminfo = NULL
    disAd$rsdf = tibble() 
  }
  
  
  observeEvent(input$confirm1,{
    outSubmit()
    alert("Submitted successfully!")
    refresh()
    resetAll()
    
    
  })
  # observeEvent(input$final_confirm,{
  #   print(disAd$resultsdf)
  # })
  
  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$final_cancel, {
    shiny::removeModal()
  })
}

# NCT03037385

##### APP ######
shinyApp(ui, server)
