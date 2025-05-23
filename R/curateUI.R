## ui side utils
library(reactable)
library(shinyWidgets)
library(shinyFiles)
##### Panel 1: NCT ID + query trial
input_form <- fluidRow(
  
  # note: columns should add upto 12
  wellPanel(
    fluidRow(
      column(4, 
             textInput("info_NCT", 
                       "1. NCT Trial ID")),
      column(4, 
             textInput("info_protNo", 
                       "2. Protocol Number")),
      column(4, 
             selectInput("info_jit", 
                         "3. Trials offered by", 
                         c("Tempus", "Caris", "Optimal", "Avera Trials", "Avera Trials (Basket)"),selected = "")),
      column(4, 
             textInput("info_trial_name", 
                       "4. Name of the trial")),
      
      # column(4, 
      #        selectInput("info_disease_cat", 
      #                  "4. Disease category",
      #                  c("Heme: Lymphoma", "Heme: Leukemia","Heme: Multiple Myeloma","Heme: Other", "Other", "Brain","Breast","Genitourinary", "Gastrointestinal","Gynecology","Head & Neck",
      #                    "Lung","Melanoma","Prostate"))),
     # New with multiple selection oct 27th
      # column(4, 
      #        selectInput("info_disease_cat", 
      #                    "5. Disease category",
      #                    c("Heme: Lymphoma", "Heme: Leukemia","Heme: Multiple Myeloma","Heme: Other","Heme/Precision: Non-Treatment","MedOnc: Lung","MedOnc: Melanoma","MedOnc: Prostate","MedOnc: GU","MedOnc: Renal","MedOnc: GEJ/Gastric", "MedOnc: Pancreas", "MedOnc: Colorectal/Anal","MedOnc: Bladder", "MedOnc: Head & Neck","MedOnc: Brain","MedOnc: Solid Tumor","MedOnc/Precision: Non-Treatment","MedOnc/Precision: Genomic/Molecular Profiling","Gyn: Breast",
      #                      "Gyn: Cervical", "Gyn: Endometrial","Gyn: Ovarian","Gyn: Vulva","Gyn: Solid Tumor","Gyn/Precision: Non-Treatment","Gyn/Precision: Genomic Profiling", "Precision: Retrospective Chart Review"
      #                      ), selected = "", multiple = T)),
      
     # Revising disease categories for MedOnc and arranging in alphabetical order
     column(4, 
            selectInput("info_disease_cat", 
                        "5. Disease category",
                        c("Gyn: Breast",
                          "Gyn: Cervical", "Gyn: Endometrial","Gyn: Ovarian","Gyn: Vulva","Gyn: Solid Tumor","Gyn/Precision: Non-Treatment","Gyn/Precision: Genomic Profiling","Heme: Lymphoma", "Heme: Leukemia","Heme: Multiple Myeloma","Heme: Other","Heme/Precision: Non-Treatment",
                          "MedOnc: Anal","MedOnc: Bladder/Urothelial","MedOnc: Brain","MedOnc: Colon","MedOnc: Colorectal","MedOnc: Esophageal","MedOnc: Gastric","MedOnc: GEJ","MedOnc/Precision: Genomic/Molecular Profiing","MedOnc: Head & Neck","MedOnc: Lung","MedOnc: Melanoma","MedOnc/Precision: Non-Treatment","MedOnc : Pancreas","MedOnc: Prostate","MedOnc : Rectal","MedOnc: Renal/Kidney","MedOnc : Solid Tumor",
                           "Precision: Retrospective Chart Review"
                        ), selected = "", multiple = T)),
      
     column(4, 
            selectInput("info_Principal_Investigator", 
                        "6. Principal Investigator",
                        c("Not available","Dr. Andrade Gonzalez", "Dr. Jehangir","Dr. Arneson","Dr. Grow","Dr. Figura","Dr. Murphy","Dr. Mendez-Hernandez", "Dr. Huber", "Dr. Solomon", "Dr. Vaca","Dr. McKean", "Dr. Higgins","Dr. Sanford","Dr. Spanos","Dr. Jameson","Dr. Jones",
                          "Dr. Rojas-Espaillat", "Dr. Starks","Dr. Bidus","Dr. Merrigan","Dr. Bryan","Dr. Villanueva", "Dr. Conklin", "Dr. Lee"
                        ), selected = "", multiple = F)),
     
     
     
      
      br()
    ),
    
    actionButton("submit", "SEARCH")
  ),
  uiOutput("firsthalf"),
  
  # Query Info display
  br(),
  br(),
  br(),
  br(),
  
  h5(strong("A: General Information")),
  wellPanel(
    reactableOutput("responses")
  ),
  
  br(),
  br(),
  
  # Arm info Display
  h5(strong("B: Arm Information")),
  wellPanel(
    #br(),
    reactableOutput("armsOnly")
  )
)

##### Panel 2: Disease
dise_form <- fluidPage(
  fluidRow(
    uiOutput("secondhalf")
  )
)

secondhalfUI <- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
    br(),
    
    div(style = "margin-top: 20px;"),
    
    column(width = 4,
           offset = 8,
           actionButton("disDis", "Move to Biomarker",class = "btn-warning")),
    br(),
    
    div(style = "margin-top: 20px;"),

    ### Hold status for the trial at the site 
    br(),
    column(3, selectInput("trHold",
                          "1. Please choose the trial status for the site:",

                        #  choices = c("open", "JIT", "open(JIT)", "pending(JIT)","on hold", "closed","coming soon"))),
                           #  choices = c("open", "JIT", "In Start Up", "Suspended","open(JIT)", "pending(JIT)","on hold", "closed","coming soon"))),
                        
                        #revising choices
                        choices = c("open", "JIT", "start up", "on hold", "closed"), selected = "")),
                        
    column(9, textInput("disSum", 
                        "2. Please enter an overall disease summary")),
    
   
    br(),
    
    ### Text Summary 
    div(style = "margin-top: 20px;"),
    br(),
    
    ### Individual disease code + selection
    # h6("3. Please choose each disease you wish to record"), 
    # h6(em("Choose levels according to Oncotree with as much detail as possible")),
    h6("3. Please choose each disease you wish to record (choose levels according to Oncotree with as much detail as possible)"),
    br(),
    
    
    br(),
    radioGroupButtons(
      inputId = "certir",
      choices = c("include", 
                  "exclude"),
      justified = F
    ),
    
    wellPanel(
      fluidRow(
        column(9,
               #style = "display: inline-block;",
               #selectInput("dise", "Tissue Site", choices = c(unique(oncotree$level_1),"All Cancer","Solid Tumors"), multiple = FALSE)
               selectInput("dise", "Tissue Site", choices = c(unique(oncotree_addrows1$level_1), multiple = FALSE))
               ),
        column(4,
               selectInput("lev2", "Level2", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               #style = "display: inline-block;",
               selectInput("lev3", "Level3", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               selectInput("lev4", "Level4", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               selectInput("lev5", "Level5", choices = "", selected = "")
        ),
        column(4,
               selectInput("lev6", "Level6", choices = "", selected = "")
        ),
        column(4,
               selectInput("lev7", "Level7", choices = "", selected = "")
        )
      ),
      # fluidRow(checkboxGroupInput(
      #   inputId = "levl_stage", label= "Disease Stage",
      #   choices = c("Not available","Stage I","Stage II","Stage III","Stage IIIc","Extensive","Stage IV","Stage IVA", "Stage IVB", "Methylated","Un-resectable","resectable",
      #               "Unmethylated","Advanced stage","Advanced/Metastatic","Locally Advanced","Neoadjuvant", "Adjuvant","Recurrent","Metastatic","Early stage", "New diagnosis","Neoplasms","Relapsed/Refractory","Post Cellular Therapy",
      #               "Smoldering Myeloma"),inline = T,selected = NULL)
      #   
      # ),
      fluidRow(checkboxGroupInput(
        inputId = "levl_stage", label= "Disease Stage",
        choices = c("Not available","Stage I","Stage II","Stage III","Stage IIIC","Extensive","Stage IV","Stage IVA", "Stage IVB", "Methylated","Unresectable","Resectable",
                    "Unmethylated","Advanced","Locally Advanced","Neoadjuvant", "Adjuvant","Recurrent","Metastatic","Early stage", "New diagnosis","Neoplasms","Relapsed/Refractory","Post Cellular Therapy",
                    "Smoldering Myeloma"),inline = T,selected = NULL)
        
      ),
      
    br(),
    br(),
    actionButton(inputId = "addDis",label = "ADD")
    ),
    
    
    
    # display chosen disease
    br(),
    div(style = "margin-top: 40px;"),
    h5(strong("A: Cohort level disease information")),
    div(style = "margin-top: 20px;"),
    # add clear button for disease
    
    div(
      style = "margin-top: 40px;",
      shiny::actionButton(
        inputId = "clr_Dis",
        label = "Clear",
        #icon = shiny::icon("plus"),
        class = "btn-primary")
    ),
    br(),
    
    DTOutput(outputId = "dt_dise",
                 width = "100%"),
    br(),
    br(),
    div(style = "margin-top: 50px;")
  )
)


##### Panel 3: Biomarker
biom_form <- fluidPage(
  uiOutput("bioDis")
  )

biom_display <- fluidPage(
  div(
    class = "container",
    br(),
    column(width = 4,
           offset = 8,
           shiny::actionButton(
             inputId = "bioMrk",
             label = "Move to Documents",
             class = "btn-warning"),
      br(),
    ), 
  ),
   
  ## PART 1 - cohort arm LoT and recruitemtnt status
  # TABLE 1 to add arm information
  div(style = "margin-top: 20px;"),
  h5("1. Please select a cohort arm to add line of therapy and arm recruitment status"),

  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::dataTableOutput(outputId = "dt_table_arm", 
                        width = "100%")
  ),
  
  # add common LoT + Arm Status
  div(
    style = "margin-top: 20px;",
    shiny::actionButton(
      inputId = "add_allArmLotStatus",
      label = "Add common LoT & Arm status",
      icon = shiny::icon("plus"),
      class = "btn-primary")
  ),
  br(),  

  # TABLE A to show LoT and Status
  div(style = "margin-top: 30px;"),
  h5(strong("A: Cohort level line of therapy and recruitment status")),
  # add clear button arm info
  
  div(
    style = "margin-top: 40px;",
    shiny::actionButton(
      inputId = "clr_Arm",
      label = "Clear",
      #icon = shiny::icon("plus"),
      class = "btn-primary")
  ),
  br(),
  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::DTOutput(outputId = "dt_table_arm_display",
                 width = "100%"),
    br()
  ),

  ## PART 2: cohort arm biomarkers
  div(style = "margin-top: 20px;"),
  h5("2. Please select a cohort arm to add corresponding biomarker(s)"),
  
  # TABLE 2 to add biomarker information
  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::dataTableOutput(outputId = "dt_table",
                        width = "100%")
  ),
  
  # add common biomarker
  div(
    style = "margin-top: 40px;",
    shiny::actionButton(
      inputId = "add_allBio",
      label = "Add common biomarker",
      #icon = shiny::icon("plus"),
      class = "btn-primary")
  ),
  br(),

  # TABLE B to show LoT, arm status and biomarkers
  div(style = "margin-top: 30px;"),
  h5(strong("B: Cohort level biomarker information")),
  
  
  # add clear button here biomaker 
  div(
    style = "margin-top: 40px;",
    shiny::actionButton(
      inputId = "clr_Bio",
      label = "Clear",
      #icon = shiny::icon("plus"),
      class = "btn-primary")
  ),
  br(),
  
  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::DTOutput(outputId = "dt_biomark",
                 width = "100%"),
    br()
  ),
 
  shiny::includeScript(here("R","script.js"))
  
)

##### Panel 4: Documentation
doc_form <- fluidPage(
  uiOutput("DisDoc")
)

docuOut <- fluidPage(
  fluidRow(
  
  div(style = "margin-top: 40px;"),
  column(width = 4,
         offset = 8,
         actionButton("move_brow", "Move to Browser",class="btn-warning")),
  br(),
  br(),

  wellPanel(
    column(4,
           # add link to trial documentation
           textInput(inputId = "doc", label = "Please add link to (site) trial documentation")),
    column(6,  h5("Link added: "), textOutput("doc_link"))
    ),

  #added last update date for documentation
  br(),
  br(),
  br(),
  
  column(6,
  # add  documentation date and trial location
  dateInput(inputId = "dt", label = "Document last updated")),
  
  #column(6, textInput(inputId = "loct", label = "Location of trial availablity (eg: Sioux Falls, Aberdeen, Pierre, Yankton, Marshall)") )
  column(6, 
         selectInput("loct", 
                     "Location of trial availablity (eg: Sioux Falls, Aberdeen, Pierre, Yankton, Marshall, Mitchell)", 
                     choices = c("Sioux Falls", "Aberdeen", "Pierre", "Yankton", "Marshall", "Mitchell"), selected = "", multiple = TRUE))
  
))

##### Panel 5: View Trial
# display for the browser tab
dis_form <- fluidPage(
  uiOutput("DisBrow")
)


browserOut <- fluidPage(
  br(),
  column(2, actionButton("confirm1", "CONFIRM")),
  
 # query specific populate 
 reactableOutput("displayBio")
)
