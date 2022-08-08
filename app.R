#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinybusy)
library(htmltools)
library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape)
library(DT)
library(DescTools)
library(readxl)
library(markdown)
library(giftedCalcs)



# import functions
source("functions.R")


# ui ----------------------------------------------------------------------
# Define UI for the application
ui <- fluidPage(
  
    tags$head(tags$style(
      HTML('
           #border {
              border: 1px solid black;
          }')
    )),

    theme = shinytheme("readable"),
    # enable MathJax (not currently used)
    withMathJax(),
    
    # format the title
    tags$head(tags$style(
      HTML("
      h1 {
        font-weight: 500;
        line-height: 1;
        font-size: 200%
      }
    ")
    )),
    
    # Removes spin wheels from inputs
    tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),
    
    
    headerPanel("Optimal ID equity explorer"),
    
    tabsetPanel(id="main",
      
# About tab ---------------------------------------------------------------
      tabPanel("About",
               
               HTML("<br>"),
               HTML("<br>"),
               htmltools::includeMarkdown("helpText.md")
        ),
      
# Data tab ----------------------------------------------------------------
      tabPanel("Data",
        shinyjs::useShinyjs(),
        
        sidebarLayout(
          sidebarPanel(
        
          HTML("<br>"),
          
          fileInput(
            inputId = "file",
            label = "Dataset",
            multiple = FALSE,
            accept = NULL,
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
                  
          helpText(
            "(Dataset should be prepared according to instructions",
            a(href="https://r4ds.had.co.nz", target="_blank", "here)")
          ),
          
          radioButtons(
            inputId = "fileType",
            label = "Type of file selected",
            inline = TRUE,
            choices = c("excel", "csv"),
            selected = "excel"
          ),
          
          conditionalPanel(
            condition = "input.fileType == 'excel'",
            
            numericInput(
              inputId = "whichSheet",
              label = "Select sheet for import",
              min = 1,
              max = 100,
              step = 1,
              value = 1
            )
          ),
          
            
          actionButton(
            inputId = "loadFile",
            label = "Load selected file"
          ),
            
          
          ), # closes sidebarPanel
          
        mainPanel(
          
          HTML("<br>"),
          
          selectizeInput(
            inputId = "filter_group", 
            label = "Filtering variable(s)", 
            multiple = TRUE,
            choices = NULL,
            options=list(maxItems=3)
          ),
          
          conditionalPanel(
            condition = "input.filter_group.length >= 1",
            selectInput(
              inputId = "filter_reference_grp1", 
              label = "Levels to select for first filter variable", 
              choices = NULL,
              multiple = TRUE,
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_group.length >= 2",
            selectInput(
              inputId = "filter_reference_grp2", 
              label = "Levels to select for second filter variable", 
              choices = NULL,
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_group.length >= 3",
            selectInput(
              inputId = "filter_reference_grp3", 
              label = "Levels to select for third filter variable", 
              choices = NULL,
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_reference_grp1 != ''",
            actionButton(
              "applyFilter",
              "Apply selected filtering"
            )
          ),
          
          HTML("<br>"),
          HTML("<br>"),
          
          DTOutput('dat')
          ) # closes mainPanel
        ) # closes sidebarPalen
        
      ), # closes tabpanel 'data'


# Setup panel -------------------------------------------------------------

      tabPanel("Setup",
        
               fluidRow(
                 
                 column(3, 
                        
                        selectizeInput(
                          inputId = "group", 
                          label = "Group(s) for equity analysis", 
                          multiple = TRUE,
                          choices = NULL,
                          options=list(maxItems=2)
                        ),
                        
                        conditionalPanel(
                          condition = "input.group.length >= 1",
                          selectInput(
                            inputId = "reference_grp1", 
                            label = "Reference group for first group", 
                            choices = NULL
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.group.length >= 2",
                          selectInput(
                            inputId = "reference_grp2", 
                            label = "Reference group for second group", 
                            choices = NULL
                          )
                        ),
                        
                        selectInput(
                          inputId = "baseline_id_var", 
                          label = "Baseline id variable", 
                          choices = NULL
                        ),
                        
                        helpText("(don't turn this off unless you really know what you are doing)"),
                        
                        checkboxInput(
                          inputId = "listwise",
                          label = "Phase-II listwise deletion",
                          value = TRUE
                        ),
                        
                        actionButton(
                          inputId = "btn_addPathway",
                          label = HTML("Add another<br/>identification pathway")
                        ),
                        
                        actionButton(
                          inputId = "btn_removePathway",
                          label = HTML("Remove the last<br/>identification pathway")
                        )
                 ) # closes column()
               ) # closes fluidRow
      ), # closes tabPanel
        

# Panel for pathway 1 --------------------------------------------------------------
      tabPanel("Pathway 1",
          
        fluidRow(
          
          column(3, 
                 
                 HTML("<br>"),
                 
                 textInput(
                   inputId = "lbl_pathway1",
                   label = "Name for pathway 1",
                   placeholder = "Pathway1"
                 ),
                 
                 selectizeInput(
                   inputId = "assessments", 
                   label = "Assessments", 
                   multiple = TRUE,
                   choices = NULL,
                   options=list(maxItems=6)
                 ),
                 
                 
                 checkboxInput(
                   inputId = "adj_weights",
                   label = "Adjust weights?",
                 ),
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 1",
                   
                   helpText(HTML("<strong>Note</strong>: weights are normalized to sum to the number of assessments")),
                   
                   div(
                     numericInput(
                       inputId = "weight1",
                       label = "Weight for first assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 2",
                   
                   div(
                     numericInput(
                       inputId = "weight2",
                       label = "Weight for second assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 3",
                   
                   div(
                     numericInput(
                       inputId = "weight3",
                       label = "Weight for third assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 4",
                   
                   div(
                     numericInput(
                       inputId = "weight4",
                       label = "Weight for fourth assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 5",
                   
                   div(
                     numericInput(
                       inputId = "weight5",
                       label = "Weight for fifth assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 conditionalPanel(
                   condition = "input.adj_weights && input.assessments.length >= 6",
                   
                   div(
                     numericInput(
                       inputId = "weight6",
                       label = "Weight for sixth assessment",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = .1,
                       width = '90%'),
                     style='font-size:80%')
                 ),
                 
                 
                 selectInput(
                   inputId = "nom", 
                   label = "Nomination instrument", 
                   choices = NULL
                 ),
                 
                 
                 sliderInput(
                   inputId = "nom_cutoff",
                   label = "Nomination cutoff percentile",
                   min = .001,
                   max = .999,
                   value = .7,
                   step=.001
                 ),
                 
                 sliderInput(
                   inputId = "mean_cutoff",
                   label = HTML("Mean score (phase-II) cutoff percentile"),
                   min = .001,
                   max = .999,
                   value = .9,
                   step=.001
                 ),
                 
          ), # closes column for pathway 1
          
          column(9,
                   
           HTML("<br>"),
           
           selectInput(
             inputId = "metric", 
             label = "Equity metric", 
             choices = c("Count", "Representation Index", "Proportion Identified", 
                         "Relative Risk", "Cramer's V"),
             selected = "Count"),
           
           HTML("<br>"),
           
           plotOutput("plot", width="120%", height= "500px"),
           
          ) #closes column()
        
        ) #closes fluidRow
       
  ), # closes tabPanel 'Pathway 1'

# Panel for pathway 2 --------------------------------------------------------------
tabPanel("Pathway 2",

         fluidRow(

           column(3,
                  
                  HTML("<br>"),

                  textInput(
                    inputId = "lbl_pathway2",
                    label = "Name for pathway 2",
                    placeholder = "Pathway2"
                  ),

                  selectizeInput(
                    inputId = "assessments2",
                    label = "Assessments",
                    multiple = TRUE,
                    choices = NULL,
                    options=list(maxItems=6)
                  ),


                  checkboxInput(
                    inputId = "adj_weights2",
                    label = "Adjust weights?",
                  ),

                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 1",

                    helpText(HTML("<strong>Note</strong>: weights are normalized to sum to the number of assessments")),

                    div(
                      numericInput(
                        inputId = "weight21",
                        label = "Weight for first assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),

                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 2",

                    div(
                      numericInput(
                        inputId = "weight22",
                        label = "Weight for second assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),


                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 3",

                    div(
                      numericInput(
                        inputId = "weight23",
                        label = "Weight for third assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),

                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 4",

                    div(
                      numericInput(
                        inputId = "weight24",
                        label = "Weight for fourth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),

                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 5",

                    div(
                      numericInput(
                        inputId = "weight25",
                        label = "Weight for fifth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),

                  conditionalPanel(
                    condition = "input.adj_weights2 && input.assessments2.length >= 6",

                    div(
                      numericInput(
                        inputId = "weight26",
                        label = "Weight for sixth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),


                  selectInput(
                    inputId = "nom2",
                    label = "Nomination instrument",
                    choices = NULL
                  ),


                  sliderInput(
                    inputId = "nom_cutoff2",
                    label = "Nomination cutoff percentile",
                    min = .001,
                    max = .999,
                    value = .7,
                    step=.001
                  ),

                  sliderInput(
                    inputId = "mean_cutoff2",
                    label = HTML("Mean score (phase-II) cutoff percentile"),
                    min = .001,
                    max = .999,
                    value = .9,
                    step=.001
                  ),

           ), # closes column for pathway 2

           column(9,

                  HTML("<br>"),

                  selectInput(
                    inputId = "metric2",
                    label = "Equity metric",
                    choices = c("Count", "Representation Index", "Proportion Identified",
                                "Relative Risk", "Cramer's V"),
                    selected = "Count"),

                  HTML("<br>"),

                  plotOutput("plot2", width="120%", height= "500px"),

           ) #closes column()

         ) # closes fluidRow
         
      ), # closes tabPanel


# Panel for pathway 3 ------------------------------------------------------------
tabPanel("Pathway 3",
         
         fluidRow(
           
           HTML("<br>"),
           
           column(3,
                  
                  textInput(
                    inputId = "lbl_pathway3",
                    label = "Name for pathway 3",
                    placeholder = "Pathway3"
                  ),
                  
                  selectizeInput(
                    inputId = "assessments3", 
                    label = "Assessments", 
                    multiple = TRUE,
                    choices = NULL,
                    options=list(maxItems=6)
                  ),
                  
                  
                  checkboxInput(
                    inputId = "adj_weights3",
                    label = "Adjust weights?",
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 1",
                    
                    helpText(HTML("<strong>Note</strong>: weights are normalized to sum to the number of assessments")),
                    
                    div(
                      numericInput(
                        inputId = "weight31",
                        label = "Weight for first assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 2",
                    
                    div(
                      numericInput(
                        inputId = "weight32",
                        label = "Weight for second assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 3",
                    
                    div(
                      numericInput(
                        inputId = "weight33",
                        label = "Weight for third assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 4",
                    
                    div(
                      numericInput(
                        inputId = "weight34",
                        label = "Weight for fourth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 5",
                    
                    div(
                      numericInput(
                        inputId = "weight35",
                        label = "Weight for fifth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights3 && input.assessments3.length >= 6",
                    
                    div(
                      numericInput(
                        inputId = "weight36",
                        label = "Weight for sixth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  
                  selectInput(
                    inputId = "nom3",
                    label = "Nomination instrument", 
                    choices = NULL
                  ),
                  
                  
                  sliderInput(
                    inputId = "nom_cutoff3",
                    label = "Nomination cutoff percentile",
                    min = .001,
                    max = .999,
                    value = .7,
                    step=.001
                  ),
                  
                  sliderInput(
                    inputId = "mean_cutoff3",
                    label = HTML("Mean score (phase-II) cutoff percentile"),
                    min = .001,
                    max = .999,
                    value = .9,
                    step=.001
                  ),
                  
           ), # closes column for pathway 3
           
           column(9,
                  
                  selectInput(
                    inputId = "metric3",
                    label = "Equity metric",
                    choices = c("Count", "Representation Index", "Proportion Identified",
                                "Relative Risk", "Cramer's V"),
                    selected = "Count"),
                  
                  HTML("<br>"),
                  
                  plotOutput("plot3", width="120%", height= "500px"),
                  
           ) #closes column()
           
         ) # closes fluidRow
         
), # closes tabPanel


# Panel for pathway 4 ------------------------------------------------------------
tabPanel("Pathway 4",
         
         fluidRow(
           
           HTML("<br>"),
           
           column(3,
                  
                  textInput(
                    inputId = "lbl_pathway4",
                    label = "Name for pathway 4",
                    placeholder = "Pathway4",
                  ),
                  
                  selectizeInput(
                    inputId = "assessments4", 
                    label = "Assessments", 
                    multiple = TRUE,
                    choices = NULL,
                    options=list(maxItems=6)
                  ),
                  
                  
                  checkboxInput(
                    inputId = "adj_weights4",
                    label = "Adjust weights?",
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 1",
                    
                    helpText(HTML("<strong>Note</strong>: weights are normalized to sum to the number of assessments")),
                    
                    div(
                      numericInput(
                        inputId = "weight41",
                        label = "Weight for first assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 2",
                    
                    div(
                      numericInput(
                        inputId = "weight42",
                        label = "Weight for second assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 3",
                    
                    div(
                      numericInput(
                        inputId = "weight43",
                        label = "Weight for third assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 4",
                    
                    div(
                      numericInput(
                        inputId = "weight44",
                        label = "Weight for fourth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 5",
                    
                    div(
                      numericInput(
                        inputId = "weight45",
                        label = "Weight for fifth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  conditionalPanel(
                    condition = "input.adj_weights4 && input.assessments4.length >= 6",
                    
                    div(
                      numericInput(
                        inputId = "weight46",
                        label = "Weight for sixth assessment",
                        value = 1,
                        min = 0,
                        max = 10,
                        step = .1,
                        width = '90%'),
                      style='font-size:80%')
                  ),
                  
                  
                  selectInput(
                    inputId = "nom4",
                    label = "Nomination instrument", 
                    choices = NULL
                  ),
                  
                  
                  sliderInput(
                    inputId = "nom_cutoff4",
                    label = "Nomination cutoff percentile",
                    min = .001,
                    max = .999,
                    value = .7,
                    step=.001
                  ),
                  
                  sliderInput(
                    inputId = "mean_cutoff4",
                    label = HTML("Mean score (phase-II) cutoff percentile"),
                    min = .001,
                    max = .999,
                    value = .9,
                    step=.001
                  ),
                  
           ), # closes column for pathway 4
           
           column(9,
                  
                  selectInput(
                    inputId = "metric4",
                    label = "Equity metric",
                    choices = c("Count", "Representation Index", "Proportion Identified",
                                "Relative Risk", "Cramer's V"),
                    selected = "Count"),
                  
                  HTML("<br>"),
                  
                  plotOutput("plot4", width="120%", height= "500px"),
                  
           ) #closes column()
           
         ) # closes fluidRow
         
), # closes tabPanel

# Panel for marginal  ------------------------------------------------------------
tabPanel("All pathways",
         
         fluidRow(
           
           HTML("<br>"),
           
           column(3),
           
           column(9,
                  
                  selectInput(
                    inputId = "metric_all",
                    label = "Equity metric",
                    choices = c("Count", "Representation Index", "Proportion Identified",
                                "Relative Risk", "Cramer's V"),
                    selected = "Count"),
                  
                  HTML("<br>"),
                  
                  plotOutput("plot_all", width="120%", height= "500px"),
                  
           ) #closes column()
           
         ) # closes fluidRow
         
), # closes tabPanel

# Equity table panel ------------------------------------------------------
  tabPanel("Equity table",
        fluidPage(
           DTOutput('tbl')
        )
    ),

# Group statistics panel --------------------------------------------------
  tabPanel("Group statistics",
       fluidPage(
         DTOutput('descr_table')
       )
    ),

# Download panel ----------------------------------------------------------
  tabPanel("Download",
           
           HTML("<br>"),
           
           helpText("Download a report of this analysis"),
           
           textAreaInput(
             inputId="run_notes",
             width='75%',
             height='400%',
             label="Notes to include in the report",
             placeholder="Enter optional text here"
           ),
           
           
           radioButtons(
              inputId="reportFormat",
              label="Report format",
              choices=list("html", "pdf", "docx"),
              inline=TRUE
           ),

          downloadButton("report", "Generate report")
        
  ),

# Info on metrics panel ---------------------------------------------------
  tabPanel("Info on metrics",
           
           HTML("<br>"),
           HTML("<br>"),
           htmltools::includeMarkdown("metrics.md")
    ),
  

  ) # closes tabsetPanel
) # closes fluidPage



# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # initialize the app with a disabled load file action button
    # and a disabled 'remove pathway' button
  shinyjs::disable('loadFile')
  shinyjs::disable("btn_removePathway")
  
  # hide tabs on load
  hideTab(inputId = "main", target = "Pathway 2")
  hideTab(inputId = "main", target = "Pathway 3")
  hideTab(inputId = "main", target = "Pathway 4")
  hideTab(inputId = "main", target = "All pathways")
  
  # enable the load file button once a file is loaded
  observeEvent(input$file, {
    shinyjs::enable('loadFile')
  })
  
  
  # initialize the 'dat' reactive object
  dat <- reactiveVal()
  
  # initialize the 'filters' reactive object
  filters <- reactiveValues(
    data_filter_string=NULL,
    group_filter_string=NULL,
    filter_group=NULL,
    filter_ref_grp1=NULL,
    filter_ref_grp2=NULL,
    filter_ref_grp3=NULL,
  )
  
  # initialize the 'tables' reactive object
  tables <- reactiveValues(
    equity_table=NULL,
    group_stats=NULL,
  )
  
  # initialize the 'weights' reactive object
  weights <- reactiveValues(
    w=rep(1, times=6),
    w2=rep(1, times=6),
    w3=rep(1, times=6),
    w4=rep(1, times=6)
  )
  
  # initialize the 'listwise' reactive object
  listwise <- reactiveValues(
      listwise=FALSE
  )
  
  # initialize the 'upload_dataname' reactive object
  upload_dataname <- reactiveValues(
    filename=NULL
  )
  
  # create a reactive object to store the status of the "remove pathway"
  #  button
  pathway_status <- reactiveValues(
      remove_btn_enabled = FALSE,
      add_btn_enabled = TRUE,
      last_pathway = 1
    )
  
  
  observeEvent(input$btn_addPathway, {

      pathway_status$last_pathway = pathway_status$last_pathway + 1
 
      if (pathway_status$last_pathway == 4) {
          shinyjs::show("Max pathways are 4")
          pathway_status$add_btn_enabled = FALSE
      }

      if (pathway_status$last_pathway > 1) {
          pathway_status$remove_btn_enabled = TRUE
          showTab(inputId = "main", target = "All pathways")
      }
      
      if (pathway_status$add_btn_enabled == TRUE) {
          shinyjs::enable("btn_addPathway")
        } else {shinyjs::disable("btn_addPathway")}


      if (pathway_status$remove_btn_enabled == TRUE) {
          shinyjs::enable("btn_removePathway")
        } else {shinyjs::disable("btn_removePathway")}

      
      if (pathway_status$last_pathway == 2) {
        showTab(inputId = "main", target = "Pathway 2")
      }
      
      if (pathway_status$last_pathway == 3) {
        showTab(inputId = "main", target = "Pathway 3")
      }
      
      if (pathway_status$last_pathway == 4) {
        showTab(inputId = "main", target = "Pathway 4")
      }

      print(pathway_status$last_pathway)
  })

  observeEvent(input$btn_removePathway, {
      
      if (pathway_status$last_pathway > 1) {
        pathway_status$last_pathway = pathway_status$last_pathway - 1
      }
      
      if (pathway_status$last_pathway == 1) {
          shinyjs::show("Min pathways are 1")
          pathway_status$add_btn_enabled = TRUE
          pathway_status$remove_btn_enabled = FALSE
      }
      
      if (pathway_status$last_pathway < 4) {
          pathway_status$add_btn_enabled = TRUE
      }
      
      if (pathway_status$add_btn_enabled == TRUE) {
          shinyjs::enable("btn_addPathway")
      } else {shinyjs::disable("btn_addPathway")}
      
      
      if (pathway_status$remove_btn_enabled == TRUE) {
          shinyjs::enable("btn_removePathway")
      } else {shinyjs::disable("btn_removePathway")}
      
      if (pathway_status$last_pathway == 1) {
        hideTab(inputId = "main", target = "Pathway 2")
        hideTab(inputId = "main", target = "Pathway 3")
        hideTab(inputId = "main", target = "Pathway 4")
        hideTab(inputId = "main", target = "All pathways")
      }
      
      if (pathway_status$last_pathway == 2) {
        hideTab(inputId = "main", target = "Pathway 3")
        hideTab(inputId = "main", target = "Pathway 4")
      }
      
      if (pathway_status$last_pathway == 3) {
        hideTab(inputId = "main", target = "Pathway 4")
      }
      
      print(pathway_status$last_pathway)
  })  
          


  # do the initial data loading when no filtering has been selected
  observeEvent(input$loadFile, {
    
    infile <- input$file
    
    if (is.null(file)) {
      return(NULL)
    }
    
    # read the data
    if (input$fileType == 'csv') {
      mydata <- read.csv(infile$datapath, header = TRUE)
    } else if (input$fileType == 'excel') {
      mydata <- read_excel(path=infile$datapath, sheet=input$whichSheet)
    }
    
    # append the 'overall' column
    mydata$overall = 1
    
    # preview the loaded data
    output$dat <- DT::renderDT(
      mydata 
    )
    
    # store the loaded data in the reactive object
    dat(mydata)
    
    # add the datafile name to the reactive object (for display in the downloaded report)
    upload_dataname$filename = basename(infile$name)
    
    })
  
  # reload the data and apply filtering
  observeEvent(input$applyFilter, {
    
    infile <- input$file

    if (is.null(file)) {
      return(NULL)
    }

    # read the data
    if (input$fileType == 'csv') {
      mydata <- read.csv(infile$datapath, header = TRUE)
    } else if (input$fileType == 'excel') {
      mydata <- read_excel(path=infile$datapath, sheet=input$whichSheet)
    }
    
    # append the 'overall' column
    mydata$overall = 1
    
    filter_group = input$filter_group
    
    #construct the filtering string
    if (!is.null(filter_group)) {
      filter_ref_group1 = input$filter_reference_grp1
      filter_col_type1 = typeof(dat()[[input$filter_group[1]]])
      
      if (filter_col_type1 == 'character') {
        data_filter_string = paste0(filter_group[1], " %in% c(", 
                                    toString(paste0("'", filter_ref_group1, "'", collapse=","))
                                    , ")")
      } else {
        data_filter_string = paste0(filter_group[1], " %in% c(", 
                                    toString(filter_ref_group1), ")")
      }
      
      if (length(filter_group) >= 2) {
        filter_ref_group2 = input$filter_reference_grp2
        filter_col_type2 = typeof(dat()[[input$filter_group[2]]])
        
        if (filter_col_type2 == 'character') {
          data_filter_string = paste0(data_filter_string, " & ", 
                                      filter_group[2], " %in% c(", 
                                      toString(paste0("'", filter_ref_group2, "'", collapse=","))
                                      , ")")
        } else {
          data_filter_string = paste0(data_filter_string, " & ", 
                                      filter_group[2], " %in% c(", 
                                      toString(filter_ref_group2), ")")
        }
        
      }
      
      if (length(filter_group) >= 3) {
        
        filter_ref_group3 = input$filter_reference_grp3
        filter_col_type3 = typeof(dat()[[input$filter_group[3]]])
        
        if (filter_col_type3 == 'character') {
          data_filter_string = paste0(data_filter_string, " & ", 
                                      filter_group[3], " %in% c(", 
                                      toString(paste0("'", filter_ref_group3, "'", collapse=","))
                                      , ")")
        } else {
          data_filter_string = paste0(data_filter_string, " & ", 
                                      filter_group[3], " %in% c(", 
                                      toString(filter_ref_group3), ")")
        }
        
      }
    }
    
    mydata <- dplyr::filter(mydata, eval(parse(text=data_filter_string)))
    
    output$dat <- DT::renderDT(
      mydata
    )
    
    # store the loaded data in the reactive object
    dat(mydata)
    
    # save the current filtering selections so they can be displayed
    #  in the downloaded report (as the inputs are cleared when the selection
    #  are applied)
    filters$data_filter_string <- data_filter_string
    filters$filter_group = filter_group
    
    if (length(filter_group) > 0) {
      filters$filter_ref_grp1 = filter_ref_group1 
    }
    if (length(filter_group) > 1) {
      filters$filter_ref_grp2 = filter_ref_group2
    }
    if (length(filter_group) > 2) {
      filters$filter_ref_grp3 = filter_ref_group3
    }
    
    
  })
  
    
    # get the column names from the dataset
    variable_names <- reactive({
      variable_names <- names(dat()) 
    })
    
    # update the weights reactive object
    observe({
      weights$w=c(input$weight1, input$weight2, input$weight3, 
                  input$weight4, input$weight5, input$weight6)
      weights$w2=c(input$weight21, input$weight22, input$weight23, 
                  input$weight24, input$weight25, input$weight26)
      weights$w3=c(input$weight31, input$weight32, input$weight33, 
                   input$weight34, input$weight35, input$weight36)
      weights$w4=c(input$weight41, input$weight42, input$weight43, 
                   input$weight44, input$weight45, input$weight46)
    })
    
    observe({
        listwise$listwise=input$listwise
    })
    
    
    # updateSelectInput updates the choices in a SelectInput interface object...
    observe({
      updateSelectInput(
        session = session, 
        inputId = "group", 
        choices = variable_names()
      )
      
      updateSelectInput(
        session = session, 
        inputId = "filter_group", 
        choices = variable_names()
      )
    })
    
    # this executes when the user selects an equity group
    observe({
      group = input$group
      
      if (!is.null(group)) {  
        if (!is.na(group[1])) {
          
          group1_levels <- reactive({
            group1_levels <-  dat() %>% dplyr::select(group[1]) %>% unique()
          })
          
          updateSelectInput(
            session = session, 
            inputId = "reference_grp1", 
            choices = group1_levels()
          )
          
          
        }
      }
      
      if (!is.null(group)) { 
        if (!is.na(group[2])) {
          
          group2_levels <- reactive({
            group2_levels <-  dat() %>% dplyr::select(group[2]) %>% unique()
          })
          
          updateSelectInput(
            session = session, 
            inputId = "reference_grp2", 
            choices = group2_levels()
          )
        }
      }
      
    })
    
    # this executes when the user selects a filtering group
    observe({
      filter_group = input$filter_group
      
      if (!is.null(filter_group)) {  
        if (!is.na(filter_group[1])) {
          
          filter_group1_levels <- reactive({
            filter_group1_levels <-  dat() %>% dplyr::select(filter_group[1]) %>% unique()
          })
          
          updateSelectInput(
            session = session, 
            inputId = "filter_reference_grp1", 
            choices = filter_group1_levels()
          )
        }
      }
    })
    
    observe({
      filter_group = input$filter_group
      
      if (!is.null(filter_group)) {  
        if (!is.na(filter_group[2])) {
          
          filter_group2_levels <- reactive({
            filter_group2_levels <-  dat() %>% dplyr::select(filter_group[2]) %>% unique()
          })
          
          updateSelectInput(
            session = session, 
            inputId = "filter_reference_grp2", 
            choices = filter_group2_levels()
          )
        }
      }
    })
    
    
    observe({
      filter_group = input$filter_group
      
      if (!is.null(filter_group)) {  
        if (!is.na(filter_group[3])) {
          
          filter_group3_levels <- reactive({
            filter_group3_levels <-  dat() %>% dplyr::select(filter_group[3]) %>% unique()
          })
          
          updateSelectInput(
            session = session, 
            inputId = "filter_reference_grp3", 
            choices = filter_group3_levels()
          )
        }
      }
    })
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "assessments", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "assessments2", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "assessments3", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "assessments4", 
        choices = variable_names())
    })
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "nom", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "nom2", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "nom3", 
        choices = variable_names())
    })
    observe({
      updateSelectInput(
        session = session, 
        inputId = "nom4", 
        choices = variable_names())
    })
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "baseline_id_var", 
        choices = variable_names())
    })
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "assessments", 
        choices = variable_names())
    })
  
    
  # this runs when the group box is touched
  observe({
  
    group = input$group 
    
    #construct the filtering string
    if (!is.null(group)) {
      ref_group1 = input$reference_grp1
      col_type1 = typeof(dat()[[input$group[1]]])

        if (col_type1 == 'character') {
          filter_string = paste0(group[1], "== '", ref_group1, "'")
        } else {
         filter_string = paste0(group[1], "==", ref_group1)
        }

      if (length(group) == 2) {
        ref_group2 = input$reference_grp2
        col_type2 = typeof(dat()[[group[2]]])

        if (col_type2 == 'character') {
          filter_string = paste0(filter_string, " & ", group[2], "== '", ref_group2, "'")
        } else {
          filter_string = paste0(filter_string, " & ", group[2], "==", ref_group2)
        }

      }
      
      filters$group_filter_string = filter_string

    }

# render plot1 ------------------------------------------------------------
    output$plot <- renderPlot({
      
      if (!is.null(group) & !is.null(input$assessments) & !is.null(input$nom)) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # get the data out of the reactive object
        mydata = dat()
        
        # assign a new variable in the local environment only!
        #  (we don't want meanscore to appear as a variable for selection)
        mydata$meanscore = identify_opti(data=mydata, 
                                        assessments=input$assessments, 
                                        nom=input$nom, 
                                        nom_cutoff=input$nom_cutoff, 
                                        test_cutoff=input$mean_cutoff,
                                        listwise=listwise$listwise,
                                        mode = "meanscores", 
                                        weights = weights$w[1:length(input$assessments)])
        
        # construct the descriptive statistics table and load it into
        #  the appropriate reactive element
        tables$group_stats <- descr_table(data=mydata,
                                          group=group,
                                          reference_grp=filter_string,
                                          vars=unique(c("meanscore", input$assessments, input$nom)),
                                          
        )
        
        # 'results' is a list containing both the plot ($p) and the raw equity statistics
        #   table
        
        results = equity_plot_multi(data=mydata, #dat()
                    group=input$group,
                    reference_grp=filter_string,
                    pathways=list(pathway_1 = list(
                        assessments=input$assessments,
                        listwise=listwise$listwise,
                        nom=input$nom,
                        nom_cutoff=input$nom_cutoff,
                        test_cutoff=input$mean_cutoff,
                        weights=weights$w[1:length(input$assessments)])
                    ),
                    baseline_id_var=input$baseline_id_var,
                    plot_metric=input$metric,
                    selected_pathway=1
                    )
      
        # process the equity table - format for display
        tbl_long = results$summary_tbl
        
        tbl_long$value = round(tbl_long$value, 3)
        
        tbl_wide = pivot_wider(
          dplyr::filter(tbl_long, metric %in% 
                          c("count", "pct_identified", "RI", "RR", "CramerV")), 
          names_from=c("metric"))
        
        if (length(group) == 1) {
          
          tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
                                    tbl_wide$comparison,
                                    decreasing=TRUE, na.last=FALSE),]
        } else if (length(group) == 2) {
          
          tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
                                    tbl_wide[[group[2]]],
                                    tbl_wide$comparison,
                                    decreasing=TRUE, na.last=FALSE),]
        }
        
        # load the table into the reactive object for display and download
        tables$equity_table = dplyr::select(tbl_wide, -baseline)
        
        # show the plot
        return(results$p)
        
      }
      
    }) 
    
# render plot2-------------------------------------------------------------
    output$plot2 <- renderPlot({
      
      
      if (!is.null(group) & !is.null(input$assessments2) & !is.null(input$nom2)) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # get the data out of the reactive object
        mydata = dat()
        
        # assign a new variable in the local environment only!
        #  (we don't want meanscore to appear as a variable for selection)
        mydata$meanscore2 = identify_opti(data=mydata, 
                                         assessments=input$assessments2, 
                                         nom=input$nom2, 
                                         nom_cutoff=input$nom_cutoff2, 
                                         test_cutoff=input$mean_cutoff2,
                                         listwise=listwise$listwise,
                                         mode = "meanscores", 
                                         weights = weights$w2[1:length(input$assessments2)])
        
        # construct the descriptive statistics table and load it into
        #  the appropriate reactive element
        # tables$group_stats <- descr_table(data=mydata,
        #                                   group=group,
        #                                   reference_grp=filter_string,
        #                                   vars=unique(c("meanscore", input$assessments, input$nom)),
        #                                   
        # )
        
        # 'results' is a list containing both the plot ($p) and the raw equity statistics
        #   table
        
        results = equity_plot_multi(data=mydata, #dat()
                                    group=input$group,
                                    reference_grp=filter_string,
                                    pathways=list(pathway_2 = list(
                                      assessments=input$assessments2,
                                      listwise=listwise$listwise2,
                                      nom=input$nom2,
                                      nom_cutoff=input$nom_cutoff2,
                                      test_cutoff=input$mean_cutoff2,
                                      weights=weights$w2[1:length(input$assessments2)])
                                    ),
                                    baseline_id_var=input$baseline_id_var,
                                    plot_metric=input$metric2,
                                    selected_pathway=1
        )
        
        # # process the equity table - format for display
        # tbl_long = results$summary_tbl
        # 
        # tbl_long$value = round(tbl_long$value, 3)
        # 
        # tbl_wide = pivot_wider(
        #   dplyr::filter(tbl_long, metric %in% 
        #                   c("count", "pct_identified", "RI", "RR", "CramerV")), 
        #   names_from=c("metric"))
        # 
        # if (length(group) == 1) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # } else if (length(group) == 2) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide[[group[2]]],
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # }
        # 
        # # load the table into the reactive object for display and download
        # tables$equity_table = dplyr::select(tbl_wide, -baseline)
        
        # show the plot
        return(results$p)
        
      }
      
    }) 
    

# render plot3 ------------------------------------------------------------
    output$plot3 <- renderPlot({
      
      
      if (!is.null(group) & !is.null(input$assessments3) & !is.null(input$nom3)) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # get the data out of the reactive object
        mydata = dat()
        
        # assign a new variable in the local environment only!
        #  (we don't want meanscore to appear as a variable for selection)
        mydata$meanscore3 = identify_opti(data=mydata, 
                                          assessments=input$assessments3, 
                                          nom=input$nom3, 
                                          nom_cutoff=input$nom_cutoff3, 
                                          test_cutoff=input$mean_cutoff3,
                                          listwise=listwise$listwise,
                                          mode = "meanscores", 
                                          weights = weights$w3[1:length(input$assessments3)])
        
        # construct the descriptive statistics table and load it into
        #  the appropriate reactive element
        # tables$group_stats <- descr_table(data=mydata,
        #                                   group=group,
        #                                   reference_grp=filter_string,
        #                                   vars=unique(c("meanscore", input$assessments, input$nom)),
        #                                   
        # )
        
        # 'results' is a list containing both the plot ($p) and the raw equity statistics
        #   table
        
        results = equity_plot_multi(data=mydata, #dat()
                                    group=input$group,
                                    reference_grp=filter_string,
                                    pathways=list(pathway_3 = list(
                                      assessments=input$assessments3,
                                      listwise=listwise$listwise3,
                                      nom=input$nom3,
                                      nom_cutoff=input$nom_cutoff3,
                                      test_cutoff=input$mean_cutoff3,
                                      weights=weights$w3[1:length(input$assessments3)])
                                    ),
                                    baseline_id_var=input$baseline_id_var,
                                    plot_metric=input$metric3,
                                    selected_pathway=1
        )
      
        
        # # process the equity table - format for display
        # tbl_long = results$summary_tbl
        # 
        # tbl_long$value = round(tbl_long$value, 3)
        # 
        # tbl_wide = pivot_wider(
        #   dplyr::filter(tbl_long, metric %in% 
        #                   c("count", "pct_identified", "RI", "RR", "CramerV")), 
        #   names_from=c("metric"))
        # 
        # if (length(group) == 1) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # } else if (length(group) == 2) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide[[group[2]]],
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # }
        # 
        # # load the table into the reactive object for display and download
        # tables$equity_table = dplyr::select(tbl_wide, -baseline)
        
        # show the plot
        return(results$p)
        
      }
      
    }) 
    
    # render plot4 ------------------------------------------------------------
    output$plot4 <- renderPlot({
      
      
      if (!is.null(group) & !is.null(input$assessments4) & !is.null(input$nom4)) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # get the data out of the reactive object
        mydata = dat()
        
        # assign a new variable in the local environment only!
        #  (we don't want meanscore to appear as a variable for selection)
        mydata$meanscore4 = identify_opti(data=mydata, 
                                          assessments=input$assessments4, 
                                          nom=input$nom4, 
                                          nom_cutoff=input$nom_cutoff4, 
                                          test_cutoff=input$mean_cutoff4,
                                          listwise=listwise$listwise,
                                          mode = "meanscores", 
                                          weights = weights$w4[1:length(input$assessments4)])
        
        # construct the descriptive statistics table and load it into
        #  the appropriate reactive element
        # tables$group_stats <- descr_table(data=mydata,
        #                                   group=group,
        #                                   reference_grp=filter_string,
        #                                   vars=unique(c("meanscore", input$assessments, input$nom)),
        #                                   
        # )
        
        # 'results' is a list containing both the plot ($p) and the raw equity statistics
        #   table
        
        results = equity_plot_multi(data=mydata, #dat()
                                    group=input$group,
                                    reference_grp=filter_string,
                                    pathways=list(pathway_4 = list(
                                      assessments=input$assessments4,
                                      listwise=listwise$listwise4,
                                      nom=input$nom4,
                                      nom_cutoff=input$nom_cutoff4,
                                      test_cutoff=input$mean_cutoff4,
                                      weights=weights$w4[1:length(input$assessments4)])
                                    ),
                                    baseline_id_var=input$baseline_id_var,
                                    plot_metric=input$metric4,
                                    selected_pathway=1
        )
        
        
        # # process the equity table - format for display
        # tbl_long = results$summary_tbl
        # 
        # tbl_long$value = round(tbl_long$value, 3)
        # 
        # tbl_wide = pivot_wider(
        #   dplyr::filter(tbl_long, metric %in% 
        #                   c("count", "pct_identified", "RI", "RR", "CramerV")), 
        #   names_from=c("metric"))
        # 
        # if (length(group) == 1) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # } else if (length(group) == 2) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide[[group[2]]],
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # }
        # 
        # # load the table into the reactive object for display and download
        # tables$equity_table = dplyr::select(tbl_wide, -baseline)
        
        # show the plot
        return(results$p)
        
      }
      
    })   

# render marginal plot ----------------------------------------------------
    output$plot_all <- renderPlot({
      
      pathways = list()
      
      # only render this plot if a group is selected, at least one of the 
      #   sets of assessments is supplied, and at least one of the nomination
      #   instruments is supplied
      if (pathway_status$last_pathway > 1 &
        !is.null(group) & 
          (!is.null(input$assessments) | 
                    !is.null(input$assessments2) |
                    !is.null(input$assessments3) |
                    !is.null(input$assessments4) ) &
          (!is.null(input$nom) |
           !is.null(input$nom2) |
           !is.null(input$nom3) |
           !is.null(input$nom4)) ) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # get the data out of the reactive object
        mydata = dat()
        
        # assign a new variable in the local environment only!
        #  (we don't want meanscore to appear as a variable for selection)
        mydata$meanscore = identify_opti(data=mydata, 
                                           assessments=input$assessments, 
                                           nom=input$nom, 
                                           nom_cutoff=input$nom_cutoff, 
                                           test_cutoff=input$mean_cutoff,
                                           listwise=listwise$listwise,
                                           mode = "meanscores", 
                                           weights = weights$w[1:length(input$assessments)])
        
        pathways = append(pathways, list(pathway_1 = list(
          assessments=input$assessments,
          listwise=listwise$listwise,
          nom=input$nom,
          nom_cutoff=input$nom_cutoff,
          test_cutoff=input$mean_cutoff,
          weights=weights$w[1:length(input$assessments)])
          )
        )
                          
        
        if (pathway_status$last_pathway >= 2 & !is.null(input$assessments2) & 
            !is.null(input$nom2)) {
        
          mydata$meanscore_2 = identify_opti(data=mydata, 
                                             assessments=input$assessments2, 
                                             nom=input$nom2, 
                                             nom_cutoff=input$nom_cutoff2, 
                                             test_cutoff=input$mean_cutoff2,
                                             listwise=listwise$listwise,
                                             mode = "meanscores", 
                                             weights = weights$w2[1:length(input$assessments2)])
          
          pathways = append(pathways, list(pathway_2 = list(
            assessments=input$assessments2,
            listwise=listwise$listwise2,
            nom=input$nom2,
            nom_cutoff=input$nom_cutoff2,
            test_cutoff=input$mean_cutoff2,
            weights=weights$w2[1:length(input$assessments2)])
            )
          )
        }
        
        if (pathway_status$last_pathway >= 3 & !is.null(input$assessments3) & 
            !is.null(input$nom3)) {
        
          mydata$meanscore_3 = identify_opti(data=mydata, 
                                           assessments=input$assessments3, 
                                           nom=input$nom3, 
                                           nom_cutoff=input$nom_cutoff3, 
                                           test_cutoff=input$mean_cutoff3,
                                           listwise=listwise$listwise,
                                           mode = "meanscores", 
                                           weights = weights$w3[1:length(input$assessments3)])
          
          pathways = append(pathways, list(pathway_3 = list(
            assessments=input$assessments3,
            listwise=listwise$listwise3,
            nom=input$nom3,
            nom_cutoff=input$nom_cutoff3,
            test_cutoff=input$mean_cutoff3,
            weights=weights$w3[1:length(input$assessments3)])
            )
          )
        }
        
        if (pathway_status$last_pathway >= 4 & !is.null(input$assessments4) & 
            !is.null(input$nom)) {
          
          mydata$meanscore_4 = identify_opti(data=mydata, 
                                            assessments=input$assessments4, 
                                            nom=input$nom4, 
                                            nom_cutoff=input$nom_cutoff4, 
                                            test_cutoff=input$mean_cutoff4,
                                            listwise=listwise$listwise,
                                            mode = "meanscores", 
                                            weights = weights$w4[1:length(input$assessments4)])
          
          pathways = append(pathways, list(pathway_4 = list(
            assessments=input$assessments4,
            listwise=listwise$listwise4,
            nom=input$nom4,
            nom_cutoff=input$nom_cutoff4,
            test_cutoff=input$mean_cutoff4,
            weights=weights$w4[1:length(input$assessments4)])
            )
          )
        }
      
        results = equity_plot_multi(data=mydata, 
                                    group=input$group,
                                    reference_grp=filter_string,
                                    pathways=pathways,
                                    baseline_id_var=input$baseline_id_var,
                                    plot_metric=input$metric_all,
                                    selected_pathway=length(pathways)+1
        )
        
        
        # # process the equity table - format for display
        # tbl_long = results$summary_tbl
        # 
        # tbl_long$value = round(tbl_long$value, 3)
        # 
        # tbl_wide = pivot_wider(
        #   dplyr::filter(tbl_long, metric %in% 
        #                   c("count", "pct_identified", "RI", "RR", "CramerV")), 
        #   names_from=c("metric"))
        # 
        # if (length(group) == 1) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # } else if (length(group) == 2) {
        #   
        #   tbl_wide = tbl_wide[order(tbl_wide[[group[1]]], 
        #                             tbl_wide[[group[2]]],
        #                             tbl_wide$comparison,
        #                             decreasing=TRUE, na.last=FALSE),]
        # }
        # 
        # # load the table into the reactive object for display and download
        # tables$equity_table = dplyr::select(tbl_wide, -baseline)
        
        # show the plot
        return(results$p)
        
      }
      
    })   
    
    output$tbl <- DT::renderDataTable({
      
        # note: this table is constructed in the render plot call
        tables$equity_table
        
      }, filter="top")
    
      output$descr_table <- renderDataTable({
        
        # note: this table is constructed in the render plot call
        tables$group_stats
      
    }, filter="top")
    
    
  }) # closes observe
  

# generate report ---------------------------------------------------------
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    #filename = "report.pdf",
    filename = function(){paste0("report.", input$reportFormat)},
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      print(paste0("report.", input$reportFormat))
      
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data = dat(),
        run_notes=input$run_notes,
        file=upload_dataname$filename,
        data_filter_string=filters$data_filter_string,
        filter_group=filters$filter_group,
        filter_ref_grp1=filters$filter_ref_grp1,
        filter_ref_grp2=filters$filter_ref_grp2,
        filter_ref_grp3=filters$filter_ref_grp3,
        group=input$group,
        reference_grp1=input$reference_grp1,
        reference_grp2=input$reference_grp2,
        group_filter_string=filters$group_filter_string,
        assessments=input$assessments,
        weights=weights$w[1: length(input$assessments)],
        nom=input$nom,
        nom_cutoff=input$nom_cutoff,
        mean_cutoff=input$mean_cutoff,
        baseline_id_var=input$baseline_id_var,
        equity_tbl=tables$equity_table,
        group_stats_tbl=tables$group_stats)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app)
      
      # display the busy spinner
      show_modal_spinner(spin = "folding-cube") 
     
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      # remove the busy spinner
      remove_modal_spinner()
    }
  )

} # closes server

# Run the application 
shinyApp(ui = ui, server = server)
