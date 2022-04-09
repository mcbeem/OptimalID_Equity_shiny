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
#library(reactable)


# import functions
source("functions.R")

# ui ----------------------------------------------------------------------
# Define UI for the application
ui <- fluidPage(

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
    
    # Application title
    headerPanel("Optimal ID equity explorer"),
    
    tabsetPanel(

      tabPanel("About",
               
               HTML("<br>"),
               HTML("<br>"),
               htmltools::includeMarkdown("helpText.md")
        ),
      
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
            selected = "csv"
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
      
      tabPanel("Plot",
        
        sidebarLayout(
          sidebarPanel(
            
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
              inputId = "assessments", 
              label = "Assessments", 
              multiple = TRUE,
              choices = NULL
              ),
            
            selectInput(
              inputId = "nom", 
              label = "Nomination instrument", 
              choices = NULL
            ),
            
            selectInput(
              inputId = "baseline_id_var", 
              label = "Baseline id variable", 
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
      
          ), # closes sidebarPanel
  
        mainPanel(
          
          HTML("<br>"),
 
          selectInput(
            inputId = "metric", 
            label = "Equity metric", 
            choices = c("Count", "Representation Index", "Proportion Identified", 
                        "Relative Risk", "Cramer's V"),
            selected = "Count"),
          
          HTML("<br>"),
          
          plotOutput("plot", width="120%", height= "500px"),
        )
      ) # closes sidebarLayout
    ), # closes tabPanel 'Plot'
    
  tabPanel("Equity table",
        fluidPage(
           DTOutput('tbl')
        )
    ),
  
  tabPanel("Group statistics",
       fluidPage(
         DTOutput('descr_table')
       )
    ),
  
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
  shinyjs::disable('loadFile')
  
  # enable the load file button once a file is loaded
  observeEvent(input$file, {
    shinyjs::enable('loadFile')
  })

  
  # initialize the 'dat' reactive object
  dat <- reactiveVal()
  
  filters <- reactiveValues(
    data_filter_string=NULL,
    group_filter_string=NULL,
    filter_group=NULL,
    filter_ref_grp1=NULL,
    filter_ref_grp2=NULL,
    filter_ref_grp3=NULL,
  )
  
  tables <- reactiveValues(
    equity_table=NULL,
    group_stats=NULL,
  )
  
  upload_dataname <- reactiveValues(
    filename=NULL
  )
  
  
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
    
    output$dat <- DT::renderDT(
      mydata 
    )
    
    # store the loaded data in the reactive object
    dat(mydata)
    
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
        inputId = "nom", 
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
    
    output$plot <- renderPlot({
      
      
      if (!is.null(group) & !is.null(input$assessments) & !is.null(input$nom)) {
        
        # we construct 3 pieces of output here: the plot and both tables
        #  this is needed to prevent users from needing to click on the all
        #  the tabs prior to downloading the report
        
        # construct the descriptive statistics table and load it into
        #  the appropriate reactive element
        tables$group_stats <- descr_table(data=dat(),
                                          group=group,
                                          vars=unique(c(input$assessments, input$nom)),
                                          
        )
        
        # 'results' is a list containing both the plot ($p) and the raw equity statistics
        #   table
        results = equity_plot(data=dat(),
                    group=input$group,
                    reference_grp=filter_string,
                    assessments=input$assessments,
                    nom=input$nom,
                    nom_cutoff=input$nom_cutoff,
                    mean_cutoff=input$mean_cutoff,
                    baseline_id_var=input$baseline_id_var,
                    plot_metric=input$metric)
        
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
        nom=input$nom,
        nom_cutoff=input$nom_cutoff,
        mean_cutoff=input$mean_cutoff,
        baseline_id_var=input$baseline_id_var,
        equity_tbl=tables$equity_table,
        group_stats_tbl=tables$group_stats)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

} # closes server

# Run the application 
shinyApp(ui = ui, server = server)
