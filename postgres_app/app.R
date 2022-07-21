
###*** App will read postgres database, allow user to manipulate, 
###*    and download file or report

# Load Libraries ----------------------------------------------------------

library(DBI) # for database operations
library(shiny) # for app
library(glue) # for pasting into sql queries
library(DT) # for nicely styled datatables
library(pool) # to handle database connection
library(rhandsontable) # for editable datatable
library(lubridate) # for handling dates
library(dplyr) # for data manipulation
library(dbplyr) # Required for database manipulation

# UI ----------------------------------------------------------------------

  # UI is user inputs and outputs
ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        h1("Data Setup"), # Title
      # Select table and give schema info. Rendered in server section below
        h2("Select Table"),
        uiOutput("select_table"),
      # Select columns to include in datatable. Rendered in server
        h2("Select Columns to Include"),
        uiOutput("select_columns_ui")
      ), # final sidebar paren
      mainPanel(
        h1("Records"),
        h2("All Records. Click to include."),
        DTOutput("all_records"),
        h2("Selected Records. Can edit"),
        rHandsontableOutput("selected_table"),
        br(),
        h2("Output Records to file or report"),
      # Will save records to csv
        h3("Download csv"),
        downloadButton(
          outputId = "save_all_records", 
          label = "Download"
          ),
      # Choose report format (limited options here)
        br(),
        hr(),
        h3("Download Report"),
        radioButtons(
          inputId = "report_format",
          label = "Choose format for report",
          choices =
            c("HTML" = "html"), inline = TRUE
        ),
      # To generate report
        downloadButton(
          outputId = "generate_report",
          label = "Generate Report",
          icon = shiny::icon("file-contract") # Optional icon
          )
      )
    )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

# Establish Datbase Connection --------------------------------------------
  # This can be called outside of server in global. 
    #Called here to connect each app load
  # Open db connection. Connection then referenced as "pool"
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "pfmegrnargs",
    host = "hh-pgsql-public.ebi.ac.uk", # This is public db on genes
    port = "5432",
    user = "reader",
    password = "NWDMCE5xdipIjRrp" # Can store as system variable to hide
  )
  
  # Close connection on app close. This will not terminate connection
    # if there are multiple users
  onStop(function() {
    poolClose(pool)
  })

# Get DB Metadata ---------------------------------------------------------

# Will retrieve schema information for selected table
  data_schema <- reactive({
    req(input$table_select) # Wait for table options to load
    dbGetQuery(pool,
               # Get limited information on selected table. 
               # I don't know if every table has column_name and data_type
               # so app might fail if table doesn't have
               glue(
                 "SELECT column_name, data_type
                  FROM information_schema.columns
                  WHERE table_name =  '{input$table_select}';"
               ))
  })

# Present table with schema information to give overview of table
  output$column_schema_table <- renderDT(
    data_schema()
  )
  
###*** Select columns to include in record display
# This will retrieve available column names for table
  selected_columns <- reactive({
    req(input$table_select) # Wait until table loads
    pool %>% 
      tbl({input$table_select}) %>% 
      colnames()
  })

###*** This renders to UI the list of tables to display, schema, and
  # column select options in one chunk
  output$select_table <- renderUI({
    tagList( # because multiple ui
    # Select table
      selectInput(inputId = "table_select", 
                  label = "Select Table", 
                  choices = dbListTables(pool) %>% 
                              sort # alphabetical order
                  ), 
      h2("Table Schema Information"),
    # Table schema information
      DTOutput("column_schema_table")
    )
  })
  
# THis gives checkbox to select columns to include in datatable
  output$select_columns_ui <- renderUI({
    req(selected_columns()) # Wait until column options load
    checkboxGroupInput(inputId = "select_columns",
                       label = "Select Columns",
                       choices = selected_columns(), # Available columns
                       selected = selected_columns()) # Selected all- default
  })

# Records- all and selected -----------------------------------------------

# This is reactive that will hold all records in table (for use across functions)
  all_records_data <- reactiveValues(
    df = data.frame()
  )
  
# Assigns selected table to reactive value
  observeEvent(input$select_columns,{ # When columns chosen will update
    all_records_data$df <- pool %>% 
      tbl({input$table_select}) %>% # Selected table
      head() %>% # Only returns a few records in case table is long
      dplyr::select({input$select_columns}) %>% # Only selected columns
      collect() # Render to tibble
  })
  
# Renders table to display in output
  output$all_records <- renderDT({
    datatable(
      all_records_data$df,
      options = list(
        scrollX = TRUE # to scroll across screen for wide records
      )
    )
  })
  
###*** Selected records from all records
# Reactive value that will hold selected records
  selected_records_data <- reactiveValues(
    df = data.frame() # initialize
  )
  
# Will update reactive value to include selected records from all records
  observe({
    req(input$all_records_rows_selected) # Need selected records
    selected_records_data$df <- all_records_data$df[input$all_records_rows_selected,]
  })
  
# Render selected records. Rhandsontable b/c it is editable table
  output$selected_table <- renderRHandsontable(
    rhandsontable(
      selected_records_data$df %>% 
      # Modify time variables b/c rhandsontable can't handle
        mutate(across(where(is.POSIXct), ~as.character(.x)))
    )
  )
  # Will retrieve selected records that have been modified in download file below

# Output- download csv and report -----------------------------------------

  ## Gives exact time to avoid duplicate file names
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
###*** Download csv of modified rhandsontable
  output$save_all_records <- downloadHandler(
    filename = function() {
    # Can modify or accept inputs to dynamically update
      paste("full_record_", humanTime(), "_rna_data.csv", sep = "")
    },
    content = function(file) {
      cd <- hot_to_r(input$selected_table) # COntents is modified rhandsontable
      write.csv(cd, file, row.names = F)
    }
  )
  
###*** Download rmarkdown report
  output$generate_report <- downloadHandler(
    # Can generate other report formats. Here, just using html
    filename = function() {
      paste('postgres_report', format(Sys.time(), "%Y%m%d"), 
            sep = '_', paste0('.', input$report_format))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "postgres_output.Rmd")
      file.copy("postgres_report.Rmd", tempReport, overwrite = TRUE)
    # Optional parameters to pass to rmarkdown 
      params <- list(report_format = input$report_format,
                     report_table = hot_to_r(input$selected_table) 
                     
      )
      # # Knit the document, passing in the `params` list, and eval it in a
      # # child of the global environment (this isolates the code in the document
      # # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        # Get yaml output format language
        output_format = switch(input$report_format,
                               html = 'html_document'
        ),
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
} # Final server bracket

shinyApp(ui = ui, server = server)