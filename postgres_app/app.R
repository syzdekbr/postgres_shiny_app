
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
library(shinyauthr) # For password
library(readxl) # To read external file with password info

# UI ----------------------------------------------------------------------

  # UI is user inputs and outputs
ui <- fluidPage(
###*** Password modals
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  uiOutput("sidebarLayout_ui")
    # sidebarLayout(
    #   sidebarPanel(
    #     h1("Data Setup"), # Title
    #   # Select table and give schema info. Rendered in server section below
    #     h2("Select Table"),
    #     uiOutput("select_table"),
    #   # Select columns to include in datatable. Rendered in server
    #     h2("Select Columns to Include"),
    #     uiOutput("select_columns_ui")
    #   ),
    #   mainPanel(
    #     h1("Records"),
    #     h2("All Records. Click to include."),
    #     DTOutput("all_records"),
    #     h2("Selected Records. Can edit"),
    #     rHandsontableOutput("selected_table"),
    #     br(),
    #     hr(),
    #   # Row with output options
    #     fluidRow(
    #       h2("Output Records to file or report"),
    #       column(6,
    #       # Will save records to csv
    #         h3("Download csv"),
    #         downloadButton(
    #           outputId = "save_all_records", 
    #           label = "Download"
    #           )
    #         ), # End column
    #       column(6,
    #       h3("Download Report"),
    #       # Choose report format (limited options here)
    #         radioButtons(
    #           inputId = "report_format",
    #           label = "Choose format for report",
    #           choices =
    #             c("HTML" = "html"), inline = TRUE
    #         ),
    #       # To generate report
    #         downloadButton(
    #           outputId = "generate_report",
    #           label = "Generate Report",
    #           icon = shiny::icon("file-contract") # Optional icon
    #           )
    #       ) # End column
    #     ) # End fluidRow
    #   ) # End mainPanel
    # ) # End sidebarLayout
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
# Rewrote shinyauthr loginserver script to be case insensitive user_name, comment out if want user_name
  # case sensitive
  source("shinyauthr_login_rewrite.R")

###*** UI moved here to renderUI to user shinyauthr
  output$sidebarLayout_ui <- renderUI({
    req(credentials()$user_auth)
    sidebarLayout(
      sidebarPanel(
        h1("Data Setup"), # Title
        # Select table and give schema info. Rendered in server section below
        h2("Select Table"),
        uiOutput("select_table"),
        # Select columns to include in datatable. Rendered in server
        h2("Select Columns to Include"),
        uiOutput("select_columns_ui")
      ),
      mainPanel(
        h1("Records"),
        h2("All Records. Click to include."),
        DTOutput("all_records"),
        h2("Selected Records. Can edit"),
        rHandsontableOutput("selected_table"),
        br(),
        hr(),
        # Row with output options
        fluidRow(
          h2("Output Records to file or report"),
          column(6,
                 # Will save records to csv
                 h3("Download csv"),
                 downloadButton(
                   outputId = "save_all_records", 
                   label = "Download"
                 )
          ), # End column
          column(6,
                 h3("Download Report"),
                 # Choose report format (limited options here)
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
          ) # End column
        ) # End fluidRow
      ) # End mainPanel
    ) # End sidebarLayout
  })
  
# Password ----------------------------------------------------------------
  password_data <- readxl::read_xlsx("password_data_file.xlsx")
  user_base <- password_data %>% 
    mutate(
      user = tolower(user_name), # tolower to make case insensitive
      password = user_password,
      permissions = rep("standard", nrow(.)),
      name = paste(user_first_name, user_last_name)
    )
  
  ###*** Following sets up dataframe from collected spreadsheet with user information
  ### credentials()$user_auth will then be true when correct combo entered, use to validate
  credentials <- loginServer( # removed shinyauthr::loginServer to make case insensitive
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show the module based on password success
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  ###*** Logout button and action- will refresh page- used instead of logoutServer b/c rest of ui needs to update as on load
  
  output$logout_button_ui <- renderUI({
    req(credentials()$user_auth)
    shiny::actionButton("logout_button", "Log Out/Manage Password", 
                        class = "btn-danger", style = "color: white; padding: 8px;")
  })
  
  observeEvent(input$logout_button,{
    refresh()
  })
  
  ###*** Close password page and open sidebar and mainpage
  observe({
    if(credentials()$user_auth) {
      # Removes password page upon success
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("password_div") # password help hide/show only at login
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show("password_div") # password help hide/show only at login
    }
  })

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
      collect() %>% # Render to tibble
      mutate(table_key = paste(input$table_select, rownames(.), sep = "_"), .before = 1)
  })
  
# Renders table to display in output
  output$all_records <- renderDT({
    datatable(
      all_records_data$df,
      options = list(
        scrollX = TRUE # to scroll across screen for wide records
      )#,selection = 'single'
    )
  })
  
###*** Selected records from all records
# Reactive value that will hold selected records
  selected_records_data <- reactiveValues(
    df = data.frame() # initialize
  )
  
# Will update reactive value to include new selected records from all records when datatable clicked
  # will merge with already selected records, including edited records
  observeEvent(input$all_records_rows_selected,{
    req(input$all_records_rows_selected) # Need selected records
  # If no records selected, then reactiveValues just the selected record
    if(nrow(selected_records_data$df) == 0){
      selected_records_data$df <- all_records_data$df[input$all_records_rows_selected,] %>% 
        # I've modified across all variables to be character, so can join tables with different column types
        # If want to preserve column types, comment out mutate(across(everything()... and uncomment
        # mutate(across(where(is.POSIXct..., needed b/c Modify time variables b/c rhandsontable can't handle
       # mutate(across(where(is.POSIXct), ~as.character(.x)))
        mutate(across(everything(), ~as.character(.x)))
    # If already have selected records, then get new records, those that don't match table_key
    } else{
      new_rows <- all_records_data$df[input$all_records_rows_selected,] %>% 
      # I've modified across all variables to be character, so can join tables with different column types
        # If want to preserve column types, comment out mutate(across(everything()... and uncomment
        # mutate(across(where(is.POSIXct..., needed b/c Modify time variables b/c rhandsontable can't handle
        # mutate(across(where(is.POSIXct), ~as.character(.x))) %>% 
        mutate(across(everything(), ~as.character(.x))) %>% 
      # This will include new records, those without matching "table_key"
        anti_join(., hot_to_r(input$selected_table), by = "table_key")
    # Merge records already selected with new records. If not done, selected records will erase, with 
      # each table click, and erase edited rows
      selected_records_data$df <- bind_rows(
        hot_to_r(input$selected_table),  
        new_rows 
      )
    }
  })
  
# Render selected records. Rhandsontable b/c it is editable table
  output$selected_table <- renderRHandsontable(
    rhandsontable(
      selected_records_data$df,
      selectCallback = TRUE,readOnly = TRUE
    )
  )
  

# Generate modal to edit table --------------------------------------------

# Generate an rhandsontable that will allow editing that is a selected row from selected records
  output$rhandsontable_selected_row <- renderRHandsontable({
    req(input$selected_table) # Need to have selected records
    out <-  hot_to_r (input$selected_table) # dataframe with all selected records
  # Generate rhandsontable of selected row
    rhandsontable(
      # input$<table_name>_select$select$r gives row number of clicked row
      out[input$selected_table_select$select$r, ] 
      ) %>% 
      # This prevents the key column from being changed, can comment if it's ok to edit, but may create  conflicts
        hot_col("table_key", readOnly = TRUE)
  })
  
# Generate a modal of selected row that user can edit
  observeEvent(input$selected_table_select$select$r,{ # wait until row clicked
    showModal(
      modalDialog(title = "Edit Selected Row",
                  rHandsontableOutput("rhandsontable_selected_row"), # rhandsontable of selected row
                  size = "l",
                # Button options to dismiss without edit or to commit edit
                  footer = tagList(modalButton("No edit"), 
                                   actionButton("save_edit", "Save edit"))
                  )
              )
            })
  
# Reactive values to hold the row that will be edited in modal
  edit_selected_row <- reactiveValues(
    df = data.frame() # initialize
  )
  
# When modal appears, capture entered data
  observe({
    req(input$rhandsontable_selected_row)
  # hot_to_r converts handsontable to r dataframe, including edited data, save to reactiveValue
    edit_selected_row$df <-  hot_to_r(input$rhandsontable_selected_row) 
  })
  
# When user clicks "Save edits" will replace the selected row in selected records with edited row in modal
  observeEvent(input$save_edit,{
    selected_records_data$df[input$selected_table_select$select$r, ]  <- edit_selected_row$df 
    removeModal() # Modal removed after saving
  })
  

# Output- download csv and report -----------------------------------------
  # Will retrieve selected records that have been modified in download file below
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