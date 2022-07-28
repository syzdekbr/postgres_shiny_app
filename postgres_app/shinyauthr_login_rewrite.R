###*** Rewrite of shinyauthr::loginServer to make user_name case insensitive

# shinyauthr::loginServer rewrite -----------------------------------------

loginServer <- function(id, 
                        data,
                        user_col,
                        pwd_col,
                        sodium_hashed = FALSE,
                        log_out = shiny::reactiveVal(),
                        reload_on_logout = FALSE,
                        cookie_logins = FALSE,
                        sessionid_col,
                        cookie_getter,
                        cookie_setter) {
  
  # if colnames are strings convert them to symbols
  try_class_uc <- try(class(user_col), silent = TRUE)
  if (try_class_uc == "character") {
    user_col <- rlang::sym(user_col)
  }
  
  try_class_pc <- try(class(pwd_col), silent = TRUE)
  if (try_class_pc == "character") {
    pwd_col <- rlang::sym(pwd_col)
  }
  
  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) | missing(sessionid_col))) {
    stop("if cookie_logins = TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  } else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }
  
  # ensure all text columns are character class
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL, cookie_already_checked = FALSE)
      
      shiny::observeEvent(log_out(), {
        if (cookie_logins) {
          shinyjs::js$rmcookie()
        }
        
        if (reload_on_logout) {
          session$reload()
        } else {
          shiny::updateTextInput(session, "password", value = "")
          credentials$user_auth <- FALSE
          credentials$info <- NULL
        }
      })
      
      shiny::observe({
        if (cookie_logins) {
          if (credentials$user_auth) {
            shinyjs::hide(id = "panel")
          } else if (credentials$cookie_already_checked) {
            shinyjs::show(id = "panel")
          }
        } else {
          shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
        }
      })
      
      if (cookie_logins) {
        
        # possibility 1: login through a present valid cookie
        # first, check for a cookie once javascript is ready
        shiny::observeEvent(shiny::isTruthy(shinyjs::js$getcookie()), {
          shinyjs::js$getcookie()
        })
        # second, once cookie is found try to use it
        shiny::observeEvent(input$jscookie, {
          credentials$cookie_already_checked <- TRUE
          
          # if already logged in or cookie missing, ignore change in input$jscookie
          shiny::req(
            credentials$user_auth == FALSE,
            is.null(input$jscookie) == FALSE,
            nchar(input$jscookie) > 0
          )
          
          cookie_data <- dplyr::filter(cookie_getter(), {{sessionid_col}} == input$jscookie)
          
          if (nrow(cookie_data) != 1) {
            shinyjs::js$rmcookie()
          } else {
            # if valid cookie, we reset it to update expiry date
            .userid <- dplyr::pull(cookie_data, {{user_col}})
            .sessionid <- randomString()
            
            shinyjs::js$setcookie(.sessionid)
            
            cookie_setter(.userid, .sessionid)
            
            cookie_data <- utils::head(dplyr::filter(cookie_getter(), {{sessionid_col}} == .sessionid, {{user_col}} == .userid))
            
            credentials$user_auth <- TRUE
            credentials$info <- dplyr::bind_cols(
              dplyr::filter(data, {{user_col}} == .userid),
              dplyr::select(cookie_data, -{{user_col}})
            )
          }
        })
        
      }
      
      # possibility 2: login through login button
      shiny::observeEvent(input$button, {
        
        # check for match of input username to username column in data
        row_username <- which(dplyr::pull(data, {{user_col}}) == tolower(input$user_name))
        
        if (length(row_username)) {
          row_password <- dplyr::filter(data, dplyr::row_number() == row_username)
          row_password <- dplyr::pull(row_password, {{pwd_col}})
          if (sodium_hashed) {
            password_match <- sodium::password_verify(row_password, input$password)
          } else {
            password_match <- identical(row_password, input$password)
          }
        } else {
          password_match <- FALSE
        }
        
        # if user name row and password name row are same, credentials are valid
        if (length(row_username) == 1 && password_match) {
          
          credentials$user_auth <- TRUE
          credentials$info <- dplyr::filter(data, {{user_col}} == tolower(input$user_name))
          
          if (cookie_logins) {
            .sessionid <- randomString()
            shinyjs::js$setcookie(.sessionid)
            cookie_setter(tolower(input$user_name), .sessionid)
            cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{user_col}}), {{sessionid_col}} == .sessionid)
            if (nrow(cookie_data) == 1) {
              credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
            }
          }
          
        } else { # if not valid temporarily show error message to user
          shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
        }
      })
      
      # return reactive list containing auth boolean and user information
      shiny::reactive({
        shiny::reactiveValuesToList(credentials)
      })
      
    }
  )
}

