library(shiny)
library(tidyverse)
library(DT)
library(clue)

# GenomicEpi Minty colors
primary_color <- "#3dbb9d"    # minty teal (primary)
secondary_color <- "#265d5a"  # dark teal (header bg)
button_fg <- "white"
table_header_bg <- "#e3f1f0"  # very light minty blue
table_header_fg <- "#265d5a"  # dark teal text

ui <- fluidPage(
  tags$head(
    tags$title("Jersey Optimizer"),   # <-- Set browser tab title here
    tags$link(rel = "shortcut icon", href = "jersey.png"),
    tags$style(HTML(sprintf("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background-color: #f7faf9; }
      h3 { color: %s; font-weight: 700; margin-top: 30px; }
      .btn-primary { background-color: %s; border-color: %s; color: %s; font-weight: 600; }
      .btn-primary:hover { background-color: #2a877d; border-color: #2a877d; }
      .btn-reset {
        background-color: #f7c6ce; border-color: #f7c6ce; color: black;
        font-weight: 600;
      }
      .btn-reset:hover {
        background-color: #f3a5b0; border-color: #f3a5b0;
      }
      .modal-header { background-color: %s; color: %s; }
      table.dataTable thead th { background-color: %s; color: %s; }
      .step-label {
        font-weight: 700;
        font-size: 1.2em;
        margin-top: 20px;
        color: %s;
      }
      .instruction-text {
        font-style: italic;
        color: %s;
        margin-bottom: 10px;
      }
    ",
                            primary_color,
                            primary_color, primary_color, button_fg,
                            secondary_color, button_fg,
                            table_header_bg, table_header_fg,
                            primary_color, secondary_color)))
  ),
  
  titlePanel(
    div(style = sprintf("background-color: %s; color: %s; padding: 15px; border-radius: 8px; font-weight: 700;",
                        secondary_color, button_fg),
        "Jersey Number Optimizer")
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "step-label", "Step 1: Enter Jersey Numbers"),
      div(class = "instruction-text", "Enter available jersey numbers separated by commas or ranges (e.g. 1-5,7,10). Click 'Submit Jerseys'."),
      textInput("jerseys", NULL, placeholder = "e.g. 1-5,7,10"),
      actionButton("submit_jerseys", "Submit Jerseys", class = "btn-primary"),
      actionButton("reset_jerseys", "Reset Jerseys", class = "btn-reset"),
      
      hr(),
      
      div(class = "step-label", "Step 2: Add Players"),
      div(class = "instruction-text", "Click 'Add Player' and enter player name and their preferred jerseys."),
      actionButton("add_player", "➕ Add Player", class = "btn-primary"),
      actionButton("reset_players", "Reset Players", class = "btn-reset"),
      
      hr(),
      
      div(class = "step-label", "Step 3: Optimize Assignments"),
      div(class = "instruction-text", "Click 'Optimize Assignments' to assign jerseys minimizing total preference rank."),
      actionButton("optimize", "Optimize Assignments", class = "btn-primary"),
      actionButton("reset_assignments", "Reset Assignments", class = "btn-reset"),
      br(), br(),
      downloadButton("download_assignments", "Download Assignments", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Players & Preferences"),
      DTOutput("player_table"),
      hr(),
      h3("Optimization Result"),
      tableOutput("assignments"),
      verbatimTextOutput("total_score")
    )
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(
    jerseys = NULL,
    prefs = tibble(
      Player = character(),
      Choice1 = integer(), Choice2 = integer(),
      Choice3 = integer(), Choice4 = integer(), Choice5 = integer()
    ),
    assignment = NULL
  )
  
  parse_numbers <- function(txt) {
    parts <- str_split(txt, ",")[[1]] %>% str_trim()
    nums <- c()
    for (p in parts) {
      if (str_detect(p, "-")) {
        limits <- str_split(p, "-")[[1]] %>% as.integer()
        if (length(limits) == 2 && all(!is.na(limits))) {
          nums <- c(nums, seq(limits[1], limits[2]))
        }
      } else {
        n <- as.integer(p)
        if (!is.na(n)) nums <- c(nums, n)
      }
    }
    sort(unique(nums))
  }
  
  observeEvent(input$submit_jerseys, {
    js <- parse_numbers(input$jerseys)
    if (length(js) == 0) {
      showNotification("No valid jersey numbers found.", type = "error")
      vals$jerseys <- NULL
    } else {
      vals$jerseys <- js
      showNotification(paste("Jerseys set:", paste(js, collapse = ", ")), type = "message")
    }
  })
  
  observeEvent(input$reset_jerseys, {
    vals$jerseys <- NULL
    updateTextInput(session, "jerseys", value = "")
    showNotification("Jerseys reset.", type = "message")
  })
  
  observeEvent(input$add_player, {
    req(vals$jerseys)
    jerseys_choices <- c("", vals$jerseys)
    showModal(modalDialog(
      title = "Add Player and Jersey Preferences",
      textInput("new_player_name", "Player Name:", ""),
      fluidRow(
        column(2, selectInput("choice1", "1st Choice", choices = jerseys_choices)),
        column(2, selectInput("choice2", "2nd Choice", choices = jerseys_choices)),
        column(2, selectInput("choice3", "3rd Choice", choices = jerseys_choices)),
        column(2, selectInput("choice4", "4th Choice", choices = jerseys_choices)),
        column(2, selectInput("choice5", "5th Choice", choices = jerseys_choices))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_player", "Add Player", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_add_player, {
    req(input$new_player_name)
    if (input$new_player_name %in% vals$prefs$Player) {
      showNotification("Player name already exists!", type = "error")
      return()
    }
    prefs_vec <- c(input$choice1, input$choice2, input$choice3, input$choice4, input$choice5)
    prefs_int <- as.integer(prefs_vec)
    invalid <- any(!is.na(prefs_int) & !(prefs_int %in% vals$jerseys))
    if (invalid) {
      showNotification("One or more choices are invalid jersey numbers.", type = "error")
      return()
    }
    new_row <- tibble(
      Player = input$new_player_name,
      Choice1 = prefs_int[1],
      Choice2 = prefs_int[2],
      Choice3 = prefs_int[3],
      Choice4 = prefs_int[4],
      Choice5 = prefs_int[5]
    )
    vals$prefs <- bind_rows(vals$prefs, new_row)
    removeModal()
  })
  
  observeEvent(input$reset_players, {
    vals$prefs <- tibble(
      Player = character(),
      Choice1 = integer(), Choice2 = integer(),
      Choice3 = integer(), Choice4 = integer(), Choice5 = integer()
    )
    showNotification("Players reset.", type = "message")
  })
  
  output$player_table <- renderDT({
    datatable(vals$prefs, rownames = FALSE, options = list(dom = 't'))
  })
  
  assignment_result <- reactiveVal(NULL)
  
  observeEvent(input$optimize, {
    req(vals$prefs, vals$jerseys)
    players <- vals$prefs$Player
    choices <- vals$prefs %>% select(starts_with("Choice"))
    jerseys <- vals$jerseys
    
    if (length(jerseys) < length(players)) {
      showNotification("Not enough jerseys for all players.", type = "error")
      assignment_result(NULL)
      return()
    }
    
    cost_matrix <- matrix(99, nrow = nrow(choices), ncol = length(jerseys))
    for (i in seq_len(nrow(choices))) {
      seen <- c()
      for (j in seq_len(ncol(choices))) {
        jersey_choice <- choices[i, j, drop = TRUE]
        if (!is.na(jersey_choice) && !(jersey_choice %in% seen)) {
          col_index <- which(jerseys == jersey_choice)
          if (length(col_index) == 1) {
            cost_matrix[i, col_index] <- j
            seen <- c(seen, jersey_choice)
          }
        }
      }
    }
    
    assignment <- tryCatch({
      solve_LSAP(cost_matrix)
    }, error = function(e) {
      showNotification(paste("Optimization error:", e$message), type = "error")
      NULL
    })
    if (is.null(assignment)) {
      assignment_result(NULL)
      return()
    }
    
    assigned_jerseys <- jerseys[assignment]
    assigned_ranks <- map2_dbl(1:length(players), assignment, ~ cost_matrix[.x, .y])
    
    res <- tibble(
      Player = players,
      Assigned_Jersey = assigned_jerseys,
      Rank = assigned_ranks
    )
    
    assignment_result(res)
  })
  
  observeEvent(input$reset_assignments, {
    assignment_result(NULL)
    showNotification("Assignments reset.", type = "message")
  })
  
  output$assignments <- renderTable({
    req(assignment_result())
    assignment_result() %>% select(Player, Assigned_Jersey)
  })
  
  output$total_score <- renderText({
    req(assignment_result())
    score <- sum(assignment_result()$Rank)
    paste("Total sum of preference ranks:", score)
  })
  
  output$download_assignments <- downloadHandler(
    filename = function() {
      paste0("jersey_assignments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(assignment_result())
      write.csv(assignment_result(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

