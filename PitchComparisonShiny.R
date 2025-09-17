library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)

savant_with_names <- readRDS("data/statcast_with_names.rds")
# Helper function: clock string to degrees
clock_to_spinaxis <- function(clock_str) {
  parts <- strsplit(clock_str, ":")[[1]]
  hr <- as.numeric(parts[1]) %% 12
  mn <- as.numeric(parts[2])
  deg_clock <- (hr * 30) + (mn / 60) * 30
  spin_axis <- (deg_clock - 180) %% 360
  spin_axis
}

ui <- dashboardPage(
  dashboardHeader(title = "PitchComp"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pitch Search", tabName = "search", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Search tab
      tabItem(tabName = "search",
              fluidRow(
                # Input panel
                box(
                  title = "Search Parameters", status = "primary", solidHeader = TRUE,
                  width = 4,
                  
                  numericInput("pfx_x_input", "Horizontal Break (inches):", 0, -30, 30, 0.1),
                  numericInput("pfx_z_input", "Vertical Break (inches):", 0, -30, 30, 0.1),
                  numericInput("rpm_input", "Minimum RPM:", 2000, 1000, 4000, 50),
                  numericInput("vaa_input", "Desired VAA (negative = steeper):", -5, -30, 30, 0.1),
                  textInput("tilt_input", "Desired Tilt (clock format e.g., '1:30'):", "12:00"),
                  
                  selectInput("pitch_type_input", "Pitch Type:",
                              choices = c("4-Seam Fastball" = "4-seam fastball",
                                          "Sinker" = "sinker",
                                          "Cutter" = "cutter", 
                                          "Slider" = "slider",
                                          "Sweeper" = "sweeper",
                                          "Curveball" = "curveball",
                                          "Knuckle Curve" = "knuckle curve",
                                          "Changeup" = "changeup",
                                          "Split-Finger" = "split-finger",
                                          "Slurve" = "slurve",
                                          "Knuckleball" = "knuckleball")),
                  radioButtons("handedness_input", "Pitcher Handedness:", choices = list("Right-handed" = "R", "Left-handed" = "L"), selected = "R"),
                  actionButton("search_btn", "Find Closest Pitch", class = "btn-primary", style = "width:100%;")
                ),
                
                # Results panel
                box(
                  title = "Search Results", status = "success", solidHeader = TRUE,
                  width = 8,
                  conditionalPanel(
                    condition = "output.results_available",
                    h4("Closest Pitch Found:"),
                    verbatimTextOutput("pitch_summary"),
                    br(),
                    actionButton("view_video", "View Video", class = "btn-info", icon = icon("play")),
                    br(), br(),
                    uiOutput("video_player"),
                    br(),
                    h4("Detailed Results:"),
                    DT::dataTableOutput("results_table")
                  ),
                  conditionalPanel(
                    condition = "!output.results_available",
                    div(
                      style = "text-align: center; margin-top: 50px;",
                      icon("search", style = "font-size: 48px; color: #ccc;"),
                      h3("Enter search parameters and click 'Find Closest Pitch'", style = "color: #999;")
                    )
                  )
                )
              ),
              fluidRow(
                box(title = "Status", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("status_message"))
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About This App", status = "primary", solidHeader = TRUE, width = 12,
                    h3("PitchComp: Baseball Pitch Finder"),
                    p("PitchComp is an interactive Shiny web application that helps coaches, players, analysts, and fans identify Major League Baseball pitches that closely match custom movement and spin profiles. Built on full-season Statcast data from 2024, the app compares your desired pitch characteristics such as horizontal/vertical movement, spin rate (RPM), vertical approach angle (VAA), and spin-axis tilt (clock-face orientation) against every pitch thrown in the MLB season. It then returns the most similar real-world pitches and provides video of the closest match."),
                    h4("Key Features:"),
                    tags$ol(
                      tags$li("Custom Pitch Search: Enter your target movement (IVB and HB), minimum RPM, desired VAA, and preferred spin-axis tilt (e.g., 1:30)."),
                      tags$li("Filters: Limit results to specific pitch types (fastball, slider, curveball, etc.) and pitcher handedness."),
                      tags$li("Similarity Ranking: The app calculates a “total difference” score across all selected parameters and highlights the 10 closest matches."),
                      tags$li("Instant Video Playback: Watch Statcast video of the best-matching pitch directly inside the app."),
                      tags$li("Rich Results Table: View key metrics like spin rate, spin axis, movement, and VAA for each top match.")
                    ),
                    h4("Uses:"),
                    tags$ul(
                      tags$li(strong("Player Development:"), "Find MLB comparables for high school or college pitchers to guide training goals."),
                      tags$li(strong("Scouting & Analysis:"), "Identify big-league arsenals that resemble a pitcher’s desired repertoire."),
                    ),
                    h4("Data Requirements:"),
                    tags$ul(
                      tags$li("pitch_hand, pitch_name, release_spin_rate"),
                      tags$li("pfx_x, pfx_z, VAA, spin_axis, tilt"),
                      tags$li("pitcher_id, play_id, pitcher_name (or Name column)")
                    )
                )
              )
      )
    )
  )
)

# ----------------------
# SERVER
# ----------------------
server <- function(input, output, session) {
  
  values <- reactiveValues(results = NULL, closest_pitch = NULL, search_performed = FALSE)
  
  data_available <- reactive({ exists("savant_with_names") && is.data.frame(savant_with_names) })
  
  # Search button
  observeEvent(input$search_btn, {
    
    # Clear previous video
    output$video_player <- renderUI({ NULL })
    
    if (!data_available()) {
      values$search_performed <- TRUE
      output$status_message <- renderText("Error: Dataset not found.")
      return()
    }
    
    # Validate tilt input
    if (!grepl("^[0-9]{1,2}:[0-9]{2}$", input$tilt_input)) {
      output$status_message <- renderText("Error: Tilt must be in format 'H:MM' (e.g., '1:30')")
      return()
    }
    
    tryCatch({
      target_spin_axis <- clock_to_spinaxis(input$tilt_input)
      
      filtered_data <- savant_with_names %>%
        filter(pitch_hand == input$handedness_input,
               tolower(pitch_name) == input$pitch_type_input,
               release_spin_rate >= input$rpm_input)
      
      if (nrow(filtered_data) == 0) {
        values$search_performed <- TRUE
        output$status_message <- renderText("No pitches match the basic filters.")
        return()
      }
      
      closest_pitch <- filtered_data %>%
        mutate(
          pfx_x_diff = abs(pfx_x * -12 - input$pfx_x_input),
          pfx_z_diff = abs(pfx_z * 12 - input$pfx_z_input),
          vaa_diff = abs(VAA - input$vaa_input),
          tilt_diff = abs(((spin_axis - target_spin_axis + 180) %% 360) - 180),
          total_diff = pfx_x_diff + pfx_z_diff + vaa_diff + tilt_diff
        ) %>%
        arrange(total_diff) %>%
        slice(1)
      
      values$closest_pitch <- closest_pitch
      values$results <- filtered_data %>%
        mutate(
          pfx_x_diff = abs(pfx_x * -12 - input$pfx_x_input),
          pfx_z_diff = abs(pfx_z * 12 - input$pfx_z_input),
          vaa_diff = abs(VAA - input$vaa_input),
          tilt_diff = abs(((spin_axis - target_spin_axis + 180) %% 360) - 180),
          total_diff = pfx_x_diff + pfx_z_diff + vaa_diff + tilt_diff
        ) %>%
        arrange(total_diff) %>%
        slice_head(n = 10)
      
      output$status_message <- renderText(paste("Search complete!", nrow(filtered_data), "pitches matched."))
      
    }, error = function(e) {
      output$status_message <- renderText(paste("Error during search:", e$message))
    })
  })
  
  # Output availability
  output$results_available <- reactive({ !is.null(values$closest_pitch) })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Pitch summary
  output$pitch_summary <- renderText({
    if (!is.null(values$closest_pitch)) {
      cp <- values$closest_pitch
      pitcher_name <- if("pitcher_name" %in% names(cp)) cp$pitcher_name else if("Name" %in% names(cp)) cp$Name else "Unknown Pitcher"
      sprintf(
        paste(
          "Pitcher: %s",
          "Pitch Name: %s",
          "RPM: %d",
          "Horizontal Break: %.2f (%.2f off target)",
          "Vertical Break: %.2f (%.2f off target)",
          "VAA: %.2f (%.2f off target)",
          "Tilt: %s (%.1f off spin-axis)",
          sep = "\n"
        ),
        pitcher_name,
        cp$pitch_name,
        cp$release_spin_rate,
        cp$pfx_x * -12, cp$pfx_x_diff,
        cp$pfx_z * 12, cp$pfx_z_diff,
        cp$VAA, cp$vaa_diff,
        if("tilt" %in% names(cp)) cp$tilt else "N/A", cp$tilt_diff
      )
    }
  })
  
  # Results table
  output$results_table <- DT::renderDataTable({
    if (!is.null(values$results)) {
      pitcher_col <- if("pitcher_name" %in% names(values$results)) "pitcher_name" else if("Name" %in% names(values$results)) "Name" else "pitcher_id"
      display_cols <- intersect(c(pitcher_col, "pitcher_id","pitch_name","release_spin_rate","pfx_x","pfx_z","VAA","spin_axis","total_diff"), names(values$results))
      df <- values$results %>% select(all_of(display_cols)) %>% 
        mutate(pfx_x=round(pfx_x*-12,2), pfx_z=round(pfx_z*12,2), VAA=round(VAA,2), total_diff=round(total_diff,2))
      colnames(df) <- gsub("^pitcher_name$|^Name$", "Pitcher Name", colnames(df))
      colnames(df) <- gsub("^pitcher_id$", "Pitcher ID", colnames(df))
      colnames(df) <- gsub("^pitch_name$", "Pitch", colnames(df))
      colnames(df) <- gsub("^release_spin_rate$", "RPM", colnames(df))
      colnames(df) <- gsub("^pfx_x$", "H-Break", colnames(df))
      colnames(df) <- gsub("^pfx_z$", "V-Break", colnames(df))
      colnames(df) <- gsub("^spin_axis$", "Spin Axis", colnames(df))
      colnames(df) <- gsub("^total_diff$", "Total Diff", colnames(df))
      df
    }
  }, options = list(pageLength=10, scrollX=TRUE))
  
  # Embedded video player
  observeEvent(input$view_video, {
    if (!is.null(values$closest_pitch) && "play_id" %in% names(values$closest_pitch)) {
      video_url <- tryCatch(sabRmetrics::get_video_url(values$closest_pitch$play_id[1]), error=function(e) NULL)
      if (!is.null(video_url)) {
        output$video_player <- renderUI({
          tags$iframe(src=video_url, width="100%", height="400px", frameborder=0,
                      allow="autoplay; encrypted-media", allowfullscreen=NA)
        })
      } else {
        output$video_player <- renderUI({ tags$p("Video URL could not be retrieved.") })
      }
    } else {
      output$video_player <- renderUI({ tags$p("No video available for this pitch.") })
    }
  })
}

# ----------------------
# Run App
# ----------------------
shinyApp(ui=ui, server=server)
