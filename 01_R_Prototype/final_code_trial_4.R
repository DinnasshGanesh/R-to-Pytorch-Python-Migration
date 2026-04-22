library(shiny)
library(shinythemes)
library(rdwd)
library(tidyverse)
library(lubridate)
library(DT)
library(curl)
library(RCurl)
library(remotes)
###################################rdwd::updateRdwd()
library(berryFunctions)
library(terra)


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Green Roof Water Storage Tank Dimensioning Tool"),
  sidebarLayout(
    sidebarPanel(
      h4("Location Selection"),
      selectizeInput("city", "Select City:", choices = NULL, options = list(placeholder = 'Type to search cities...')),
      actionButton("load_data", "Load Climate Data"),
      
      h4("Green Roof Parameters"),
      numericInput("substrate_depth", "Substrate Depth (mm) [20 - 1000]:", value = 200, min = 20, max = 1000),
      numericInput("pwp", "Soil Water Content at PWP (%) [2 - 20]:", value = 10, min = 2, max = 20),
      numericInput("field_capacity", "Field Capacity Water Content (%) [35 - 50]:", value = 45, min = 35, max = 50),
      numericInput("porosity", "Substrate Porosity (%) [30 - 60]:", value = 55, min = 30, max = 60),
      numericInput("init_storage", "Initial Soil Water Storage (mm):", value = 70, min = 0, max = 100),
      
      h4("Irrigation Parameters"),
      sliderInput("irrigation_months", "Irrigation Season:", min = 1, max = 12, value = c(5, 10)),
      numericInput("irrigation_area", "Irrigation Area (m²):", value = 100, min = 10, max = 1000),
      
      h4("Water Source Parameters"),
      numericInput("runoff_area", "Runoff Source Area (m²):", value = 100, min = 10, max = 1000),
      numericInput("initial_loss", "Initial Loss (mm) [0 - 5]:", value = 0.5, min = 0, max = 5),
      numericInput("runoff_coef", "Runoff Coefficient [0.2 - 1]:", value = 0.9, min = 0.2, max = 1, step = 0.1),
      numericInput("greywater", "Greywater Production (m³/d) [0.02 - 0.1]:", value = 0.1, min = 0.02, max = 0.1, step = 0.1),
      
      h4("Storage Tank"),
      numericInput("tank_volume", "Total Storage Tank Volume (m³):", value = 10, min = 1, max = 100),
      numericInput("init_tank_volume", "Initial Storage Tank Volume (m³):", value = 5, min = 0, max = 100),
      
      actionButton("calculate", "Calculate Water Balance")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h3("Water Balance Results"),
                 plotOutput("water_balance_plot"),
                 h4("Summary Statistics"),
                 DTOutput("summary_table"),
                 h4("Storage Tank Performance"),
                 DTOutput("tank_performance"),
                 h4("Suggested Tank Dimensions (based on Minimum Storage)"),
                 verbatimTextOutput("tank_dimensions_output")),
        tabPanel("Climate Data",
                 h3("Downloaded Climate Data"),
                 DTOutput("climate_data")),
        tabPanel("Station Info",
                 h3("Selected Station Information"),
                 verbatimTextOutput("station_info")),
        tabPanel("Station Map",
                 h3("DWD Weather Station Map"),
                 plotOutput("station_map", height = "500px")),
        tabPanel("About",
                 h3("About this Tool"),
                 p("This tool helps dimension water storage tanks for irrigated green roofs based on local climate conditions."),
                 p("It uses data from the German Weather Service (DWD) through the rdwd package."),
                 p("The calculations follow the water balance approach described in the provided Excel spreadsheet."))
      )
    )
  )
)

server <- function(input, output, session) {
  meta <- reactiveVal(data.frame())
  
  DEU <- reactiveVal(NULL)
  observe({
    tryCatch({
      DEU(terra::vect(system.file("extdata/DEU.gpkg", package="rdwd")))
    }, error = function(e) {
      showNotification(
        paste("Error loading Germany map data: ", e$message, 
              " Please ensure 'rdwd' package is installed correctly and has access to its data."), 
        type = "error", 
        duration = NULL
      )
      DEU(NULL)
    })
  })
  
  observe({
    temp_meta <- data.frame()
    tryCatch({
      data("metaIndex", package = "rdwd")
      
      if (!is.data.frame(metaIndex)) {
        stop("metaIndex is not a data frame after loading from rdwd package.")
      }
      
      # Re-introducing the filtering for daily KL (climate) stations with available files
      m <- metaIndex[metaIndex$res == "daily" & metaIndex$var == "kl" & metaIndex$hasfile, ] 
      temp_meta <- m
      
      print(paste("Initial metaIndex dimensions:", paste(dim(metaIndex), collapse = "x")))
      print(paste("Filtered meta dimensions:", paste(dim(temp_meta), collapse = "x")))
      
    }, error = function(e) {
      showNotification(
        paste("Error loading DWD meta data: ", e$message, 
              " Please ensure 'rdwd' package is installed correctly and has access to its data (e.g., check internet connection or package installation)."), 
        type = "error", 
        duration = NULL
      )
      print(paste("Error loading DWD meta data:", e$message))
      temp_meta <- data.frame()
    })
    meta(temp_meta)
  })
  
  observe({
    current_meta <- meta()
    
    if (nrow(current_meta) > 0) {
      if (!"Stations_id" %in% names(current_meta) || !"Stationsname" %in% names(current_meta)) {
        showNotification("Required columns 'Stations_id' or 'Stationsname' not found in DWD metadata. Please check rdwd package data structure.", type = "error")
        choices <- c("Error: Missing station info" = "")
      } else {
        print(paste("Column names in current_meta:", paste(names(current_meta), collapse = ", ")))
        print(paste("Head of current_meta$Stations_id:", paste(head(current_meta$Stations_id), collapse = ", ")))
        print(paste("Head of current_meta$Stationsname:", paste(head(current_meta$Stationsname), collapse = ", ")))
        
        ids_for_choices <- as.character(current_meta$Stations_id) 
        names_for_choices <- as.character(current_meta$Stationsname)
        
        ids_for_choices[is.na(ids_for_choices)] <- ""
        names_for_choices[is.na(names_for_choices)] <- "Unknown Station"
        
        choices <- setNames(ids_for_choices, paste0(names_for_choices, " (", ids_for_choices, ")"))
      }
    } else {
      choices <- c("No stations found" = "") 
      showNotification("No DWD stations found matching criteria. Please check rdwd package data or try again.", type = "warning")
    }
    updateSelectizeInput(session, "city", choices = choices, server = TRUE)
  })
  
  climate_data <- reactiveVal(NULL)
  station_info <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$city)
    showNotification("Downloading climate data. This may take a moment...", type = "message")
    tryCatch({
      station_id <- as.numeric(input$city)
      selected_station <- meta()[meta()$Stations_id == station_id, ] 
      station_info(selected_station)
      
      link <- tryCatch({
        rdwd::selectDWD(id = station_id, res = "daily", var = "kl", per = "historical")
      }, error = function(e) {
        rdwd::selectDWD(id = station_id, res = "daily", var = "kl", per = "recent")
      })
      
      file <- rdwd::dataDWD(link, read = FALSE, dir = tempdir())
      clidata <- rdwd::readDWD(file)
      
      clnms <- names(clidata)
      outcols <- list(
        MESS_DATUM = if ("MESS_DATUM" %in% clnms) "MESS_DATUM" else if ("date" %in% clnms) "date" else NA,
        RSK = if ("RSK" %in% clnms) "RSK" else if ("precip" %in% clnms) "precip" else NA,
        TMK = if ("TMK" %in% clnms) "TMK" else if ("tmean" %in% clnms) "tmean" else NA,
        TXK = if ("TXK" %in% clnms) "TXK" else if ("tmax" %in% clnms) "tmax" else NA,
        TNK = if ("TNK" %in% clnms) "TNK" else if ("tmin" %in% clnms) "tmin" else NA,
        FM = if ("FM" %in% clnms) "FM" else if ("wind" %in% clnms) "wind" else NA,
        SDK = if ("SDK" %in% clnms) "SDK" else if ("sun" %in% clnms) "sun" else NA
      )
      
      if (any(is.na(unlist(outcols)))) {
        stop("Climate data does not contain all required columns (MESS_DATUM/date, RSK/precip, TMK/tmean, TXK/tmax, TNK/tmin, FM/wind, SDK/sun).")
      }
      
      processed_data <- clidata %>%
        select(
          MESS_DATUM = all_of(outcols$MESS_DATUM),
          RSK = all_of(outcols$RSK),
          TMK = all_of(outcols$TMK),
          TXK = all_of(outcols$TXK),
          TNK = all_of(outcols$TNK),
          FM = all_of(outcols$FM),
          SDK = all_of(outcols$SDK)
        ) %>%
        mutate(
          MESS_DATUM = as.numeric(gsub("-", "", as.character(MESS_DATUM))),
          RSK = ifelse(is.na(RSK), 0, RSK)
        ) %>%
        arrange(MESS_DATUM)
      
      climate_data(processed_data)
      showNotification(paste("Climate data loaded for station:", selected_station$Stationsname), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  output$station_info <- renderPrint({
    info <- station_info()
    req(info, nrow(info) > 0)
    cat("Station Name: ", info$Stationsname, "\n")
    cat("Station ID: ", info$Stations_id, "\n")
    cat("Coordinates: ", info$geoLaenge, "°E, ", info$geoBreite, "°N\n")
    cat("Bundesland: ", info$Bundesland, "\n")
    cat("Data Period: ", as.character(info$von_datum), "to", as.character(info$bis_datum), "\n")
  })
  
  output$station_map <- renderPlot({
    req(DEU(), meta())
    
    current_meta <- meta()
    selected_station_info <- station_info()
    
    par(mar=rep(0,4), bg="grey96")
    terra::plot(DEU(), border=8)
    
    points(current_meta$geoLaenge, current_meta$geoBreite, 
           asp=1.6, pch=3, lwd=1, col="steelblue")
    
    if (!is.null(selected_station_info) && nrow(selected_station_info) > 0) {
      points(selected_station_info$geoLaenge, selected_station_info$geoBreite, 
             cex=3, lwd=2, col="salmon", pch=16)
      text(selected_station_info$geoLaenge, selected_station_info$geoBreite, 
           labels = selected_station_info$Stationsname, 
           pos = 3, col = "salmon", cex = 1.2)
    }
  })
  
  results <- eventReactive(input$calculate, {
    req(climate_data())
    data <- climate_data()
    
    data <- data %>%
      mutate(
        Date = as.Date(as.character(MESS_DATUM), format = "%Y%m%d"),
        Month = month(Date),
        irrigation_season = ifelse(Month >= input$irrigation_months[1] & Month <= input$irrigation_months[2], 1, 0),
        PET = 0.0023 * (TMK + 17.8) * sqrt(pmax(TXK - TNK, 0)) * SDK,
        substrate_storage = input$init_storage,
        irrigation_min = ifelse(irrigation_season == 1, input$irrigation_area * 2 / 1000, 0),
        irrigation_med = ifelse(irrigation_season == 1, input$irrigation_area * 4 / 1000, 0),
        irrigation_max = ifelse(irrigation_season == 1, input$irrigation_area * 6 / 1000, 0),
        rain_inflow = pmax(0, (RSK - input$initial_loss) * input$runoff_coef * input$runoff_area / 1000),
        greywater_inflow = input$greywater
      )
    
    storage_R_min <- input$init_tank_volume
    storage_R_med <- input$init_tank_volume
    storage_R_max <- input$init_tank_volume
    storage_GW_min <- input$init_tank_volume
    storage_GW_med <- input$init_tank_volume
    storage_GW_max <- input$init_tank_volume
    storage_RGW_min <- input$init_tank_volume
    storage_RGW_med <- input$init_tank_volume
    storage_RGW_max <- input$init_tank_volume
    
    n <- nrow(data)
    
    storage_R_min_vec <- numeric(n)
    storage_R_med_vec <- numeric(n)
    storage_R_max_vec <- numeric(n)
    storage_GW_min_vec <- numeric(n)
    storage_GW_med_vec <- numeric(n)
    storage_GW_max_vec <- numeric(n)
    storage_RGW_min_vec <- numeric(n)
    storage_RGW_med_vec <- numeric(n)
    storage_RGW_max_vec <- numeric(n)
    
    for (i in 1:n) {
      storage_R_min_vec[i] <- min(input$tank_volume, pmax(0, storage_R_min + data$rain_inflow[i] - data$irrigation_min[i]))
      storage_R_med_vec[i] <- min(input$tank_volume, pmax(0, storage_R_med + data$rain_inflow[i] - data$irrigation_med[i]))
      storage_R_max_vec[i] <- min(input$tank_volume, pmax(0, storage_R_max + data$rain_inflow[i] - data$irrigation_max[i]))
      
      storage_GW_min_vec[i] <- min(input$tank_volume, pmax(0, storage_GW_min + data$greywater_inflow[i] - data$irrigation_min[i]))
      storage_GW_med_vec[i] <- min(input$tank_volume, pmax(0, storage_GW_med + data$greywater_inflow[i] - data$irrigation_med[i]))
      storage_GW_max_vec[i] <- min(input$tank_volume, pmax(0, storage_GW_max + data$greywater_inflow[i] - data$irrigation_max[i]))
      
      storage_RGW_min_vec[i] <- min(input$tank_volume, pmax(0, storage_RGW_min + data$rain_inflow[i] + data$greywater_inflow[i] - data$irrigation_min[i]))
      storage_RGW_med_vec[i] <- min(input$tank_volume, pmax(0, storage_RGW_med + data$rain_inflow[i] + data$greywater_inflow[i] - data$irrigation_med[i]))
      storage_RGW_max_vec[i] <- min(input$tank_volume, pmax(0, storage_RGW_max + data$rain_inflow[i] + data$greywater_inflow[i] - data$irrigation_max[i]))
      
      storage_R_min <- storage_R_min_vec[i]
      storage_R_med <- storage_R_med_vec[i]
      storage_R_max <- storage_R_max_vec[i]
      storage_GW_min <- storage_GW_min_vec[i]
      storage_GW_med <- storage_GW_med_vec[i]
      storage_GW_max <- storage_GW_max_vec[i]
      storage_RGW_min <- storage_RGW_min_vec[i]
      storage_RGW_med <- storage_RGW_med_vec[i]
      storage_RGW_max <- storage_RGW_max_vec[i]
    }
    
    data <- data %>%
      mutate(
        storage_R_min = storage_R_min_vec,
        storage_R_med = storage_R_med_vec,
        storage_R_max = storage_R_max_vec,
        storage_GW_min = storage_GW_min_vec,
        storage_GW_med = storage_GW_med_vec,
        storage_GW_max = storage_GW_max_vec,
        storage_RGW_min = storage_RGW_min_vec,
        storage_RGW_med = storage_RGW_med_vec,
        storage_RGW_max = storage_RGW_max_vec,
        overflow_R_min = pmax(0, (storage_R_min_vec + data$rain_inflow - data$irrigation_min) - input$tank_volume),
        overflow_R_med = pmax(0, (storage_R_med_vec + data$rain_inflow - data$irrigation_med) - input$tank_volume),
        overflow_R_max = pmax(0, (storage_R_max_vec + data$rain_inflow - data$irrigation_max) - input$tank_volume),
        overflow_GW_min = pmax(0, (storage_GW_min_vec + data$greywater_inflow - data$irrigation_min) - input$tank_volume),
        overflow_GW_med = pmax(0, (storage_GW_med_vec + data$greywater_inflow - data$irrigation_med) - input$tank_volume),
        overflow_GW_max = pmax(0, (storage_GW_max_vec + data$greywater_inflow - data$irrigation_max) - input$tank_volume),
        overflow_RGW_min = pmax(0, (storage_RGW_min_vec + data$rain_inflow + data$greywater_inflow - data$irrigation_min) - input$tank_volume),
        overflow_RGW_med = pmax(0, (storage_RGW_med_vec + data$rain_inflow + data$greywater_inflow - data$irrigation_med) - input$tank_volume),
        overflow_RGW_max = pmax(0, (storage_RGW_max_vec + data$rain_inflow + data$greywater_inflow - data$irrigation_max) - input$tank_volume)
      )
    
    summary_table <- data.frame(
      Scenario = c("Rainwater - Min", "Rainwater - Med", "Rainwater - Max",
                   "Greywater - Min", "Greywater - Med", "Greywater - Max",
                   "Rain+Grey - Min", "Rain+Grey - Med", "Rain+Grey - Max"),
      Min_Storage = c(min(data$storage_R_min), min(data$storage_R_med), min(data$storage_R_max),
                      min(data$storage_GW_min), min(data$storage_GW_med), min(data$storage_GW_max),
                      min(data$storage_RGW_min), min(data$storage_RGW_med), min(data$storage_RGW_max)),
      Max_Storage = c(max(data$storage_R_min), max(data$storage_R_med), max(data$storage_R_max),
                      max(data$storage_GW_min), max(data$storage_GW_med), max(data$storage_GW_max),
                      max(data$storage_RGW_min), max(data$storage_RGW_med), max(data$storage_RGW_max)),
      Days_Empty = c(sum(data$storage_R_min <= 0), sum(data$storage_R_med <= 0), sum(data$storage_R_max <= 0),
                     sum(data$storage_GW_min <= 0), sum(data$storage_GW_med <= 0), sum(data$storage_GW_max <= 0),
                     sum(data$storage_RGW_min <= 0), sum(data$storage_RGW_med <= 0), sum(data$storage_RGW_max <= 0)),
      Total_Overflow = c(sum(data$overflow_R_min), sum(data$overflow_R_med), sum(data$overflow_R_max),
                         sum(data$overflow_GW_min), sum(data$overflow_GW_med), sum(data$overflow_GW_max),
                         sum(data$overflow_RGW_min), sum(data$overflow_RGW_med), sum(data$overflow_RGW_max))
    )
    
    list(
      data = data,
      summary = summary_table
    )
  })
  
  output$climate_data <- renderDT({
    req(climate_data())
    datatable(climate_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$water_balance_plot <- renderPlot({
    req(results())
    data <- results()$data
    
    plot_data <- data %>%
      select(Date, starts_with("storage_")) %>%
      pivot_longer(cols = -Date, names_to = "Scenario", values_to = "Storage") %>%
      mutate(Scenario = case_when(
        Scenario == "storage_R_min" ~ "Rainwater - Min",
        Scenario == "storage_R_med" ~ "Rainwater - Med",
        Scenario == "storage_R_max" ~ "Rainwater - Max",
        Scenario == "storage_GW_min" ~ "Greywater - Min",
        Scenario == "storage_GW_med" ~ "Greywater - Med",
        Scenario == "storage_GW_max" ~ "Greywater - Max",
        Scenario == "storage_RGW_min" ~ "Rain+Grey - Min",
        Scenario == "storage_RGW_med" ~ "Rain+Grey - Med",
        Scenario == "storage_RGW_max" ~ "Rain+Grey - Max",
        TRUE ~ Scenario
      ))
    
    ggplot(plot_data, aes(x = Date, y = Storage, color = Scenario)) +
      geom_line(alpha = 0.7) +
      geom_hline(yintercept = input$tank_volume, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Storage Tank Volume Over Time",
           y = "Storage Volume (m³)", x = "Date", color = "Scenario") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title = element_text(face = "bold")) +
      scale_color_brewer(palette = "Set1")
  })
  
  output$summary_table <- renderDT({
    req(results())
    datatable(results()$summary, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(columns = c("Min_Storage", "Max_Storage", "Total_Overflow"), digits = 2)
  })
  
  output$tank_performance <- renderDT({
    req(results())
    data_results <- results()$data
    
    total_rainwater_yield <- sum(data_results$rain_inflow, na.rm = TRUE)
    total_greywater_available <- sum(data_results$greywater_inflow, na.rm = TRUE)
    total_irrigation_min <- sum(data_results$irrigation_min, na.rm = TRUE)
    total_irrigation_med <- sum(data_results$irrigation_med, na.rm = TRUE)
    total_irrigation_max <- sum(data_results$irrigation_max, na.rm = TRUE)
    
    perf_data <- data.frame(
      Metric = c("Total Rainwater Collected (m³)", 
                 "Total Greywater Available (m³)",
                 "Total Irrigation Need - Min (m³)",
                 "Total Irrigation Need - Med (m³)",
                 "Total Irrigation Need - Max (m³)",
                 "Average Daily Irrigation Need - Min (m³/day)",
                 "Average Daily Irrigation Need - Med (m³/day)",
                 "Average Daily Irrigation Need - Max (m³/day)",
                 "Tank Utilization - Rainwater (%)",
                 "Tank Utilization - Greywater (%)",
                 "Tank Utilization - Combined (%)"),
      Value = c(
        total_rainwater_yield,
        total_greywater_available,
        total_irrigation_min,
        total_irrigation_med,
        total_irrigation_max,
        mean(data_results$irrigation_min, na.rm = TRUE),
        mean(data_results$irrigation_med, na.rm = TRUE),
        mean(data_results$irrigation_max, na.rm = TRUE),
        mean(data_results$storage_R_med, na.rm = TRUE) / input$tank_volume * 100,
        mean(data_results$storage_GW_med, na.rm = TRUE) / input$tank_volume * 100,
        mean(data_results$storage_RGW_med, na.rm = TRUE) / input$tank_volume * 100
      )
    )
    datatable(perf_data, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(columns = "Value", digits = 2)
  })
  
  output$tank_dimensions_output <- renderPrint({
    req(results())
    summary_data <- results()$summary
    
    cat("--- Suggested Tank Dimensions and Appropriateness Assessment ---\n\n")
    
    default_cuboid_height <- 2
    default_cyl_radius <- 1
    
    for (i in 1:nrow(summary_data)) {
      scenario_name <- summary_data$Scenario[i]
      min_storage_reached <- summary_data$Min_Storage[i]
      days_empty <- summary_data$Days_Empty[i]
      total_overflow <- summary_data$Total_Overflow[i]
      
      suggested_tank_volume <- input$tank_volume 
      
      cat(sprintf("Scenario: %s\n", scenario_name))
      cat(sprintf("  Minimum Storage Reached: %.2f m³\n", min_storage_reached))
      cat(sprintf("  Days Tank was Empty: %d\n", days_empty))
      cat(sprintf("  Total Overflow: %.2f m³\n", total_overflow))
      
      if (days_empty > 0) {
        suggested_tank_volume <- input$tank_volume + abs(min_storage_reached)
        cat(sprintf("  Water Security: \u274C COMPROMISED. Tank ran dry for %d days, with a maximum deficit of %.2f m³.\n", days_empty, abs(min_storage_reached)))
        cat(sprintf("  Suggested minimum volume to avoid this deficit: %.2f m³\n", suggested_tank_volume))
        cat("  Overall Appropriateness: **Insufficient.**\n")
        cat("  Recommendations:\n")
        cat("    - **Increase Tank Volume:** Consider a tank of at least %.2f m³.\n", suggested_tank_volume)
        cat("    - **Reduce Irrigation Demand:** Adjust irrigation area or daily rates.\n")
        cat("    - **Explore Additional Sources:** Consider more rainwater collection area or greywater supply.\n")
      } else {
        cat(sprintf("  Water Security: \u2705 MAINTAINED. Tank never ran dry.\n"))
        
        if (total_overflow > 0.05 * input$tank_volume) { 
          cat(sprintf("  Overflow: \u26A0 PRESENT (%.2f m³). This indicates potential over-sizing or excess supply.\n", total_overflow))
          cat("  Overall Appropriateness: **Potentially Oversized.**\n")
          cat("  Recommendations:\n")
          cat("    - **Optimize Tank Size:** A slightly smaller tank (closer to your actual demand) might be more efficient.\n")
          cat("    - **Increase Demand/Utilization:** Explore using more collected water (e.g., larger irrigation area, other uses).\n")
          cat("    - **Consider Alternative Uses:** For excess water, such as groundwater recharge or other non-potable uses.\n")
        } else {
          cat("  Overflow: \u2705 NONE (or negligible).\n")
          cat("  Overall Appropriateness: **Well-suited.** The current tank volume appears well-matched to the supply and demand for this scenario.\n")
        }
      }
      
      suggested_tank_volume <- max(0.1, suggested_tank_volume)
      
      cat("\n  --- Cuboid Tank (Square Base) ---\n")
      cuboid_height <- default_cuboid_height 
      cuboid_base_area <- suggested_tank_volume / cuboid_height
      cuboid_side_length <- sqrt(cuboid_base_area)
      
      cat(sprintf("    Assuming a height of %.2f m:\n", cuboid_height))
      cat(sprintf("      Side Length: %.2f m\n", cuboid_side_length))
      cat(sprintf("      Height: %.2f m\n", cuboid_height))
      cat(sprintf("      (Dimensions: %.2f m x %.2f m x %.2f m)\n\n", 
                  cuboid_side_length, cuboid_side_length, cuboid_height))
      
      cat("  --- Cylindrical Tank ---\n")
      cyl_radius <- default_cyl_radius
      cyl_base_area <- pi * (cyl_radius^2)
      cyl_height <- suggested_tank_volume / cyl_base_area
      
      cat(sprintf("    Assuming a radius of %.2f m:\n", cyl_radius))
      cat(sprintf("      Radius: %.2f m\n", cyl_radius))
      cat(sprintf("      Height: %.2f m\n", cyl_height))
      cat(sprintf("      (Dimensions: Radius %.2f m, Height %.2f m)\n\n", 
                  cyl_radius, cyl_height))
      cat("---\n\n")
    }
  })
}

shinyApp(ui = ui, server = server)
