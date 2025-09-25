# ──────────────────────────────────────────────────────────────────────────────
# ENVIRONMENT ------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

## -- Load Packages ------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(arrow)
library(scales)
library(dplyr)
library(mapgl)
library(sf)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(DT)

## -- Run Global Script --------------------------------------------------------
source("global.R")

# ──────────────────────────────────────────────────────────────────────────────
# UI ---------------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  # Use custom CSS
  tags$style(HTML("
  /* === Data Table CSV Button Customization === */
  div.dt-buttons { float: right; margin-top: 10px; }
  .btn-secondary.buttons-csv {
    background: #fff !important;
    color: #337ab7 !important;
    border: 1px solid #337ab7 !important;
    font-weight: bold;
    border-radius: 4px;
    padding: 6px 16px 6px 12px;
    display: flex;
    align-items: center;
    transition: background 0.3s, color 0.3s;
    gap: 0.5em;
  }
  .btn-secondary.buttons-csv svg {
    fill: #337ab7 !important;
    vertical-align: text-bottom;
    margin-right: 6px;
  }
  .btn-secondary.buttons-csv:hover {
    background: #337ab7 !important;
    color: #fff !important;
  }
  .btn-secondary.buttons-csv:hover svg { fill: #fff !important; }
  ")),
  # Use custom theme
  theme = bs_theme(
    primary = "#337ab7",
    navbar_bg = "#337ab7",
    navbar_color = "#fff",
    navbar_brand_color = "#fff"
  ),
  title = tagList(
    tags$strong("U.S. Poverty Mapping Project"),
    tags$span(style = "float: right; margin-left: 1rem;",
              tags$a(
                href = "https://www.linkedin.com/in/adamferalio/",
                target = "_blank",
                bsicons::bs_icon("linkedin", style = "color: #fff; fill: #fff;")
              ),
              tags$a(
                href = "https://github.com/feralad/us-poverty-shiny-app-draft",
                target = "_blank",
                style = "margin-left: 0.5rem;",
                bsicons::bs_icon("github", style = "color: #fff; fill: #fff;")
              )
    )
  ),
  sidebar = sidebar(
    width = 315,
    accordion(
      id = "filters_accordion",
      open = c("map_layer_filter", "demographic_filters"),
      multiple = TRUE,
      accordion_panel(
        title = "Map Layer",
        id = "map_layer_filter",
        icon = bsicons::bs_icon("map"),
        radioButtons(
          inputId = "map_layer_selection",
          label = "Selected Layer",
          choices = c(
            "Poverty Rates" = "poverty_rates",
            "Population in Poverty (3D)" = "poverty_extrusion"
          ),
          selected = "poverty_rates"
        )
      ),
      accordion_panel(
        title = "Demographic",
        id = "demographic_filters",
        icon = bsicons::bs_icon("person"),
        selectInput(
          inputId = "race_eth_dropdown",
          label = "Race/Ethnicity",
          choices = race_eth_choices,
          selected = "All",
          multiple = FALSE
        )
      ),
      accordion_panel(
        title = "Geography",
        id = "geography_filters",
        icon = bsicons::bs_icon("compass"),
        selectizeInput(
          inputId = "state_dropdown",
          label = "State/Territory",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = "county_dropdown",
          label = "County",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      ),
      accordion_panel(
        title = "Poverty Data",
        id = "data_filters",
        icon = bsicons::bs_icon("database"),
        selectizeInput(
          inputId = "n_poverty_total_dropdown",
          label = "Population in Poverty (US)",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = "s_poverty_total_dropdown",
          label = "Population in Poverty (State)",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = "poverty_rate_dropdown",
          label = "Poverty Rate",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = "poverty_reliability_dropdown",
          label = "Reliability",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = "poverty_moe_dropdown",
          label = "Margin of Error",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        )
      )
    ),
    input_task_button(
      id = "submit_button",
      label = "Submit",
      label_busy = "Processing..."
    ),
    input_task_button(
      id = "reset_button",  # Add a Reset Button
      label = "Reset to Default",
      label_busy = "Resetting..."
    )
  ),
  navset_card_tab(
    id = "master_tab",
    nav_panel(
      title = "Map",
      value = "map_tab",
      card_body(
        style = "display: flex; flex-direction: column; height: 100vh;",  # Full screen height
        div(
          style = "flex: 1 1 auto; min-height: 200px;",                   # Map area grows as needed
          maplibreOutput("map", height = "100%")
        ),
        div(
          style = "min-height: 32px; color: #666; font-size: 12px; font-style: italic; margin-top: 0;",
          htmlOutput("map_footnote")
        )
      )
    ),
    nav_panel(
      title = "Plot",
      value = "plot_tab",
      card_body(
        style = "display: flex; flex-direction: column; height: 100vh;",
        div(
          style = "flex: 1 1 auto; min-height: 200px;",
          plotlyOutput("scatterplot", height = "100%")
        ),
        div(
          style = "min-height: 32px; color: #666; font-size: 12px; font-style: italic; margin-top: 0; margin-left: 5%;",
          htmlOutput("plot_footnote")
        )
      )
    ),
    nav_panel(
      title = "Data",
      value = "data_tab",
      navset_card_tab(
        nav_panel(
          "Poverty",
          dataTableOutput("poverty_data_table")
        )
      )
    )
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# SERVER -----------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────  
server <- function(input, output, session) {
  
  ## -- Dynamize Data ----------------------------------------------------------
  
  ### -- Filtered Data ---------------------------------------------------------
  
  #### -- Filtered Poverty Data ------------------------------------------------
  filtered_poverty_data <- reactive({
    get_county_poverty_data(
      race_eth_f = input$race_eth_dropdown,
      state_n = input$state_dropdown,
      county_f = input$county_dropdown,
      gnsmpbpE = input$n_poverty_total_dropdown,
      gssmpbpE = input$s_poverty_total_dropdown,
      gppbpE = input$poverty_rate_dropdown,
      rppbpE = input$poverty_reliability_dropdown,
      gppbpM = input$poverty_moe_dropdown
    )
  })
  
  ### -- Dropdown Data ---------------------------------------------------------
  
  #### -- Dropdown Poverty Data ------------------------------------------------
  
  # Dynamically update all filters
  observe({
    dropdown_poverty_data <- isolate(filtered_poverty_data()) # Use isolate to avoid retriggering reactive dependencies
    
    # --- Race/Ethnicity
    updateSelectInput(
      session,
      inputId = "race_eth_dropdown",
      choices = race_eth_choices,
      selected = input$race_eth_dropdown
    )
    
    # --- State
    # Get unique, sorted state choices from the filtered data
    state_choices <- sort(unique(dropdown_poverty_data$state_n))
    # Combine current selections with available choices to preserve user choices
    combined_state_choices <- unique(c(input$state_dropdown, state_choices))
    # Store the user's current selection
    current_state_selection <- input$state_dropdown
    # Keep only selections that are still valid with the new choices
    preserved_state_selection <- current_state_selection[current_state_selection %in% combined_state_choices]
    # Only update the dropdown if the available choices have changed (prevents flicker)
    if (!identical(sort(combined_state_choices), sort(input$state_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "state_dropdown",
        choices = combined_state_choices,          # Update with new and preserved choices
        selected = preserved_state_selection,      # Restore still-valid selections
        server = TRUE                              # Use server-side processing for efficiency
      )
    }
    
    # --- County
    county_choices <- sort(unique(dropdown_poverty_data$county_f))
    combined_choices <- unique(c(input$county_dropdown, county_choices))
    current_selection <- input$county_dropdown
    preserved_selection <- current_selection[current_selection %in% combined_choices]
    if (!identical(sort(combined_choices), sort(input$county_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "county_dropdown",
        choices = combined_choices,
        selected = preserved_selection,
        server = TRUE
      )
    }
    
    # --- Population in Poverty (US)
    n_poverty_total_choices <- unique(dropdown_poverty_data$gnsmpbpE[order(dropdown_poverty_data$sognsmpbpE)])
    combined_n_poverty_total_choices <- unique(c(input$n_poverty_total_dropdown, n_poverty_total_choices))
    current_n_poverty_total_selection <- input$n_poverty_total_dropdown
    preserved_n_poverty_total_selection <- current_n_poverty_total_selection[current_n_poverty_total_selection %in% combined_n_poverty_total_choices]
    if (!identical(sort(combined_n_poverty_total_choices), sort(input$n_poverty_total_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "n_poverty_total_dropdown",
        choices = combined_n_poverty_total_choices, # Update with new and preserved choices
        selected = preserved_n_poverty_total_selection, # Restore still-valid selections
        server = TRUE
      )
    }
    
    # --- Population in Poverty (State)
    s_poverty_total_choices <- unique(dropdown_poverty_data$gssmpbpE[order(dropdown_poverty_data$sogssmpbpE)])
    combined_s_poverty_total_choices <- unique(c(input$s_poverty_total_dropdown, s_poverty_total_choices))
    current_s_poverty_total_selection <- input$s_poverty_total_dropdown
    preserved_s_poverty_total_selection <- current_s_poverty_total_selection[current_s_poverty_total_selection %in% combined_s_poverty_total_choices]
    if (!identical(sort(combined_s_poverty_total_choices), sort(input$s_poverty_total_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "s_poverty_total_dropdown",
        choices = combined_s_poverty_total_choices,
        selected = preserved_s_poverty_total_selection,
        server = TRUE
      )
    }
    
    # --- Poverty Rate
    poverty_rate_choices <- unique(dropdown_poverty_data$gppbpE[order(dropdown_poverty_data$sogppbpE)])
    combined_poverty_rate_choices <- unique(c(input$poverty_rate_dropdown, poverty_rate_choices))
    current_poverty_rate_selection <- input$poverty_rate_dropdown
    preserved_poverty_rate_selection <- current_poverty_rate_selection[current_poverty_rate_selection %in% combined_poverty_rate_choices]
    if (!identical(sort(combined_poverty_rate_choices), sort(input$poverty_rate_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "poverty_rate_dropdown",
        choices = combined_poverty_rate_choices,
        selected = preserved_poverty_rate_selection,
        server = TRUE
      )
    }
    
    # --- Reliability
    poverty_reliability_choices <- unique(dropdown_poverty_data$rppbpE[order(dropdown_poverty_data$sorppbpE)])
    combined_poverty_reliability_choices <- unique(c(input$poverty_reliability_dropdown, poverty_reliability_choices))
    current_poverty_reliability_selection <- input$poverty_reliability_dropdown
    preserved_poverty_reliability_selection <- current_poverty_reliability_selection[current_poverty_reliability_selection %in% combined_poverty_reliability_choices]
    if (!identical(sort(combined_poverty_reliability_choices), sort(input$poverty_reliability_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "poverty_reliability_dropdown",
        choices = combined_poverty_reliability_choices,
        selected = preserved_poverty_reliability_selection,
        server = TRUE
      )
    }
    
    # --- Margin of Error
    poverty_moe_choices <- unique(dropdown_poverty_data$gppbpM[order(dropdown_poverty_data$sogppbpM)])
    combined_poverty_moe_choices <- unique(c(input$poverty_moe_dropdown, poverty_moe_choices))
    current_poverty_moe_selection <- input$poverty_moe_dropdown
    preserved_poverty_moe_selection <- current_poverty_moe_selection[current_poverty_moe_selection %in% combined_poverty_moe_choices]
    if (!identical(sort(combined_poverty_moe_choices), sort(input$poverty_moe_dropdown))) {
      updateSelectizeInput(
        session,
        inputId = "poverty_moe_dropdown",
        choices = combined_poverty_moe_choices,
        selected = preserved_poverty_moe_selection,
        server = TRUE
      )
    }
  })
  
  ### -- Submitted Data --------------------------------------------------------
  
  #### -- Submitted Poverty Data -----------------------------------------------
  
  # Create empty submitted data frame when app initially loads
  submitted_poverty_data <- reactiveVal(NULL)
  
  # Whenever the user clicks submit, update it
  observeEvent(input$submit_button, {
    submitted_poverty_data(filtered_poverty_data())
  })
  
  ## -- Map --------------------------------------------------------------------
  
  # Directly define the US bounds as the initial bounding box
  ## Maplibre appears to take bbox in this structure
  us_bounds <- matrix(
    c(-177, 12,   # xmin, ymin (SW)
      -60, 72),  # xmax, ymax (NE)
    ncol = 2, byrow = TRUE
  )
  
  # Store the last bounding box for use with tab-activation observer
  last_bbox <- reactiveVal(us_bounds)
  
  # Define initial map data
  initial_map_data <- get_county_poverty_data(race_eth_f = "All")
  
  # Create the map with the initial data and fit to the US bounds
  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron")) %>%
      add_fill_layer(
        id = "poverty_rates",
        source = initial_map_data, 
        fill_color = step_expr(
          column = "sogppbpE",
          # Use plasma colors from viridis package
          base  = c("#0D0887FF"),
          stops = c("#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF", "#8a8a8a"),
          values = c(2, 3, 4, 5, 6)
        ),
        fill_opacity = .7,
        tooltip = "ptt",
        hover_options = list(
          fill_color = "#00FFFF",
          fill_opacity = 1
        )
      ) %>% 
      add_categorical_legend(
        legend_title = "Poverty Rate",
        values = c("Extreme", "Very High", "High", "Moderate", "Low", "Zero Population"),
        # Use B3 instead of FF at end of hex codes to match map opacity of .7 (255 full opacity x .7 = 178.5 ~ 179)
        colors = c("#0D0887B3", "#7E03A8B3", "#CC4678B3", "#F89441B3", "#F0F921B3", "#8a8a8aB3"),
        circular_patches = FALSE,
        position = "bottom-left",
        add = FALSE,
        unique_id = "poverty_rates_legend",
        layer_id = "poverty_rates"
      ) %>%
      fit_bounds(
        bbox = us_bounds  # Pass bbox directly
      )
  })
  
  # Dynamically update map based on submitted data and fit to selected geography bounds
  observeEvent(input$submit_button, {
    # Get the submitted poverty data
    dynamic_map_data <- submitted_poverty_data()
    
    # Duplicate the data for bounding box calculations and preserve original
    bbox_geom_data <- dynamic_map_data
    
    # --- Generalized special-case filtering for Alaska/Hawaii ---
    to_drop <- integer(0)
    
    for (i in seq_len(nrow(bbox_geom_data))) {
      county <- bbox_geom_data$county_f[i]
      state <- bbox_geom_data$state_n[i]
      geom <- st_geometry(bbox_geom_data)[[i]]
      
      # --- Hawaii: Special handling for Honolulu County ---
      if (state == "Hawaii") {
        if (trimws(county) == "Honolulu County, Hawaii") {
          polys <- st_cast(st_sfc(geom), "POLYGON")
          keep_idx <- sapply(polys, function(poly) min(st_coordinates(poly)[, 1], na.rm = TRUE) > -161)
          keep_polys <- polys[keep_idx]
          if (length(keep_polys) == 0) {
            to_drop <- c(to_drop, i)
          } else {
            new_multi <- st_sfc(st_multipolygon(lapply(keep_polys, unclass)), crs = st_crs(bbox_geom_data))
            st_geometry(bbox_geom_data)[i] <- new_multi
          }
        } else {
          coords <- st_coordinates(geom)
          if (min(coords[, 1], na.rm = TRUE) <= -162) {
            to_drop <- c(to_drop, i)
          }
        }
      }
      
      # --- Alaska: Special handling for Aleutians West Census Area ---
      else if (state == "Alaska" && trimws(county) == "Aleutians West Census Area, Alaska") {
        polys <- st_cast(st_sfc(geom), "POLYGON")
        mean_lons <- sapply(polys, function(poly) mean(st_coordinates(poly)[, 1], na.rm = TRUE))
        n_neg <- sum(mean_lons < 0)
        n_pos <- sum(mean_lons > 0)
        if (n_neg >= n_pos) {
          keep_idx <- which(mean_lons < 0)
        } else {
          keep_idx <- which(mean_lons > 0)
        }
        keep_polys <- polys[keep_idx]
        if (length(keep_polys) == 0) {
          to_drop <- c(to_drop, i)
        } else {
          new_multi <- st_sfc(st_multipolygon(lapply(keep_polys, unclass)), crs = st_crs(bbox_geom_data))
          st_geometry(bbox_geom_data)[i] <- new_multi
        }
      }
      
      # --- Alaska: Drop far western counties below lon threshold (except Aleutians West, already handled above) ---
      else if (state == "Alaska") {
        coords <- st_coordinates(geom)
        if (min(coords[, 1], na.rm = TRUE) < -170) {
          to_drop <- c(to_drop, i)
        }
      }
    }
    
    if (length(to_drop) > 0) {
      bbox_geom_data <- bbox_geom_data[-unique(to_drop), ]
    }
    
    # --- Bounding box calculation ---
    
    # us_bounds: matrix [xmin ymin; xmax ymax]
    bbox_numeric_from_matrix <- function(us_bounds_matrix) {
      c(
        xmin = us_bounds_matrix[1, 1],
        ymin = us_bounds_matrix[1, 2],
        xmax = us_bounds_matrix[2, 1],
        ymax = us_bounds_matrix[2, 2]
      )
    }
    
    # Detect what percentage of all possible geographies (GEOIDs) for selected race/ethnicity are in the submitted data
    
    # Define selected race/ethnicity
    race_val <- input$race_eth_dropdown
    
    # Get all possible GEOIDs for the selected race/ethnicity
    all_geos <- county_poverty_data %>%
      filter(race_eth_f == race_val) %>%
      select(GEOID) %>%
      distinct() %>%
      collect() %>%
      pull(GEOID)
    
    # Get GEOIDs that are selected in the submitted data
    selected_geos <- unique(bbox_geom_data$GEOID)
    
    # Fraction-based threshold for using us_bounds
    frac_selected <- length(selected_geos) / length(all_geos)
    us_bounds_threshold <- 0.98  # 98% (adjust as needed)
    
    # us_bounds as numeric vector for easy reference during clipping
    us_bounds_numeric <- bbox_numeric_from_matrix(us_bounds)
    
    if (length(selected_geos) == 0 || frac_selected >= us_bounds_threshold) {
      bbox_numeric <- us_bounds_numeric
    } else {
      combined_geometry <- st_combine(st_geometry(bbox_geom_data))
      bbox_raw <- st_bbox(combined_geometry)
      bbox <- as.numeric(bbox_raw)
      names(bbox) <- names(bbox_raw)
      buffer_percentage <- 0.1
      bbox_width <- bbox["xmax"] - bbox["xmin"]
      bbox_height <- bbox["ymax"] - bbox["ymin"]
      buffer_x <- bbox_width * buffer_percentage
      buffer_y <- bbox_height * buffer_percentage
      bbox_expanded <- bbox
      bbox_expanded["xmin"] <- bbox["xmin"] - buffer_x
      bbox_expanded["ymin"] <- bbox["ymin"] - buffer_y
      bbox_expanded["xmax"] <- bbox["xmax"] + buffer_x
      bbox_expanded["ymax"] <- bbox["ymax"] + buffer_y
      
      # --- CLIP bbox_expanded to us_bounds ---
      bbox_clipped <- bbox_expanded
      bbox_clipped["xmin"] <- max(bbox_expanded["xmin"], us_bounds_numeric["xmin"])
      bbox_clipped["ymin"] <- max(bbox_expanded["ymin"], us_bounds_numeric["ymin"])
      bbox_clipped["xmax"] <- min(bbox_expanded["xmax"], us_bounds_numeric["xmax"])
      bbox_clipped["ymax"] <- min(bbox_expanded["ymax"], us_bounds_numeric["ymax"])
      
      bbox_numeric <- bbox_clipped
    }
    
    # Format for maplibre: 2x2 matrix
    bbox_matrix <- matrix(
      c(bbox_numeric["xmin"], bbox_numeric["ymin"],
        bbox_numeric["xmax"], bbox_numeric["ymax"]),
      nrow = 2, byrow = TRUE
    )
    
    last_bbox(bbox_matrix)
    
    # Clear layers and update map
    maplibre_proxy("map") %>%
      clear_layer("poverty_rates") %>%
      clear_layer("org_points") %>%
      clear_layer("poverty_extrusion")
    
    # Add layers based on selection
    selected_layer <- input$map_layer_selection
    
    if (selected_layer == "poverty_rates") {
      maplibre_proxy("map") %>%
        add_fill_layer(
          id = "poverty_rates",
          source = dynamic_map_data,
          fill_color = step_expr(
            column = "sogppbpE",
            base  = c("#0D0887FF"),
            stops = c("#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF", "#8a8a8a"),
            values = c(2, 3, 4, 5, 6)
          ),
          fill_opacity = .7,
          tooltip = "ptt",
          hover_options = list(
            fill_color = "#00FFFF",
            fill_opacity = 1
          )
        ) %>%
        add_categorical_legend(
          legend_title = "Poverty Rate",
          values = c("Extreme", "Very High", "High", "Moderate", "Low", "Zero Population"),
          # Use B3 instead of FF at end of hex codes to match map opacity of .7 (255 full opacity x .7 = 178.5 ~ 179)
          colors = c("#0D0887B3", "#7E03A8B3", "#CC4678B3", "#F89441B3", "#F0F921B3", "#8a8a8aB3"),
          circular_patches = FALSE,
          position = "bottom-left",
          add = FALSE,
          unique_id = "poverty_rates_legend",
          layer_id = "poverty_rates"
        ) %>%
        fit_bounds(bbox = bbox_matrix)
      
    } else if (selected_layer == "poverty_extrusion") {
      maplibre_proxy("map") %>%
        add_fill_extrusion_layer(
          id = "poverty_extrusion",
          source = dynamic_map_data,
          fill_extrusion_color = step_expr(
            column = "sogppbpE",
            base  = c("#0D0887FF"),
            stops = c("#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF", "#8a8a8a"),
            values = c(2, 3, 4, 5, 6)
          ),
          fill_extrusion_opacity = .7,
          fill_extrusion_height = get_column("pbpE"),
          tooltip = "ptt",
          # Hover options doesn't work with extrusion layer (leaving for future enhancements)
          hover_options = list(
            fill_color = "#00FFFF",
            fill_opacity = 1
          )
        ) %>%
        add_categorical_legend(
          legend_title = "Poverty Rate",
          values = c("Extreme", "Very High", "High", "Moderate", "Low", "Zero Population"),
          # Use B3 instead of FF at end of hex codes to match map opacity of .7 (255 full opacity x .7 = 178.5 ~ 179)
          colors = c("#0D0887B3", "#7E03A8B3", "#CC4678B3", "#F89441B3", "#F0F921B3", "#8a8a8aB3"),
          circular_patches = FALSE,
          position = "bottom-left",
          add = FALSE,
          unique_id = "poverty_rates_legend",
          layer_id = "poverty_rates"
        ) %>%
        fit_bounds(bbox = bbox_matrix)
    }
  })
  
  # Fit map to last bounding box when map tab is reactivated
  observeEvent(input$master_tab, {
    if (input$master_tab == "map_tab") {
      maplibre_proxy("map") %>%
        fit_bounds(bbox = last_bbox())
    }
  })
  
  # Map Footnote
  
  output$map_footnote <- renderUI({
    map_footnote_text <- paste0(
      "<span style='font-size:13px; color:#666; font-style:italic;'>",
      "*Poverty data are reported as estimates, not exact figures. Use the Reliability and Margin of Error filters to select reliable estimates.<br>",
      "*Poverty rates are based on the percentage of population in poverty: Extreme: >= 40%, Very High: >= 30%, High: >= 20%, Moderate: >= 10%, Low: < 10%.<br>",
      "*Data source: 2019-2023 American Community Survey 5-Year Estimates via tidycensus.<br>",
      "*To change the pitch and/or angle of the map, hold Ctrl/control on keyboard and use mouse to drag map up/down (pitch) or left/right (angle).<br>",
      "</span>"
    )
    HTML(map_footnote_text)
  })
  
  ## -- Plot -------------------------------------------------------------------
  
  # Helper function for displaying a blank plotly plot with a message
  empty_plotly_msg <- function(msg) {
    plotly::plot_ly() %>%
      plotly::layout(
        xaxis = list(showticklabels = FALSE, zeroline = FALSE),
        yaxis = list(showticklabels = FALSE, zeroline = FALSE),
        annotations = list(
          list(
            x = 0.5, y = 0.5, text = paste0("<b>", msg, "</b>"),
            showarrow = FALSE, font = list(size = 18), xref = "paper", yref = "paper"
          )
        )
      )
  }
  
  # Create plot data
  plot_data <- reactive({
    submitted_df <- submitted_poverty_data()
    if (is.null(submitted_df) || nrow(submitted_df) == 0) return(NULL)
    # Only proceed if exactly one unique county is selected
    if (length(unique(submitted_df$county_f)) != 1) return(NULL)
    
    # Get data for all races/ethnicities for that county
    df <- county_poverty_data %>%
      filter(county_f == unique(submitted_df$county_f)) %>%
      collect()
    
    # Filtering logic for race/eth (using race_eth_s here):
    # Always filter out "all".
    # Filter out "white_alone" except if selected, in which case filter out "wanhl".
    selected_race <- submitted_df$race_eth_s[1]
    if (selected_race == "white_alone") {
      # Show only white_alone and filter out wanhl and all
      df <- df %>%
        filter(!race_eth_s %in% c("all", "wanhl"))
    } else {
      # Show all except "all" and "white_alone"
      df <- df %>%
        filter(!race_eth_s %in% c("all", "white_alone"))
    }
    
    # Only keep rows with positive population
    df <- df %>%
      filter(pE > 0)
    
    # Select only columns needed for plotting
    df %>%
      select(
        GEOID,
        county_f,
        race_eth_f,
        pbpE,
        pbpE_fmt,
        pbpM,
        pbpM_fmt,
        lpbpE,
        lpbpE_fmt,
        upbpE,
        upbpE_fmt,
        ppbpE,
        ppbpE_fmt,
        ppbpM,
        ppbpM_fmt,
        lppbpE,
        lppbpE_fmt,
        uppbpE,
        uppbpE_fmt
      )
  })
  
  # Render the plot
  
  output$scatterplot <- renderPlotly({
    # Handle empty plot when app initially loads with no submitted data with empty message
    sub_df <- submitted_poverty_data()
    if (is.null(sub_df) || nrow(sub_df) == 0) {
      return(empty_plotly_msg("Please select a single county and click Submit to display the plot."))
    }
    # Handle properly with submitted data
    plot_df <- plot_data()
    if (is.null(plot_df) || nrow(plot_df) == 0) {
      return(empty_plotly_msg("Please select a single county and click Submit to display the plot."))
    }
    
    # Define the bins and colors
    plot_bins <- data.frame(
      category = c("Low", "Moderate", "High", "Very High", "Extreme"),
      xmin = c(0.00, 0.10, 0.20, 0.30, 0.40),  # Start of each bin
      xmax = c(0.10, 0.20, 0.30, 0.40, 1.00),  # End of each bin
      color = c("#F0F921FF", "#F89441FF", "#CC4678FF", "#7E03A8FF", "#0D0887FF"),  # Hex color codes for each bin
      label_position = c(0.05, 0.15, 0.25, 0.35, 0.70)  # Midpoints for labels
    )
    
    # Define colors for Race/Ethnicity
    plot_colors <- c(
      "White Alone, Not Hispanic or Latino" = "#1B9E77FF",
      "Black or African American Alone" = "#D95F02FF",
      "Asian Alone" = "#7570B3FF",
      "Hispanic or Latino" = "#E7298AFF",
      "American Indian and Alaska Native Alone" = "#66A61EFF",
      "Native Hawaiian and Other Pacific Islander Alone" = "#E6AB02FF",
      "Some Other Race Alone" = "#A6761DFF",
      "Two or More Races" = "#666666FF",
      "White Alone" = "#bcbcbc"
    )
    
    # Dynamically calculate needed y values to set appropriate limits
    plot_max_y <- max(plot_df$upbpE, na.rm = TRUE) * 1.1 # Add 10% padding
    plot_bin_bar_ymin <- -plot_max_y * 0.18  # Position of the bottom of the bin bar
    plot_bin_bar_ymax <- -plot_max_y * 0.09  # Position of the top of the bin bar
    plot_bin_label_y <- plot_bin_bar_ymin - (plot_max_y * 0.06)  # Position of the labels below the bin bar
    plot_lower_limit <- plot_bin_label_y - (plot_max_y * 0.01)  # Lower limit for y-axis
    
    # Create county name object from data
    plot_county_name <- unique(plot_df$county_f)
    
    # Create hover labels for the plot
    plot_df <- plot_df %>%
      mutate(
        # For the main estimate point (center estimate)
        hover_label_main = paste0(
          "", race_eth_f, "<br>",
          "Population in Poverty Estimate: ", pbpE_fmt, "<br>",
          "Population in Poverty Estimate (%): ", ppbpE_fmt
        ),
        # For the lower bound endpoint
        hover_label_lower = paste0(
          "", race_eth_f, "<br>",
          "Lower Confidence Bound: ", lpbpE_fmt, "<br>",
          "Lower Confidence Bound (%): ", lppbpE_fmt
        ),
        # For the upper bound endpoint
        hover_label_upper = paste0(
          "", race_eth_f, "<br>",
          "Upper Confidence Bound: ", upbpE_fmt, "<br>",
          "Upper Confidence Bound (%): ", uppbpE_fmt
        )
      )
    
    # Create the scatterplot
    p <- ggplot() +
      # Add scatterplot points
      geom_point(
        data = plot_df,
        aes(
          x = ppbpE,
          y = pbpE,
          color = race_eth_f,
          text = hover_label_main
        ), size = 3.5
      ) +
      # Add diagonal error bar (lower bound to estimate)
      geom_segment(
        data = plot_df,
        aes(
          x = lppbpE,
          y = lpbpE,
          xend = ppbpE,
          yend = pbpE,
          color = race_eth_f
        ), linewidth = 0.8
      ) +
      # Add diagonal error bar (estimate to upper bound)
      geom_segment(
        data = plot_df,
        aes(
          x = ppbpE,
          y = pbpE,
          xend = uppbpE,
          yend = upbpE,
          color = race_eth_f
        ), linewidth = 0.8
      ) +
      # Left endpoint
      geom_point(
        data = plot_df,
        aes(
          x = lppbpE,
          y = lpbpE,
          color = race_eth_f,
          text = hover_label_lower
        ), size = 1.5
      ) +
      # Right endpoint
      geom_point(
        data = plot_df,
        aes(
          x = uppbpE,
          y = upbpE,
          color = race_eth_f,
          text = hover_label_upper
        ), size = 1.5
      ) +
      # Custom color scale
      scale_color_manual(
        name = "Race/Ethnicity",
        values = plot_colors) +
      # Add the bin bar at the bottom using geom_rect
      geom_rect(
        data = plot_bins,
        aes(xmin = xmin, xmax = xmax, 
            ymin = plot_bin_bar_ymin, ymax = plot_bin_bar_ymax, 
            fill = color
            ),
        inherit.aes = FALSE, show.legend = FALSE
      ) +
      guides(fill = "none") + # Ensure legend is not visible
      # Add labels for the bins further below the bar
      geom_text(
        data = plot_bins,
        aes(x = label_position, y = plot_bin_label_y, label = category),
        inherit.aes = FALSE, color = "#404040", size = 12 / .pt, family = "Arial Narrow"
      ) +
      # Define the x-axis scale
      scale_x_continuous(
        breaks = seq(0, 1, 0.1),  # Specify tick positions
        labels = scales::percent_format(accuracy = 1),  # Format labels as percentages
        expand = c(0, 0)  # Match data range exactly
      ) +
      # Define the y-axis scale dynamically to include the largest y value and the bin bar
      scale_y_continuous(
        limits = c(plot_lower_limit, plot_max_y),
        breaks = function(lims) {
          # Only show breaks >= 0 and below plot_max_y
          breaks <- scales::pretty_breaks()(lims)
          breaks[breaks >= 0 & breaks <= plot_max_y]
        },
        labels = scales::comma
      ) +
      # Ensure fill matches colors in "plot_bins"
      scale_fill_identity() +
      # Add labels and title
      labs(
        x = "Percentage of Population in Poverty", 
        y = "Total Population in Poverty"
      ) +
      # Apply theme
      hrbrthemes::theme_ipsum() +
      theme(
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, margin = margin(t = 0)),  # Move tick labels further up toward the bin labels
        axis.line.x = element_blank(),  # Remove the horizontal gray bar (x-axis line)
        panel.grid.major.x = element_line(color = "lightgray", linewidth = 0.4, linetype = "solid"),  # Subtle x-axis grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
        panel.grid.major = element_line(linewidth = 0.4, color = "lightgray"),  # Lighten y-axis grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.text.y = element_text(size = 12, margin = margin(r = 5)),  # Move y-axis tick labels slightly right
        plot.margin = margin(t = 50, r = 30, b = 0, l = 10)  # Add extra space at the bottom
      )
    
    # Convert to plotly object
    p_plotly <- ggplotly(p, tooltip = "text")
    
    # ggplot2 will trigger warning "Ignoring unknown aesthetics: text" for geom_point
    ## Safe to ignore, as text aesthetic is passed to plotly for hover info
    ## Can suppress warning if desired with code
    
    # Remove hover info for geom_rect layer
    # Loop through all traces in the plotly object
    # Discoverable via str(p_plotly$x$data, max.level = 2)
    for (i in seq_along(p_plotly$x$data)) {
      tr <- p_plotly$x$data[[i]]
      if (
        # Only geom_rect traces have this combination of all three properties, so it will only be true for that layer
        !is.null(tr$mode) && tr$mode == "lines" &&
        !is.null(tr$fill) && tr$fill == "toself" &&
        !is.null(tr$fillcolor)
      ) {
        p_plotly$x$data[[i]]$hoverinfo <- "skip"
        p_plotly$x$data[[i]]$hovertemplate <- NULL
      }
    }
    # For geom_text labels
    if (
      # Only geom_text traces have this combination of properties
      !is.null(tr$mode) && tr$mode == "text" &&
      !is.null(tr$text)
    ) {
      p_plotly$x$data[[i]]$hoverinfo <- "skip"
      p_plotly$x$data[[i]]$hovertemplate <- NULL
    }
    
    # Add custom layout
    p_plotly %>%
      layout(
        margin = list(t = 100),
        # Position plot title and subtitle
        title = list(
          text = paste0(
            "<b>Total and Percentage of Population in Poverty</b><br>",
            "<span style='font-size:20px;'>", plot_county_name, "</span>"
          ),
          x = 0.05,
          xanchor = "left",
          y = 1.02,
          yanchor = "bottom",
          font = list(family = "Arial Narrow", size = 24)
        ),
        # Position axis titles at ideal distance from axis labels
        xaxis = list(title = list(
          text = "Percentage of Population in Poverty",
          standoff = 10
        )),
        yaxis = list(title = list(
          text = "Total Population in Poverty",
          standoff = 2
        )),
        legend = list(
          title = list(
            text = "Race/Ethnicity",
            font = list(size = 18, family = "Arial Narrow")
          )
        )
      )
  })
  
  # Plot Footnote
  
  # Helper to determine if plot is empty
  is_plot_empty <- reactive({
    plot_df <- plot_data()
    is.null(plot_df) || nrow(plot_df) == 0
  })
  
  # Create plot footnote
  plot_footnote_text <- paste0(
    "<span style='font-size:13px; color:#666; font-style:italic;'>",
    "*Unless White Alone is selected, the plot will default to display White Alone, Not Hispanic or Latino.<br>",
    "*If the population estimate for a race/ethnicity is zero, that race/ethnicity is not displayed on the plot.<br>",
    "*If the population in poverty estimate + or - the margin of error exceeds logical limits (lower bound: 0 or 0%, upper bound: population ", 
    "estimate or 100%), the lower and/or upper bound is coerced to remain within logical limits. This can cause lines to appear crooked.<br>",
    "</span>"
  )
  
  # Render the plot footnote
  output$plot_footnote <- renderUI({
    if (is_plot_empty()) return(NULL)       # Don't show footnote if plot is empty
    HTML(plot_footnote_text)
  })
  
  ## -- Data Tables ------------------------------------------------------------
  
  # Get SVG for the "download" icon
  download_svg <- bsicons::bs_icon("download", size = "1em")
  # Button label: SVG + text
  csv_btn_label <- paste0(download_svg, " CSV")
  
  # Render poverty data table
  output$poverty_data_table <- renderDataTable({
    # Create detailed variable labels for display
    poverty_data_table_var_labels <- c(
      NAME = "Location",
      sapE = "Summary Population Estimate (All Races/Ethnicities)",
      race_eth_s = "Selected Race/Ethnicity Abbreviation",
      pE = "Population Estimate",
      pM = "Population Margin of Error",
      pbpE = "Population Below Poverty Level Estimate",
      pbpM = "Population Below Poverty Level Margin of Error",
      gnsmpbpE = "Population Below Poverty Level Compared to National Mean",
      gssmpbpE = "Population Below Poverty Level Compared to State Mean",
      ppbpE = "Percent Population Below Poverty Level Estimate",
      ppbpM = "Percent Population Below Poverty Level Margin of Error",
      gppbpE = "Poverty Rate",
      rppbpE = "Reliability of Percent Population Below Poverty Level"
    )
    
    # Define target columns for the summary table
    target_cols <- names(poverty_data_table_var_labels)
    
    # Specify which columns to format
    percent_cols <- c("ppbpE", "ppbpM")
    comma_cols <- c("sapE", "pE", "pM", "pbpE", "pbpM")
    
    poverty_data <- submitted_poverty_data()
    
    # Helper for extracting values, always returning "-" for NA/missing
    get_vals <- function(df, cols) {
      if (is.null(df) || nrow(df) == 0) return(rep("-", length(cols)))
      sapply(cols, function(col) {
        val <- df[[col]]
        if (is.null(val) || length(val) == 0 || is.na(val)) {
          "-"
        } else if (col %in% percent_cols) {
          # Format as percentage with 1 decimal and % sign
          paste0(formatC(100 * as.numeric(val), format = "f", digits = 1), "%")
        } else if (col %in% comma_cols) {
          # Format with commas, no decimals
          formatC(as.numeric(val), format = "f", digits = 0, big.mark = ",")
        } else {
          as.character(val)
        }
      })
    }
    
    # County column: only if one county is selected
    show_county <- !is.null(poverty_data) && nrow(poverty_data) == 1
    county_vals <- if (show_county) get_vals(st_drop_geometry(poverty_data), target_cols) else rep("-", length(target_cols))
    
    # State column: only if all rows in poverty_data are from the same state_n
    show_state <- !is.null(poverty_data) && nrow(poverty_data) > 0 && length(unique(poverty_data$state_n)) == 1
    state_vals <- rep("-", length(target_cols))
    if (show_state) {
      # Find *matching* row in state_poverty_data (for the right state and race/ethnicity)
      state_row <- state_poverty_data[
        state_poverty_data$state_n == unique(poverty_data$state_n) &
          state_poverty_data$race_eth_f == unique(poverty_data$race_eth_f),
      ]
      if (nrow(state_row) == 1) {
        state_vals <- get_vals(state_row, target_cols)
      }
    }
    
    # US column: always shown, filtered for race/ethnicity if possible
    us_vals <- rep("-", length(target_cols))
    if (!is.null(poverty_data) && "race_eth_f" %in% colnames(poverty_data)) {
      us_row <- us_poverty_data[
        us_poverty_data$race_eth_f == unique(poverty_data$race_eth_f),
      ]
      if (nrow(us_row) == 1) {
        us_vals <- get_vals(us_row, target_cols)
      }
    } else if (nrow(us_poverty_data) == 1) {
      us_vals <- get_vals(us_poverty_data, target_cols)
    }
    
    summary_tbl <- tibble::tibble(
      'Data Field' = unname(poverty_data_table_var_labels[target_cols]),
      County = county_vals,
      State = state_vals,
      Nation = us_vals
    )
    
    DT::datatable(
      summary_tbl,
      extensions = "Buttons",
      options = list(
        dom = 'tB',             # Table, then Buttons below
        buttons = list(
          list(
            extend = "csv",
            text = csv_btn_label,
            filename = "poverty_data_summary_table"
          )
        ),
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        pageLength = nrow(summary_tbl),
        ordering = FALSE
      ),
      escape = FALSE, # Allow HTML/SVG in button text
      rownames = FALSE
    )
  })
  
  ## -- Reset ------------------------------------------------------------------
  
  # Reset button functionality
  observeEvent(input$reset_button, {
    
    ### -- Reset Dropdowns -----------------------------------------------------
    
    # Reset Map Layer radio buttons
    updateRadioButtons(
      session,
      inputId = "map_layer_selection",
      selected = "poverty_rates"  
    )
    
    # Reset Race/Ethnicity dropdown
    updateSelectInput(
      session,
      inputId = "race_eth_dropdown",
      selected = "All"
    )
    
    # Reset State dropdown
    updateSelectizeInput(
      session,
      inputId = "state_dropdown",
      choices = state_choices,
      selected = NULL,
      server = TRUE  
    )
    
    # Reset County dropdown
    updateSelectizeInput(
      session,
      inputId = "county_dropdown",
      choices = county_choices,
      selected = NULL,
      server = TRUE  
    )
    
    # Reset Population in Poverty (US) dropdown
    updateSelectizeInput(
      session,
      inputId = "n_poverty_total_dropdown",
      choices = n_poverty_total_choices,
      selected = NULL,
      server = TRUE
    )
    
    # Reset Population in Poverty (State) dropdown
    updateSelectizeInput(
      session,
      inputId = "s_poverty_total_dropdown",
      choices = s_poverty_total_choices,
      selected = NULL,
      server = TRUE
    )
    
    # Reset Poverty Rate dropdown
    updateSelectizeInput(
      session,
      inputId = "poverty_rate_dropdown",
      choices = poverty_rate_choices,
      selected = NULL,
      server = TRUE
    )
    
    # Reset Reliability dropdown
    updateSelectizeInput(
      session,
      inputId = "poverty_reliability_dropdown",
      choices = poverty_reliability_choices,
      selected = NULL,
      server = TRUE
    )
    
    # Reset Margin of Error dropdown
    updateSelectizeInput(
      session,
      inputId = "poverty_moe_dropdown",
      choices = poverty_moe_choices,
      selected = NULL,
      server = TRUE
    )
    
    ### -- Reset Submitted Data ------------------------------------------------
    
    # Causes plot and data tables to reset
    
    submitted_poverty_data(NULL)
    
    ### -- Reset Map -----------------------------------------------------------
    
    # Reset last bounding box to us_bounds
    ## Necessary for when user returns to map tab after viewing plot or data tables
    last_bbox(us_bounds)
    
    # Reset map to initial state
    maplibre_proxy("map") %>%
      clear_layer("poverty_rates") %>%
      clear_layer("poverty_extrusion") %>%
      add_fill_layer(
        id = "poverty_rates",
        source = initial_map_data,  
        fill_color = step_expr(
          column = "sogppbpE",
          # Use plasma colors from viridis package
          base  = c("#0D0887FF"),
          stops = c("#7E03A8FF", "#CC4678FF", "#F89441FF", "#F0F921FF", "#8a8a8a"),
          values = c(2, 3, 4, 5, 6)
        ),
        fill_opacity = .7,
        tooltip = "ptt",
        hover_options = list(
          fill_color = "#00FFFF",
          fill_opacity = 1
        )
      ) %>% 
      add_categorical_legend(
        legend_title = "Poverty Rate",
        values = c("Extreme", "Very High", "High", "Moderate", "Low", "Zero Population"),
        # Use B3 instead of FF at end of hex codes to match map opacity of .7 (255 full opacity x .7 = 178.5 ~ 179)
        colors = c("#0D0887B3", "#7E03A8B3", "#CC4678B3", "#F89441B3", "#F0F921B3", "#8a8a8aB3"),
        circular_patches = FALSE,
        position = "bottom-left",
        add = FALSE,
        unique_id = "poverty_rates_legend",
        layer_id = "poverty_rates"
      ) %>%
      fit_bounds(
        bbox = last_bbox
      )
  })
  
  ## -- Miscellaneous ----------------------------------------------------------
   
  # Trigger initial filtering on app load
  observe({
    # Set the default value for race_eth_dropdown to "All"
    if (is.null(input$race_eth_dropdown)) {
      updateSelectInput(session, "race_eth_dropdown", selected = "All")
    }
  })
}

# ──────────────────────────────────────────────────────────────────────────────
# APP --------------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
