# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(ncdf4)
library(leaflet)

# Add resource path for images
addResourcePath("images", "/Users/praddy5/Desktop/Dashboard/images")

# Function to check for processed data
check_processed_data <- function(data_type, year = NULL) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    return(FALSE)
  }
  
  if (!is.null(year)) {
    file_pattern <- paste0(".*", year, ".*\\.rds$")
    files <- list.files(processed_path, pattern = file_pattern, full.names = TRUE)
    return(length(files) > 0)
  }
  
  return(TRUE)
}

# Function to load processed data
load_processed_data <- function(data_type, year = NULL) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!is.null(year)) {
    file_pattern <- paste0(".*", year, ".*\\.rds$")
    file <- list.files(processed_path, pattern = file_pattern, full.names = TRUE)[1]
  } else {
    file <- list.files(processed_path, pattern = "\\.rds$", full.names = TRUE)[1]
  }
  
  if (file.exists(file)) {
    return(readRDS(file))
  }
  return(NULL)
}

# Function to save processed data
save_processed_data <- function(data, data_type, year = NULL) {
  processed_path <- file.path("data", paste0(data_type, "_processed"))
  if (!dir.exists(processed_path)) {
    dir.create(processed_path, recursive = TRUE)
  }
  
  if (!is.null(year)) {
    file_name <- paste0(data_type, "_", year, ".rds")
  } else {
    file_name <- paste0(data_type, ".rds")
  }
  
  saveRDS(data, file.path(processed_path, file_name))
}

# ASU Theme Colors
asu_maroon <- "#8C1D40"
asu_gold <- "#FFC627"
asu_light_gold <- "#FFD700"
asu_dark_maroon <- "#5C0025"

# Custom CSS for enhanced ASU theme
custom_css <- tags$head(
  tags$style(HTML(paste0("
    /* Main header */
    .skin-blue .main-header .logo {
      background-color: white;
      color: #000000;
      font-weight: bold;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Header navbar */
    .skin-blue .main-header .navbar {
      background-color: white;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Sidebar */
    .skin-blue .main-sidebar {
      background-color: white;
      border-right: 4px solid ", asu_maroon, ";
      box-shadow: 2px 0 4px rgba(0,0,0,0.1);
    }
    
    /* Sidebar menu items */
    .skin-blue .sidebar-menu > li > a {
      color: #000000;
      font-weight: 500;
    }
    
    .skin-blue .sidebar-menu > li.active > a {
      background-color: ", asu_gold, ";
      color: #000000;
      border-left: 4px solid ", asu_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      font-weight: bold;
    }
    
    .skin-blue .sidebar-menu > li:hover > a {
      background-color: ", asu_light_gold, ";
      color: #000000;
      border-left: 4px solid ", asu_maroon, ";
      transition: all 0.3s ease;
      font-weight: bold;
    }
    
    /* Sidebar icons */
    .skin-blue .sidebar-menu > li > a > .fa,
    .skin-blue .sidebar-menu > li > a > .fas,
    .skin-blue .sidebar-menu > li > a > .far {
      color: #000000;
    }
    
    /* Sidebar treeview */
    .skin-blue .sidebar-menu > li > .treeview-menu {
      background-color: white;
    }
    
    .skin-blue .sidebar-menu > li > .treeview-menu > li > a {
      color: #000000;
    }
    
    .skin-blue .sidebar-menu > li > .treeview-menu > li.active > a {
      color: #000000;
      font-weight: bold;
    }
    
    /* Box headers */
    .box-header {
      background-color: white;
      color: #000000;
      border-bottom: 2px solid ", asu_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Box content */
    .box {
      background-color: white;
      border: 1px solid #ddd;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-radius: 4px;
      margin-bottom: 20px;
      transition: all 0.3s ease;
    }
    
    .box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    /* Tab panels */
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: ", asu_maroon, ";
      box-shadow: 0 -2px 4px rgba(0,0,0,0.1);
    }
    
    /* Buttons */
    .btn-primary {
      background-color: ", asu_maroon, ";
      border-color: ", asu_dark_maroon, ";
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: all 0.3s ease;
    }
    
    .btn-primary:hover {
      background-color: ", asu_dark_maroon, ";
      border-color: ", asu_maroon, ";
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      transform: translateY(-1px);
    }
    
    /* Info boxes */
    .info-box {
      background-color: white;
      border: 1px solid #ddd;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-radius: 4px;
      margin-bottom: 20px;
      transition: all 0.3s ease;
    }
    
    .info-box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    .info-box-icon {
      background-color: ", asu_gold, ";
      color: #000000;
      border-right: 2px solid ", asu_maroon, ";
    }
    
    /* Dashboard body */
    .content-wrapper {
      background-color: #f9f9f9;
    }
    
    /* Plot containers */
    .plot-container {
      background-color: white;
      padding: 15px;
      border-radius: 4px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Variable info tiles */
    .variable-info {
      background-color: white;
      padding: 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-left: 4px solid ", asu_gold, ";
      border-right: 4px solid ", asu_maroon, ";
    }
    
    .variable-info h4 {
      color: #000000;
      margin-bottom: 10px;
      font-weight: bold;
    }
    
    .variable-info p {
      color: #000000;
      margin-bottom: 5px;
    }

    /* General text styles */
    body {
      color: #000000;
    }

    h1, h2, h3, h4, h5, h6 {
      color: #000000;
    }

    .box-title {
      color: #000000;
      font-weight: bold;
    }

    .nav-tabs > li > a {
      color: #000000;
    }

    .nav-tabs > li.active > a {
      color: #000000;
      font-weight: bold;
    }

    /* Info tiles with yellow border and 3D shadow */
    .info-tile {
      background-color: white;
      border-left: 4px solid #FFC627;
      border-right: 4px solid #8C1D40;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      padding: 15px;
      margin-bottom: 15px;
      border-radius: 4px;
      transition: all 0.3s ease;
    }

    .info-tile:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }

    .info-tile h4 {
      color: #000000;
      margin-bottom: 10px;
      font-weight: bold;
    }

    .info-tile p {
      color: #000000;
      margin-bottom: 5px;
      font-size: 14px;
    }
  ")))
)

# Add static image paths with proper file handling
static_images <- list(
  basin = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_basin_map.png",
  huc10 = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_huc10_map.png",
  snotel = "/Users/praddy5/Desktop/Dashboard/images/colorado_river_snotel_map.png",
  vic_precip = "/Users/praddy5/Desktop/Dashboard/images/vic_precipitation_map.png",
  vic_et = "/Users/praddy5/Desktop/Dashboard/images/vic_evapotranspiration_map.png",
  vic_runoff = "/Users/praddy5/Desktop/Dashboard/images/vic_runoff_map.png",
  vic_soil = "/Users/praddy5/Desktop/Dashboard/images/vic_soil_moisture_layer_1_map.png",
  vic_swe = "/Users/praddy5/Desktop/Dashboard/images/vic_snow_water_equivalent_map.png",
  vic_baseflow = "/Users/praddy5/Desktop/Dashboard/images/vic_baseflow_map.png",
  smap = "/Users/praddy5/Desktop/Dashboard/images/smap_time_series.png",
  grace = "/Users/praddy5/Desktop/Dashboard/images/grace_time_series.png",
  snotel_ts = "/Users/praddy5/Desktop/Dashboard/images/snotel_time_series.png",
  monthly = "/Users/praddy5/Desktop/Dashboard/images/monthly_trends.png",
  historical = "/Users/praddy5/Desktop/Dashboard/images/historical_trend.png",
  combined = "/Users/praddy5/Desktop/Dashboard/images/combined_map.png"
)

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Colorado River Basin Dashboard",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Spatial Map", tabName = "spatial", icon = icon("map")),
      menuItem("VIC Model Output", tabName = "vic", icon = icon("water")),
      menuItem("SMAP Data", tabName = "smap", icon = icon("satellite")),
      menuItem("GRACE Data", tabName = "grace", icon = icon("globe")),
      menuItem("Snow Water Equivalent", tabName = "swe", icon = icon("snowflake")),
      menuItem("Precipitation", tabName = "precip", icon = icon("cloud-rain")),
      menuItem("Soil Moisture", tabName = "soil", icon = icon("seedling"))
    )
  ),
  
  dashboardBody(
    custom_css,
    tabItems(
      # Spatial Map Tab
      tabItem(tabName = "spatial",
              fluidRow(
                div(class = "variable-info",
                    h4("Basin Information"),
                    p("The Colorado River Basin covers approximately 246,000 square miles across seven U.S. states and Mexico."),
                    p("Key features include major reservoirs like Lake Powell and Lake Mead."),
                    p("The basin supports over 40 million people and 5.5 million acres of farmland.")
                )
              ),
              fluidRow(
                # Add info tiles for basin monitoring
                infoBox(
                  title = "Basin Area",
                  value = "246,000 sq mi",
                  icon = icon("map"),
                  color = "maroon",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: USGS National Hydrography Dataset (NHD) - 2024</span>")
                ),
                infoBox(
                  title = "Population Served",
                  value = "40M+",
                  icon = icon("users"),
                  color = "yellow",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2024</span>")
                ),
                infoBox(
                  title = "Farmland",
                  value = "5.5M acres",
                  icon = icon("tractor"),
                  color = "green",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: USDA Census of Agriculture - 2022</span>")
                ),
                infoBox(
                  title = "Major Reservoirs",
                  value = "2",
                  icon = icon("water"),
                  color = "blue",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2024</span>")
                )
              ),
              fluidRow(
                # Add climate info tiles
                infoBox(
                  title = "Average Temperature",
                  value = "12.5°C",
                  icon = icon("temperature-high"),
                  color = "red",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: NOAA Climate Data - 2023</span>")
                ),
                infoBox(
                  title = "Annual Precipitation",
                  value = "400mm",
                  icon = icon("cloud-rain"),
                  color = "aqua",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: NOAA Climate Data - 2023</span>")
                ),
                infoBox(
                  title = "Drought Status",
                  value = "Moderate",
                  icon = icon("sun"),
                  color = "orange",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Drought Monitor - 2024</span>")
                ),
                infoBox(
                  title = "Water Use",
                  value = "15MAF/yr",
                  icon = icon("faucet"),
                  color = "teal",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: US Bureau of Reclamation - 2023</span>")
                )
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel("Colorado Basin",
                           box(width = 12, title = "Colorado River Basin Map",
                               div(class = "info-tile",
                                   h4("Basin Overview"),
                                   p("The Colorado River Basin spans across seven U.S. states, covering approximately 246,000 square miles."),
                                   p("Key features include Lake Powell and Lake Mead reservoirs.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       uiOutput("basin_map")
                                   )
                                 ),
                                 column(width = 4,
                                   # Add info tiles for basin characteristics
                                   infoBox(
                                     title = "Total Area",
                                     value = "246,000 sq mi",
                                     icon = icon("map"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Source: USGS NHD - 2024"
                                   ),
                                   infoBox(
                                     title = "Major Reservoirs",
                                     value = "2",
                                     icon = icon("water"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Lake Powell and Lake Mead"
                                   ),
                                   infoBox(
                                     title = "States Covered",
                                     value = "7",
                                     icon = icon("flag"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "AZ, CA, CO, NM, NV, UT, WY"
                                   )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS National Hydrography Dataset (NHD) - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  ),
                  tabPanel("HUC10 Map",
                           box(width = 12, title = "HUC10 Watersheds",
                               div(class = "info-tile",
                                   h4("HUC10 Watersheds"),
                                   p("Hydrologic Unit Code (HUC) 10 watersheds represent sub-basins used for water management."),
                                   p("These units help in organizing and managing water resources at a local scale.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/colorado_river_huc10_map.png",
                                               alt = "HUC10 Watersheds Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("Hydrologic Unit Code (HUC) 10 watersheds within the Colorado River Basin."),
                                           p("These units represent sub-basins used for water management.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   # Add info tiles for HUC10 characteristics
                                   infoBox(
                                     title = "Total HUC10 Units",
                                     value = "100+",
                                     icon = icon("layer-group"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Source: USGS WBD - 2024"
                                   ),
                                   infoBox(
                                     title = "Average Area",
                                     value = "2,400 sq mi",
                                     icon = icon("ruler"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Per HUC10 watershed"
                                   ),
                                   infoBox(
                                     title = "Data Resolution",
                                     value = "1:24,000",
                                     icon = icon("map-marked-alt"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Scale of delineation"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Area Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/huc10_area.png",
                                                          alt = "HUC10 Area Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Area distribution of HUC10 watersheds."),
                                                      p("Shows the size distribution of sub-basins.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Largest HUC10",
                                                value = "5,000+ sq mi",
                                                icon = icon("expand"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Upper Colorado River"
                                              ),
                                              infoBox(
                                                title = "Smallest HUC10",
                                                value = "500 sq mi",
                                                icon = icon("compress"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Lower Basin tributaries"
                                              ),
                                              infoBox(
                                                title = "Median Area",
                                                value = "2,400 sq mi",
                                                icon = icon("balance-scale"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Typical watershed size"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined View",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/basin_huc10_snotel.png",
                                                          alt = "Combined Basin and HUC10 Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Combined view of Colorado Basin and HUC10 watersheds."),
                                                      p("Shows the spatial relationship between basin boundaries and watersheds.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Spatial Resolution",
                                                value = "1:24,000",
                                                icon = icon("ruler"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Map scale"
                                              ),
                                              infoBox(
                                                title = "Data Integration",
                                                value = "3 Layers",
                                                icon = icon("layer-group"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Basin, HUC10, SNOTEL"
                                              ),
                                              infoBox(
                                                title = "Update Frequency",
                                                value = "Annual",
                                                icon = icon("sync"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Data refresh cycle"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Detailed Map",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/huc10_map.png",
                                                          alt = "Detailed HUC10 Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Detailed view of HUC10 watershed boundaries."),
                                                      p("Provides high-resolution visualization of watershed delineations.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Boundary Type",
                                                value = "Hydrologic",
                                                icon = icon("water"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Watershed boundaries"
                                              ),
                                              infoBox(
                                                title = "Coordinate System",
                                                value = "NAD83",
                                                icon = icon("globe"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Geographic reference"
                                              ),
                                              infoBox(
                                                title = "Data Source",
                                                value = "USGS WBD",
                                                icon = icon("database"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Watershed Boundary Dataset"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS Watershed Boundary Dataset (WBD) - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  ),
                  tabPanel("SNOTEL Stations",
                           box(width = 12, title = "SNOTEL Stations in Colorado Basin",
                               div(class = "info-tile",
                                   h4("SNOTEL Network"),
                                   p("SNOTEL (SNOwpack TELemetry) stations provide real-time snowpack data across the basin."),
                                   p("Each station measures snow water equivalent, snow depth, temperature, and precipitation.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/colorado_river_snotel_map.png",
                                               alt = "SNOTEL Stations Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("SNOTEL stations across the Colorado River Basin."),
                                           p("Each station provides real-time snowpack data.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   infoBox(
                                     title = "Total Stations",
                                     value = "100+",
                                     icon = icon("snowflake"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Active monitoring sites"
                                   ),
                                   infoBox(
                                     title = "Elevation Range",
                                     value = "5,000-12,000 ft",
                                     icon = icon("mountain"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Station coverage"
                                   ),
                                   infoBox(
                                     title = "Update Frequency",
                                     value = "Daily",
                                     icon = icon("clock"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Data collection"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Time Series",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_time_series.png",
                                                          alt = "SNOTEL Time Series",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Time series of snow water equivalent (SWE) from SNOTEL stations."),
                                                      p("Shows daily variations in snowpack conditions.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Data Range",
                                                value = "1982-2024",
                                                icon = icon("calendar"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Historical coverage"
                                              ),
                                              infoBox(
                                                title = "Variables",
                                                value = "4",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "SWE, depth, temp, precip"
                                              ),
                                              infoBox(
                                                title = "Resolution",
                                                value = "Daily",
                                                icon = icon("clock"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Temporal resolution"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Seasonal Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_seasonal.png",
                                                          alt = "Seasonal Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Seasonal patterns of snowpack accumulation and melt."),
                                                      p("Helps understand annual snowpack dynamics.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Peak SWE",
                                                value = "April",
                                                icon = icon("calendar-alt"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Typical maximum"
                                              ),
                                              infoBox(
                                                title = "Melt Season",
                                                value = "May-July",
                                                icon = icon("sun"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Primary melt period"
                                              ),
                                              infoBox(
                                                title = "Accumulation",
                                                value = "Nov-Mar",
                                                icon = icon("snowflake"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Snow accumulation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Elevation Analysis",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_elevation.png",
                                                          alt = "Elevation Analysis",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Relationship between elevation and snowpack characteristics."),
                                                      p("Important for understanding snow distribution patterns.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Elevation Range",
                                                value = "5,000-12,000 ft",
                                                icon = icon("mountain"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Station coverage"
                                              ),
                                              infoBox(
                                                title = "Optimal Zone",
                                                value = "8,000-10,000 ft",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Peak snow accumulation"
                                              ),
                                              infoBox(
                                                title = "Data Points",
                                                value = "100+",
                                                icon = icon("map-marker-alt"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Station locations"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined Map",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "map-container",
                                                  tags$img(src = "images/basin_huc10_snotel.png",
                                                          alt = "Combined Basin, HUC10, and SNOTEL Map",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "map-caption",
                                                      p("Combined view of Colorado Basin, HUC10 watersheds, and SNOTEL stations."),
                                                      p("Shows the spatial relationship between different data layers.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Data Layers",
                                                value = "3",
                                                icon = icon("layer-group"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Basin, HUC10, SNOTEL"
                                              ),
                                              infoBox(
                                                title = "Spatial Coverage",
                                                value = "100%",
                                                icon = icon("globe"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Basin coverage"
                                              ),
                                              infoBox(
                                                title = "Integration",
                                                value = "Seamless",
                                                icon = icon("link"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Data layer integration"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: NRCS SNOTEL Network - Updated Daily",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  )
                )
              )
      ),
      
      # VIC Model Output Tab
      tabItem(tabName = "vic",
              fluidRow(
                div(class = "variable-info",
                    h4("VIC Model Variables"),
                    p("Precipitation: Total water input from rain and snow"),
                    p("Evapotranspiration: Water loss through plant transpiration and soil evaporation"),
                    p("Runoff: Surface water flow into streams and rivers"),
                    p("Soil Moisture: Water content in the soil profile")
                )
              ),
              fluidRow(
                # Add info tiles for VIC model monitoring
                infoBox(
                  title = "Model Resolution",
                  value = "4km",
                  icon = icon("ruler"),
                  color = "maroon",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Temporal Resolution",
                  value = "Daily",
                  icon = icon("clock"),
                  color = "yellow",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Data Range",
                  value = "1982-2024",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Outputs</span>")
                ),
                infoBox(
                  title = "Variables",
                  value = "7",
                  icon = icon("list"),
                  color = "blue",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                )
              ),
              fluidRow(
                box(width = 12, title = "VIC Model Output",
                    fluidRow(
                      column(width = 6,
                             uiOutput("vic_variable")
                      ),
                      column(width = 6,
                             sliderInput("vic_year", "Select Year:",
                                       min = 1982, max = 2024, value = 2024,
                                       step = 1, sep = "")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 6, title = "Spatial Distribution",
                    plotlyOutput("vic_map", height = "500px")),
                box(width = 6, title = "Time Series",
                    plotlyOutput("vic_timeseries", height = "500px"))
              ),
              fluidRow(
                box(width = 12, title = "Monthly Statistics",
                    plotlyOutput("vic_monthly", height = "300px"))
              )
      ),
      
      # SMAP Data Tab
      tabItem(tabName = "smap",
              fluidRow(
                div(class = "variable-info",
                    h4("SMAP Data Information"),
                    p("SMAP (Soil Moisture Active Passive) provides global measurements of soil moisture."),
                    p("Data includes three layers: Surface (0-5 cm), Root Zone (0-100 cm), and Profile (0 cm to bedrock)."),
                    p("Spatial resolution: 9km, Temporal resolution: 2-3 days")
                )
              ),
              fluidRow(
                # Add info tiles for SMAP monitoring
                infoBox(
                  title = "Spatial Resolution",
                  value = "9km",
                  icon = icon("ruler"),
                  color = "maroon",
                  width = 3,
                  subtitle = "Source: NASA SMAP Mission - 2024"
                ),
                infoBox(
                  title = "Temporal Resolution",
                  value = "2-3 days",
                  icon = icon("clock"),
                  color = "yellow",
                  width = 3,
                  subtitle = "Source: NASA SMAP Mission - 2024"
                ),
                infoBox(
                  title = "Data Range",
                  value = "2015-2024",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  subtitle = "Source: NASA SMAP Data Archive"
                ),
                infoBox(
                  title = "Soil Layers",
                  value = "3",
                  icon = icon("layer-group"),
                  color = "blue",
                  width = 3,
                  subtitle = "Source: NASA SMAP Mission - 2024"
                )
              ),
              fluidRow(
                box(width = 12, title = "SMAP Soil Moisture Map",
                    fluidRow(
                      column(width = 4,
                             selectInput("smap_layer", "Select Layer:",
                                       choices = c("Surface (0-5 cm)" = "surface",
                                                 "Root Zone (0-100 cm)" = "rootzone",
                                                 "Profile (0 cm to bedrock)" = "profile"))
                      ),
                      column(width = 4,
                             dateInput("smap_date", "Select Date:",
                                     value = Sys.Date(),
                                     min = "2000-01-01",
                                     max = Sys.Date())
                      )
                    ),
                    plotlyOutput("smap_map", height = "600px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Series Analysis",
                    plotlyOutput("smap_timeseries", height = "400px")),
                box(width = 6, title = "Layer Comparison",
                    plotlyOutput("smap_comparison", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Seasonal Analysis",
                    plotlyOutput("smap_seasonal", height = "400px")),
                box(width = 6, title = "Anomaly Analysis",
                    plotlyOutput("smap_anomaly", height = "400px"))
              )
      ),
      
      # GRACE Data Tab
      tabItem(tabName = "grace",
              fluidRow(
                div(class = "variable-info",
                    h4("GRACE Data Information"),
                    p("GRACE (Gravity Recovery and Climate Experiment) measures terrestrial water storage."),
                    p("Provides monthly estimates of total water storage changes."),
                    p("Spatial resolution: ~300km, Temporal resolution: Monthly")
                )
              ),
              fluidRow(
                # Add info tiles for GRACE monitoring
                infoBox(
                  title = "Spatial Resolution",
                  value = "300km",
                  icon = icon("ruler"),
                  color = "maroon",
                  width = 3,
                  subtitle = "Source: NASA GRACE Mission - 2024"
                ),
                infoBox(
                  title = "Temporal Resolution",
                  value = "Monthly",
                  icon = icon("calendar"),
                  color = "yellow",
                  width = 3,
                  subtitle = "Source: NASA GRACE Mission - 2024"
                ),
                infoBox(
                  title = "Data Range",
                  value = "2002-2024",
                  icon = icon("history"),
                  color = "green",
                  width = 3,
                  subtitle = "Source: NASA GRACE Data Archive"
                ),
                infoBox(
                  title = "Uncertainty",
                  value = "±2cm",
                  icon = icon("exclamation-triangle"),
                  color = "blue",
                  width = 3,
                  subtitle = "Source: NASA GRACE Mission - 2024"
                )
              ),
              fluidRow(
                box(width = 12, title = "GRACE Terrestrial Water Storage Map",
                    plotlyOutput("grace_map", height = "600px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Series with Trend Analysis",
                    plotlyOutput("grace_timeseries", height = "400px")),
                box(width = 6, title = "Seasonal Analysis",
                    plotlyOutput("grace_seasonal", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "TWS Anomalies",
                    plotlyOutput("grace_anomaly", height = "400px"))
              )
      ),
      
      # Snow Water Equivalent Tab
      tabItem(tabName = "swe",
              fluidRow(
                div(class = "variable-info",
                    h4("Snow Water Equivalent Information"),
                    p("SNOTEL (SNOwpack TELemetry) stations provide real-time snowpack data across the Colorado River Basin."),
                    p("Data includes snow water equivalent (SWE), snow depth, temperature, and precipitation."),
                    p("Critical for water resource management and flood forecasting.")
                )
              ),
              fluidRow(
                # Add info tiles for SNOTEL monitoring
                infoBox(
                  title = "Stations in Basin",
                  value = "100+",
                  icon = icon("snowflake"),
                  color = "maroon",
                  width = 3,
                  subtitle = "Source: NRCS SNOTEL Network - 2024"
                ),
                infoBox(
                  title = "Update Frequency",
                  value = "Daily",
                  icon = icon("clock"),
                  color = "yellow",
                  width = 3,
                  subtitle = "Source: NRCS SNOTEL Network - 2024"
                ),
                infoBox(
                  title = "Data Range",
                  value = "1982-2024",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  subtitle = "Source: NRCS SNOTEL Data Archive"
                ),
                infoBox(
                  title = "Metrics",
                  value = "4",
                  icon = icon("chart-line"),
                  color = "blue",
                  width = 3,
                  subtitle = "Source: NRCS SNOTEL Network - 2024"
                )
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel("Station Map",
                           box(width = 12, title = "SNOTEL Stations in Colorado Basin",
                               div(class = "map-container",
                                   tags$img(src = "images/colorado_river_snotel_map.png",
                                           alt = "SNOTEL Stations Map",
                                           style = "width: 100%; height: auto; max-height: 800px; object-fit: contain;"),
                                   div(class = "map-caption",
                                       p("SNOTEL stations across the Colorado River Basin."),
                                       p("Each station provides real-time snowpack data.")
                                   )
                               )
                           )
                  ),
                  tabPanel("Time Series",
                           box(width = 12, title = "SNOTEL Time Series",
                               div(class = "plot-container",
                                   tags$img(src = "images/snotel_time_series.png",
                                           alt = "SNOTEL Time Series",
                                           style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                   div(class = "plot-caption",
                                       p("Time series of snow water equivalent (SWE) from SNOTEL stations."),
                                       p("Shows daily variations in snowpack conditions.")
                                   )
                               )
                           )
                  ),
                  tabPanel("Seasonal Analysis",
                           box(width = 12, title = "Seasonal Snowpack Patterns",
                               div(class = "plot-container",
                                   tags$img(src = "images/snotel_seasonal.png",
                                           alt = "Seasonal Analysis",
                                           style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                   div(class = "plot-caption",
                                       p("Seasonal patterns of snowpack accumulation and melt."),
                                       p("Helps understand annual snowpack dynamics.")
                                   )
                               )
                           )
                  ),
                  tabPanel("Elevation Analysis",
                           box(width = 12, title = "Elevation Distribution",
                               div(class = "plot-container",
                                   tags$img(src = "images/snotel_elevation.png",
                                           alt = "Elevation Analysis",
                                           style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                   div(class = "plot-caption",
                                       p("Relationship between elevation and snowpack characteristics."),
                                       p("Important for understanding snow distribution patterns.")
                                   )
                               )
                           )
                  )
                )
              ),
              fluidRow(
                box(width = 12, title = "Interactive Analysis",
                    fluidRow(
                      column(width = 4,
                             selectInput("swe_metric", "Select Metric:",
                                       choices = c("Snow Water Equivalent" = "WTEQ",
                                                 "Snow Depth" = "SNWD",
                                                 "Temperature" = "TOBS",
                                                 "Precipitation" = "PREC"))
                      ),
                      column(width = 4,
                             sliderInput("swe_year", "Select Year:",
                                       min = 1982, max = 2024, value = 2024,
                                       step = 1, sep = "")
                      ),
                      column(width = 4,
                             selectInput("swe_analysis", "Select Analysis:",
                                       choices = c("Spatial Distribution" = "spatial",
                                                 "Monthly Statistics" = "monthly",
                                                 "Elevation Analysis" = "elevation"))
                      )
                    ),
                    leafletOutput("snotel_map", height = "600px")
                )
              )
      ),
      
      # Precipitation Tab
      tabItem(tabName = "precip",
              fluidRow(
                div(class = "variable-info",
                    h4("Precipitation Information"),
                    p("Precipitation data from VIC (Variable Infiltration Capacity) model."),
                    p("Includes total precipitation and rainfall components."),
                    p("Spatial resolution: 4km, Temporal resolution: Daily")
                )
              ),
              fluidRow(
                box(width = 12, title = "VIC Precipitation",
                    fluidRow(
                      column(width = 6,
                             selectInput("precip_type", "Select Type:",
                                       choices = c("Total Precipitation" = "total",
                                                 "Rainfall" = "rainfall"))
                      ),
                      column(width = 6,
                             sliderInput("precip_year", "Select Year:",
                                       min = 1982, max = 2024, value = 2024,
                                       step = 1, sep = "")
                      )
                    ),
                    plotlyOutput("precip_map", height = "600px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Series Analysis",
                    plotlyOutput("precip_timeseries", height = "400px")),
                box(width = 6, title = "Monthly Statistics",
                    plotlyOutput("precip_monthly", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "Annual Trends",
                    plotlyOutput("precip_trends", height = "400px"))
              )
      ),
      
      # Soil Moisture Tab
      tabItem(tabName = "soil",
              fluidRow(
                div(class = "variable-info",
                    h4("Soil Moisture Information"),
                    p("Comparison of soil moisture from VIC model and SMAP observations."),
                    p("Includes both surface and root zone soil moisture."),
                    p("Helps validate model performance and understand soil water dynamics.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Soil Moisture Comparison",
                    fluidRow(
                      column(width = 3,
                             selectInput("soil_source", "Select Data Source:",
                                       choices = c("VIC Model" = "vic",
                                                 "SMAP Surface" = "smap_surface",
                                                 "SMAP Root Zone" = "smap_rootzone",
                                                 "SMAP Profile" = "smap_profile"))
                      ),
                      column(width = 3,
                             sliderInput("soil_year", "Select Year:",
                                       min = 2000, max = 2024, value = 2024,
                                       step = 1, sep = "")
                      ),
                      column(width = 3,
                             selectInput("soil_analysis", "Select Analysis:",
                                       choices = c("Spatial Distribution" = "spatial",
                                                 "Time Series" = "timeseries",
                                                 "Monthly Statistics" = "monthly",
                                                 "Anomaly Analysis" = "anomaly",
                                                 "Model Validation" = "validation"))
                      )
                    ),
                    # Add info tiles for year selection
                    fluidRow(
                      infoBox(
                        title = "VIC Model Data Range",
                        value = "1982-2024",
                        icon = icon("calendar"),
                        color = "maroon",
                        width = 3
                      ),
                      infoBox(
                        title = "SMAP Data Range",
                        value = "2015-2024",
                        icon = icon("satellite"),
                        color = "yellow",
                        width = 3
                      ),
                      infoBox(
                        title = "Common Data Range",
                        value = "2015-2024",
                        icon = icon("chart-line"),
                        color = "green",
                        width = 3
                      ),
                      infoBox(
                        title = "Selected Year",
                        value = textOutput("selected_soil_year"),
                        icon = icon("check-circle"),
                        color = "blue",
                        width = 3
                      )
                    ),
                    plotlyOutput("soil_map", height = "600px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Series Comparison",
                    plotlyOutput("soil_timeseries", height = "400px")),
                box(width = 6, title = "Monthly Statistics",
                    plotlyOutput("soil_monthly", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Anomaly Analysis",
                    plotlyOutput("soil_anomaly", height = "400px")),
                box(width = 6, title = "Model Validation",
                    plotlyOutput("soil_validation", height = "400px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Add resource path for images
  addResourcePath("images", "/Users/praddy5/Desktop/Dashboard/images")
  
  # Load basin shapefile
  basin_data <- reactive({
    if (check_processed_data("basin")) {
      return(load_processed_data("basin"))
    }
    
    basin_sf <- st_read("data/Analysis_Basin_Shapefiles/basins_all_unique.shp")
    save_processed_data(basin_sf, "basin")
    return(basin_sf)
  })
  
  # Load HUC10 shapefile
  huc10_data <- reactive({
    if (check_processed_data("huc10")) {
      return(load_processed_data("huc10"))
    }
    
    huc10_sf <- st_read("data/huc10s/huc10.shp")
    save_processed_data(huc10_sf, "huc10")
    return(huc10_sf)
  })
  
  # Load SNOTEL data with enhanced processing
  snotel_data <- reactive({
    if (check_processed_data("snotel")) {
      return(load_processed_data("snotel"))
    }
    
    snotel_meta <- read.csv("data/snotel/snotel_metadata.csv")
    snotel_values <- read.csv("data/snotel/snotel_values.csv")
    
    # Merge metadata with values
    snotel_data <- merge(snotel_meta, snotel_values, by = "station_id")
    
    # Convert dates
    snotel_data$date <- as.Date(snotel_data$date)
    
    save_processed_data(snotel_data, "snotel")
    return(snotel_data)
  })
  
  # Static image outputs with error handling
  output$basin_map <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/colorado_river_basin_map.png"
    if (file.exists(img_path)) {
      div(class = "map-container",
          tags$img(src = "images/colorado_river_basin_map.png", 
                   alt = "Colorado River Basin Map",
                   style = "width: 100%; height: auto; max-height: 800px; object-fit: contain; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"),
          div(class = "map-caption",
              p("The Colorado River Basin spans across seven U.S. states, covering approximately 246,000 square miles."),
              p("Key features include Lake Powell and Lake Mead reservoirs.")
          )
      )
    } else {
      div(class = "alert alert-warning",
          tags$h4("Map Preview"),
          tags$p("The Colorado River Basin spans across seven U.S. states, covering approximately 246,000 square miles."),
          tags$p("Key features include Lake Powell and Lake Mead reservoirs."),
          style = "text-align: center; padding: 20px;"
      )
    }
  })
  
  output$huc10_map <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/huc10_map.png"
    if (file.exists(img_path)) {
      tags$img(src = "images/huc10_map.png",
               alt = "HUC10 Map",
               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$huc10_area <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/huc10_area.png"
    if (file.exists(img_path)) {
      tags$img(src = "images/huc10_area.png",
               alt = "HUC10 Area",
               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$monthly_trends <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/monthly_trends.png"
    if (file.exists(img_path)) {
      tags$img(src = "images/monthly_trends.png",
               alt = "Monthly Trends",
               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;")
    }
  })
  
  output$snotel_map <- renderLeaflet({
    data <- snotel_data()
    if (is.null(data)) return(NULL)
    
    # Filter data for selected year and metric
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    filtered_data <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      group_by(station_id) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(data %>% select(station_id, latitude, longitude, elevation, name) %>% distinct(),
                by = "station_id")
    
    # Create color palette based on values
    pal <- colorNumeric(
      palette = "viridis",
      domain = filtered_data$mean_value
    )
    
    # Create popup content
    popup_content <- paste(
      "<strong>Station:</strong> ", filtered_data$name, "<br>",
      "<strong>Elevation:</strong> ", filtered_data$elevation, " m<br>",
      "<strong>Mean Value:</strong> ", round(filtered_data$mean_value, 2), "<br>",
      "<strong>Max Value:</strong> ", round(filtered_data$max_value, 2), "<br>",
      "<strong>Min Value:</strong> ", round(filtered_data$min_value, 2)
    )
    
    # Create the map
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 6,
        color = ~pal(mean_value),
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~mean_value,
        title = paste("Mean", selected_metric),
        opacity = 1
      )
  })
  
  # Add CSS for map containers
  output$map_container_css <- renderUI({
    tags$style(HTML("
      .map-container {
        background-color: white;
        border-radius: 4px;
        padding: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
        margin-bottom: 20px;
      }
      .map-container img {
        max-width: 100%;
        height: auto;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        transition: transform 0.3s ease;
      }
      .map-container img:hover {
        transform: scale(1.02);
      }
      .map-caption {
        margin-top: 15px;
        padding: 10px;
        background-color: #f8f9fa;
        border-radius: 4px;
        text-align: center;
      }
      .map-caption p {
        margin: 5px 0;
        color: #333;
      }
      .alert-warning {
        background-color: #fff3cd;
        border-color: #ffeeba;
        color: #856404;
        border-radius: 4px;
        margin: 15px 0;
        padding: 15px;
      }
      .alert-warning h4 {
        color: #856404;
        margin-bottom: 10px;
      }
      .alert-warning p {
        margin-bottom: 5px;
      }
    "))
  })
  
  # Load VIC data with memory optimization
  vic_data <- reactive({
    selected_year <- input$vic_year
    
    if (check_processed_data("vic", selected_year)) {
      return(load_processed_data("vic", selected_year))
    }
    
    tryCatch({
      file_path <- paste0("data/VIC_outputs/CRB_PRISM_Calibrated.", selected_year, "-01-01.nc")
      
      if (!file.exists(file_path)) {
        print(paste("VIC data file not found:", file_path))
        return(NULL)
      }
      
      # Open the netCDF file
      nc <- nc_open(file_path)
      on.exit(nc_close(nc))
      
      # Get dimensions
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time <- ncvar_get(nc, "time")
      
      # Get selected variable
      var_name <- input$vic_variable
      if (is.null(var_name)) return(NULL)
      
      # Handle soil moisture differently due to its additional dimension
      if (var_name == "OUT_SOIL_MOIST") {
        start <- c(1, 1, 1, 1)
        count <- c(1, -1, -1, -1)
        data <- ncvar_get(nc, var_name, start = start, count = count)
        data <- drop(data)
      } else {
        data <- ncvar_get(nc, var_name)
      }
      
      if (is.null(data)) return(NULL)
      
      # Convert time to dates
      dates <- as.Date(time, origin = "0001-01-01")
      
      result <- list(
        data = data,
        lon = lon,
        lat = lat,
        dates = dates,
        var_name = var_name
      )
      
      save_processed_data(result, "vic", selected_year)
      return(result)
    }, error = function(e) {
      print(paste("Error loading VIC data:", e$message))
      return(NULL)
    })
  })
  
  # Load SMAP data
  smap_data <- reactive({
    if (check_processed_data("smap")) {
      return(load_processed_data("smap"))
    }
    
    tryCatch({
      nc <- nc_open("data/SPL4SMGP.007_9km_aid0001.nc")
      on.exit(nc_close(nc))
      
      # Get dimensions
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      time <- ncvar_get(nc, "time")
      day_period <- ncvar_get(nc, "day_period")
      
      # Get soil moisture data
      sm_surface <- ncvar_get(nc, "Geophysical_Data_sm_surface")
      sm_rootzone <- ncvar_get(nc, "Geophysical_Data_sm_rootzone")
      sm_profile <- ncvar_get(nc, "Geophysical_Data_sm_profile")
      
      # Get data attributes
      surface_attrs <- ncatt_get(nc, "Geophysical_Data_sm_surface")
      rootzone_attrs <- ncatt_get(nc, "Geophysical_Data_sm_rootzone")
      profile_attrs <- ncatt_get(nc, "Geophysical_Data_sm_profile")
      
      # Convert time to dates
      dates <- as.Date(time, origin = "2000-01-01")
      
      # Calculate spatial means for each time step
      surface_means <- apply(sm_surface[,,,1], 3, mean, na.rm = TRUE)
      rootzone_means <- apply(sm_rootzone[,,,1], 3, mean, na.rm = TRUE)
      profile_means <- apply(sm_profile[,,,1], 3, mean, na.rm = TRUE)
      
      result <- list(
        surface = sm_surface,
        rootzone = sm_rootzone,
        profile = sm_profile,
        lon = lon,
        lat = lat,
        dates = dates,
        day_period = day_period,
        surface_attrs = surface_attrs,
        rootzone_attrs = rootzone_attrs,
        profile_attrs = profile_attrs,
        surface_means = surface_means,
        rootzone_means = rootzone_means,
        profile_means = profile_means
      )
      
      save_processed_data(result, "smap")
      return(result)
    }, error = function(e) {
      print(paste("Error loading SMAP data:", e$message))
      return(NULL)
    })
  })
  
  # Load GRACE data
  grace_data <- reactive({
    if (check_processed_data("grace")) {
      return(load_processed_data("grace"))
    }
    
    nc <- nc_open("data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc")
    on.exit(nc_close(nc))
    
    # Get dimensions
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    
    # Get TWS data and uncertainty
    tws_data <- ncvar_get(nc, "lwe_thickness")
    uncertainty <- ncvar_get(nc, "uncertainty")
    
    # Convert time to dates
    dates <- as.Date(time, origin = "2002-01-01")
    
    result <- list(
      tws = tws_data,
      uncertainty = uncertainty,
      lon = lon,
      lat = lat,
      dates = dates
    )
    
    save_processed_data(result, "grace")
    return(result)
  })
  
  # Load Precipitation data
  precip_data <- reactive({
    nc <- nc_open("data/CRB_PRISM_Calibrated.2024-01-01.nc")
    on.exit(nc_close(nc))
    
    # Get dimensions
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    
    # Get precipitation data
    precip <- ncvar_get(nc, "OUT_PREC")
    rainf <- ncvar_get(nc, "OUT_RAINF")
    
    # Convert time to dates
    dates <- as.Date(time, origin = "0001-01-01")
    
    return(list(
      total = precip,
      rainfall = rainf,
      lon = lon,
      lat = lat,
      dates = dates
    ))
  })
  
  # VIC Map
  output$vic_map <- renderUI({
    var_name <- input$vic_variable
    if (is.null(var_name)) return(NULL)
    
    # Map image paths based on variable
    img_paths <- list(
      "OUT_PREC" = "images/vic_precipitation_map.png",
      "OUT_RAINF" = "images/vic_precipitation_map.png",
      "OUT_EVAP" = "images/vic_evapotranspiration_map.png",
      "OUT_RUNOFF" = "images/vic_runoff_map.png",
      "OUT_BASEFLOW" = "images/vic_baseflow_map.png",
      "OUT_SOIL_MOIST" = "images/vic_soil_moisture_layer_1_map.png",
      "OUT_SWE" = "images/vic_snow_water_equivalent_map.png"
    )
    
    img_path <- img_paths[[var_name]]
    if (!is.null(img_path) && file.exists(img_path)) {
      tags$img(src = img_path, 
               alt = paste("VIC Model", var_name, "Map"),
               style = "width: 100%; height: auto; max-height: 500px; object-fit: contain;")
    } else {
      div(class = "alert alert-warning",
          tags$h4("VIC Model Map"),
          tags$p("Spatial distribution of selected variable."),
          tags$p("Shows the geographical pattern across the basin."),
          style = "text-align: center; padding: 20px;")
    }
  })
  
  # SMAP Map
  output$smap_map <- renderPlotly({
    data <- smap_data()
    basin <- basin_data()
    layer <- input$smap_layer
    selected_date <- input$smap_date
    
    if (is.null(data) || is.null(basin)) {
      print("SMAP or basin data is NULL")
      return(NULL)
    }
    
    # Constrain date to available range
    min_date <- min(data$dates)
    max_date <- max(data$dates)
    if (selected_date < min_date) selected_date <- min_date
    if (selected_date > max_date) selected_date <- max_date
    
    # Get the time index for selected date
    time_index <- which.min(abs(data$dates - selected_date))
    if (length(time_index) == 0) return(NULL)
    
    # Get the data for selected layer
    if (layer == "surface") {
      last_step <- data$surface[,,time_index,1]
      title <- "SMAP Surface Soil Moisture"
    } else if (layer == "rootzone") {
      last_step <- data$rootzone[,,time_index,1]
      title <- "SMAP Root Zone Soil Moisture"
    } else {
      last_step <- data$profile[,,time_index,1]
      title <- "SMAP Profile Soil Moisture"
    }
    
    # Create spatial data frame
    spatial_df <- expand.grid(
      lon = data$lon,
      lat = data$lat
    )
    spatial_df$value <- as.vector(last_step)
    
    # Remove NA values
    spatial_df <- spatial_df[!is.na(spatial_df$value),]
    
    # Create the plot
    p <- ggplot() +
      # Add basin boundary
      geom_sf(data = basin, fill = NA, color = asu_maroon, size = 1) +
      # Add SMAP data
      geom_tile(data = spatial_df, aes(x = lon, y = lat, fill = value)) +
      scale_fill_viridis_c(option = "viridis", na.value = "transparent",
                          name = "Soil Moisture (m³/m³)",
                          limits = c(0, 0.9)) +
      labs(
        title = paste(title, "- Colorado River Basin"),
        subtitle = paste("Date:", format(selected_date, "%Y-%m-%d")),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # GRACE Map
  output$grace_map <- renderPlotly({
    data <- grace_data()
    basin <- basin_data()
    
    # Debug information
    print("Loading GRACE data...")
    if (is.null(data)) {
      print("GRACE data is NULL")
      return(NULL)
    }
    if (is.null(basin)) {
      print("Basin data is NULL")
      return(NULL)
    }
    
    # Get the last time step
    last_step <- data$tws[,,dim(data$tws)[3]]
    print(paste("Last step dimensions:", dim(last_step)))
    
    # Create spatial data frame
    spatial_df <- expand.grid(
      lon = data$lon,
      lat = data$lat
    )
    spatial_df$value <- as.vector(last_step)
    
    # Remove NA values
    spatial_df <- spatial_df[!is.na(spatial_df$value),]
    print(paste("Number of valid points:", nrow(spatial_df)))
    
    # Create the plot
    p <- ggplot() +
      # Add basin boundary
      geom_sf(data = basin, fill = NA, color = asu_maroon, size = 1) +
      # Add TWS data
      geom_tile(data = spatial_df, aes(x = lon, y = lat, fill = value)) +
      scale_fill_viridis_c(option = "viridis", na.value = "transparent",
                          name = "TWS (cm)") +
      labs(
        title = "GRACE Terrestrial Water Storage - Colorado River Basin",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    # Convert to plotly
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # GRACE Time Series with Trend Analysis
  output$grace_timeseries <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    uncertainty_means <- apply(data$uncertainty, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means,
      Uncertainty = uncertainty_means
    )
    
    # Calculate trend line
    trend_line <- lm(TWS ~ as.numeric(Date), data = ts_df)
    ts_df$Trend <- predict(trend_line)
    
    # Calculate seasonal decomposition
    ts_data <- ts(ts_df$TWS, frequency = 12)
    decomp <- stl(ts_data, s.window = "periodic")
    ts_df$Seasonal <- as.vector(decomp$time.series[,"seasonal"])
    ts_df$Trend_Decomp <- as.vector(decomp$time.series[,"trend"])
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~TWS, name = 'TWS', type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~Date, y = ~Uncertainty, name = 'Uncertainty', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash')) %>%
      add_trace(x = ~Date, y = ~Trend, name = 'Trend', type = 'scatter', mode = 'lines',
                line = list(dash = 'dot')) %>%
      layout(
        title = "GRACE TWS Time Series with Trend Analysis",
        xaxis = list(title = "Date"),
        yaxis = list(title = "TWS (cm)"),
        legend = list(x = 0.1, y = 0.9)
      )
  })
  
  # GRACE Seasonal Analysis
  output$grace_seasonal <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means
    )
    
    # Add month and year columns
    ts_df$Month <- month(ts_df$Date, label = TRUE)
    ts_df$Year <- year(ts_df$Date)
    
    # Calculate monthly statistics
    monthly_stats <- ts_df %>%
      group_by(Month) %>%
      summarise(
        Mean = mean(TWS, na.rm = TRUE),
        Min = min(TWS, na.rm = TRUE),
        Max = max(TWS, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(monthly_stats) %>%
      add_trace(x = ~Month, y = ~Mean, type = 'scatter', mode = 'lines+markers',
                name = 'Mean', line = list(width = 2)) %>%
      add_trace(x = ~Month, y = ~Min, type = 'scatter', mode = 'lines',
                name = 'Min', line = list(dash = 'dash', width = 1)) %>%
      add_trace(x = ~Month, y = ~Max, type = 'scatter', mode = 'lines',
                name = 'Max', line = list(dash = 'dash', width = 1)) %>%
      layout(
        title = "GRACE TWS Seasonal Analysis",
        xaxis = list(title = "Month"),
        yaxis = list(title = "TWS (cm)"),
        showlegend = TRUE,
        legend = list(x = 0.1, y = 0.9)
      )
  })

  # GRACE Anomaly Analysis
  output$grace_anomaly <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means
    )
    
    # Calculate monthly climatology
    ts_df$Month <- month(ts_df$Date)
    monthly_clim <- ts_df %>%
      group_by(Month) %>%
      summarise(Climatology = mean(TWS, na.rm = TRUE))
    
    # Calculate anomalies
    ts_df <- ts_df %>%
      left_join(monthly_clim, by = "Month") %>%
      mutate(Anomaly = TWS - Climatology)
    
    plot_ly(ts_df, x = ~Date, y = ~Anomaly, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "GRACE TWS Anomalies",
        xaxis = list(title = "Date"),
        yaxis = list(title = "TWS Anomaly (cm)"),
        shapes = list(
          list(type = "line", x0 = min(ts_df$Date), x1 = max(ts_df$Date),
               y0 = 0, y1 = 0, line = list(dash = "dash"))
        )
      )
  })
  
  # Soil Moisture Map
  output$soil_map <- renderPlotly({
    vic_data <- vic_data()
    smap_data <- smap_data()
    basin <- basin_data()
    source <- input$soil_source
    selected_year <- input$soil_year
    
    if (is.null(vic_data) || is.null(smap_data) || is.null(basin)) return(NULL)
    
    # Get available date ranges
    vic_dates <- vic_data$dates
    smap_dates <- smap_data$dates
    
    # Filter dates for selected year
    vic_dates <- vic_dates[year(vic_dates) == selected_year]
    smap_dates <- smap_dates[year(smap_dates) == selected_year]
    
    # Constrain selected date to available range
    if (source == "vic") {
      if (length(vic_dates) == 0) {
        print(paste("No VIC data found for year", selected_year))
        return(NULL)
      }
      selected_date <- max(vic_dates)  # Use last available date of the year
    } else {
      if (length(smap_dates) == 0) {
        print(paste("No SMAP data found for year", selected_year))
        return(NULL)
      }
      selected_date <- max(smap_dates)  # Use last available date of the year
    }
    
    # Get data based on selected source
    if (source == "vic") {
      # Get VIC soil moisture
      time_index <- which.min(abs(vic_dates - selected_date))
      if (length(time_index) == 0) {
        print("No matching VIC data found for selected date")
        return(NULL)
      }
      data <- vic_data$data[,,time_index]
      title <- "VIC Model Soil Moisture"
    } else {
      # Get SMAP data
      time_index <- which.min(abs(smap_dates - selected_date))
      if (length(time_index) == 0) {
        print("No matching SMAP data found for selected date")
        return(NULL)
      }
      
      if (source == "smap_surface") {
        data <- smap_data$surface[,,time_index,1]
        title <- "SMAP Surface Soil Moisture"
      } else if (source == "smap_rootzone") {
        data <- smap_data$rootzone[,,time_index,1]
        title <- "SMAP Root Zone Soil Moisture"
      } else {
        data <- smap_data$profile[,,time_index,1]
        title <- "SMAP Profile Soil Moisture"
      }
    }
    
    # Create spatial data frame
    spatial_df <- expand.grid(
      lon = if (source == "vic") vic_data$lon else smap_data$lon,
      lat = if (source == "vic") vic_data$lat else smap_data$lat
    )
    spatial_df$value <- as.vector(data)
    
    # Remove NA values
    spatial_df <- spatial_df[!is.na(spatial_df$value),]
    
    # Create the plot
    p <- ggplot() +
      # Add basin boundary
      geom_sf(data = basin, fill = NA, color = asu_maroon, size = 1) +
      # Add soil moisture data
      geom_tile(data = spatial_df, aes(x = lon, y = lat, fill = value)) +
      scale_fill_viridis_c(option = "viridis", na.value = "transparent",
                          name = "Soil Moisture (m³/m³)") +
      labs(
        title = paste(title, "- Colorado River Basin"),
        subtitle = paste("Year:", selected_year),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # Soil Moisture Time Series
  output$soil_timeseries <- renderPlotly({
    vic_data <- vic_data()
    smap_data <- smap_data()
    
    if (is.null(vic_data) || is.null(smap_data)) return(NULL)
    
    # Get common date range
    common_dates <- intersect(vic_data$dates, smap_data$dates)
    if (length(common_dates) == 0) return(NULL)
    
    # Filter data to common dates
    vic_indices <- which(vic_data$dates %in% common_dates)
    smap_indices <- which(smap_data$dates %in% common_dates)
    
    # Calculate spatial means for common dates
    vic_means <- apply(vic_data$data[,,vic_indices], 3, mean, na.rm = TRUE)
    smap_surface_means <- apply(smap_data$surface[,,smap_indices,1], 3, mean, na.rm = TRUE)
    smap_rootzone_means <- apply(smap_data$rootzone[,,smap_indices,1], 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = common_dates,
      VIC = vic_means,
      SMAP_Surface = smap_surface_means,
      SMAP_Rootzone = smap_rootzone_means
    )
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~VIC, name = 'VIC Model', type = 'scatter', mode = 'lines',
                line = list(color = asu_maroon, width = 2)) %>%
      add_trace(x = ~Date, y = ~SMAP_Surface, name = 'SMAP Surface', type = 'scatter', mode = 'lines',
                line = list(color = asu_gold, width = 2)) %>%
      add_trace(x = ~Date, y = ~SMAP_Rootzone, name = 'SMAP Root Zone', type = 'scatter', mode = 'lines',
                line = list(color = asu_dark_maroon, width = 2)) %>%
      layout(
        title = "Soil Moisture Time Series Comparison",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Soil Moisture (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Soil Moisture Monthly Statistics
  output$soil_monthly <- renderPlotly({
    vic_data <- vic_data()
    smap_data <- smap_data()
    
    if (is.null(vic_data) || is.null(smap_data)) return(NULL)
    
    # Get common date range
    common_dates <- intersect(vic_data$dates, smap_data$dates)
    if (length(common_dates) == 0) {
      print("No common dates found between VIC and SMAP data")
      return(NULL)
    }
    
    # Filter data to common dates
    vic_indices <- which(vic_data$dates %in% common_dates)
    smap_indices <- which(smap_data$dates %in% common_dates)
    
    # Calculate spatial means for common dates
    vic_means <- apply(vic_data$data[,,vic_indices], 3, mean, na.rm = TRUE)
    smap_surface_means <- apply(smap_data$surface[,,smap_indices,1], 3, mean, na.rm = TRUE)
    smap_rootzone_means <- apply(smap_data$rootzone[,,smap_indices,1], 3, mean, na.rm = TRUE)
    
    # Create monthly statistics data frame
    monthly_df <- data.frame(
      Date = common_dates,
      VIC = vic_means,
      SMAP_Surface = smap_surface_means,
      SMAP_Rootzone = smap_rootzone_means
    ) %>%
      mutate(Month = month(Date, label = TRUE)) %>%
      group_by(Month) %>%
      summarise(
        VIC_Mean = mean(VIC, na.rm = TRUE),
        SMAP_Surface_Mean = mean(SMAP_Surface, na.rm = TRUE),
        SMAP_Rootzone_Mean = mean(SMAP_Rootzone, na.rm = TRUE),
        VIC_SD = sd(VIC, na.rm = TRUE),
        SMAP_Surface_SD = sd(SMAP_Surface, na.rm = TRUE),
        SMAP_Rootzone_SD = sd(SMAP_Rootzone, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Ensure all months are present
    all_months <- data.frame(Month = month(1:12, label = TRUE))
    monthly_df <- left_join(all_months, monthly_df, by = "Month")
    
    # Create the plot with error bars
    plot_ly(monthly_df) %>%
      add_trace(x = ~Month, y = ~VIC_Mean, name = 'VIC Model', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_maroon, width = 2),
                marker = list(color = asu_maroon, size = 8),
                error_y = list(type = "data", array = ~VIC_SD, color = asu_maroon)) %>%
      add_trace(x = ~Month, y = ~SMAP_Surface_Mean, name = 'SMAP Surface', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_gold, width = 2),
                marker = list(color = asu_gold, size = 8),
                error_y = list(type = "data", array = ~SMAP_Surface_SD, color = asu_gold)) %>%
      add_trace(x = ~Month, y = ~SMAP_Rootzone_Mean, name = 'SMAP Root Zone', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_dark_maroon, width = 2),
                marker = list(color = asu_dark_maroon, size = 8),
                error_y = list(type = "data", array = ~SMAP_Rootzone_SD, color = asu_dark_maroon)) %>%
      layout(
        title = "Monthly Soil Moisture Statistics",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Soil Moisture (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Soil Moisture Anomaly Analysis
  output$soil_anomaly <- renderPlotly({
    vic_data <- vic_data()
    smap_data <- smap_data()
    
    if (is.null(vic_data) || is.null(smap_data)) return(NULL)
    
    # Get common date range
    common_dates <- intersect(vic_data$dates, smap_data$dates)
    if (length(common_dates) == 0) return(NULL)
    
    # Filter data to common dates
    vic_indices <- which(vic_data$dates %in% common_dates)
    smap_indices <- which(smap_data$dates %in% common_dates)
    
    # Calculate spatial means for common dates
    vic_means <- apply(vic_data$data[,,vic_indices], 3, mean, na.rm = TRUE)
    smap_surface_means <- apply(smap_data$surface[,,smap_indices,1], 3, mean, na.rm = TRUE)
    smap_rootzone_means <- apply(smap_data$rootzone[,,smap_indices,1], 3, mean, na.rm = TRUE)
    
    # Create anomaly data frame
    anomaly_df <- data.frame(
      Date = common_dates,
      VIC = vic_means,
      SMAP_Surface = smap_surface_means,
      SMAP_Rootzone = smap_rootzone_means
    ) %>%
      mutate(Month = month(Date)) %>%
      group_by(Month) %>%
      mutate(
        VIC_Clim = mean(VIC, na.rm = TRUE),
        SMAP_Surface_Clim = mean(SMAP_Surface, na.rm = TRUE),
        SMAP_Rootzone_Clim = mean(SMAP_Rootzone, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        VIC_Anomaly = VIC - VIC_Clim,
        SMAP_Surface_Anomaly = SMAP_Surface - SMAP_Surface_Clim,
        SMAP_Rootzone_Anomaly = SMAP_Rootzone - SMAP_Rootzone_Clim
      )
    
    plot_ly(anomaly_df) %>%
      add_trace(x = ~Date, y = ~VIC_Anomaly, name = 'VIC Model', type = 'scatter', mode = 'lines',
                line = list(color = asu_maroon, width = 2)) %>%
      add_trace(x = ~Date, y = ~SMAP_Surface_Anomaly, name = 'SMAP Surface', type = 'scatter', mode = 'lines',
                line = list(color = asu_gold, width = 2)) %>%
      add_trace(x = ~Date, y = ~SMAP_Rootzone_Anomaly, name = 'SMAP Root Zone', type = 'scatter', mode = 'lines',
                line = list(color = asu_dark_maroon, width = 2)) %>%
      layout(
        title = "Soil Moisture Anomalies",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Anomaly (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        shapes = list(
          list(type = "line", x0 = min(anomaly_df$Date), x1 = max(anomaly_df$Date),
               y0 = 0, y1 = 0, line = list(dash = "dash"))
        )
      )
  })
  
  # Soil Moisture Model Validation
  output$soil_validation <- renderPlotly({
    vic_data <- vic_data()
    smap_data <- smap_data()
    
    if (is.null(vic_data) || is.null(smap_data)) return(NULL)
    
    # Get common date range
    common_dates <- intersect(vic_data$dates, smap_data$dates)
    if (length(common_dates) == 0) return(NULL)
    
    # Filter data to common dates
    vic_indices <- which(vic_data$dates %in% common_dates)
    smap_indices <- which(smap_data$dates %in% common_dates)
    
    # Calculate spatial means for common dates
    vic_means <- apply(vic_data$data[,,vic_indices], 3, mean, na.rm = TRUE)
    smap_surface_means <- apply(smap_data$surface[,,smap_indices,1], 3, mean, na.rm = TRUE)
    smap_rootzone_means <- apply(smap_data$rootzone[,,smap_indices,1], 3, mean, na.rm = TRUE)
    
    # Create validation data frame
    validation_df <- data.frame(
      VIC = vic_means,
      SMAP_Surface = smap_surface_means,
      SMAP_Rootzone = smap_rootzone_means
    )
    
    # Calculate correlation coefficients
    cor_surface <- cor(validation_df$VIC, validation_df$SMAP_Surface, use = "complete.obs")
    cor_rootzone <- cor(validation_df$VIC, validation_df$SMAP_Rootzone, use = "complete.obs")
    
    plot_ly(validation_df) %>%
      add_trace(x = ~VIC, y = ~SMAP_Surface, name = 'Surface', type = 'scatter', mode = 'markers',
                marker = list(color = asu_maroon)) %>%
      add_trace(x = ~VIC, y = ~SMAP_Rootzone, name = 'Root Zone', type = 'scatter', mode = 'markers',
                marker = list(color = asu_gold)) %>%
      add_trace(x = ~VIC, y = ~VIC, name = '1:1 Line', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash', color = asu_dark_maroon)) %>%
      layout(
        title = paste("Model Validation<br>Surface r =", round(cor_surface, 2),
                     "Root Zone r =", round(cor_rootzone, 2)),
        xaxis = list(title = "VIC Model (m³/m³)"),
        yaxis = list(title = "SMAP Observations (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # VIC Time Series
  output$vic_timeseries <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/vic_time_series.png"
    if (file.exists(img_path)) {
      tags$img(src = "images/vic_time_series.png", 
               alt = "VIC Model Time Series",
               style = "width: 100%; height: auto; max-height: 400px; object-fit: contain;")
    } else {
      div(class = "alert alert-warning",
          tags$h4("VIC Model Time Series"),
          tags$p("Time series visualization of VIC model outputs."),
          tags$p("Shows daily variations in selected variables."),
          style = "text-align: center; padding: 20px;")
    }
  })
  
  # VIC Monthly Statistics
  output$vic_monthly <- renderUI({
    img_path <- "/Users/praddy5/Desktop/Dashboard/images/vic_monthly_stats.png"
    if (file.exists(img_path)) {
      tags$img(src = "images/vic_monthly_stats.png", 
               alt = "VIC Model Monthly Statistics",
               style = "width: 100%; height: auto; max-height: 300px; object-fit: contain;")
    } else {
      div(class = "alert alert-warning",
          tags$h4("VIC Model Monthly Statistics"),
          tags$p("Monthly statistics for VIC model outputs."),
          tags$p("Shows mean, min, and max values for each month."),
          style = "text-align: center; padding: 20px;")
    }
  })
  
  # SMAP Time Series
  output$smap_timeseries <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Surface = data$surface_means,
      Rootzone = data$rootzone_means,
      Profile = data$profile_means
    )
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Surface, name = 'Surface (0-5 cm)', type = 'scatter', mode = 'lines',
                line = list(color = asu_maroon)) %>%
      add_trace(x = ~Date, y = ~Rootzone, name = 'Root Zone (0-100 cm)', type = 'scatter', mode = 'lines',
                line = list(color = asu_gold)) %>%
      add_trace(x = ~Date, y = ~Profile, name = 'Profile (0 cm to bedrock)', type = 'scatter', mode = 'lines',
                line = list(color = asu_dark_maroon)) %>%
      layout(
        title = "SMAP Soil Moisture Time Series",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Soil Moisture (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SMAP Layer Comparison
  output$smap_comparison <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Create comparison data frame
    comp_df <- data.frame(
      Surface = data$surface_means,
      Rootzone = data$rootzone_means,
      Profile = data$profile_means
    )
    
    plot_ly(comp_df) %>%
      add_trace(x = ~Surface, y = ~Rootzone, name = 'Surface vs Rootzone', type = 'scatter', mode = 'markers',
                marker = list(color = asu_maroon)) %>%
      add_trace(x = ~Surface, y = ~Profile, name = 'Surface vs Profile', type = 'scatter', mode = 'markers',
                marker = list(color = asu_gold)) %>%
      add_trace(x = ~Rootzone, y = ~Profile, name = 'Rootzone vs Profile', type = 'scatter', mode = 'markers',
                marker = list(color = asu_dark_maroon)) %>%
      layout(
        title = "SMAP Layer Comparison",
        xaxis = list(title = "Soil Moisture (m³/m³)"),
        yaxis = list(title = "Soil Moisture (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SMAP Seasonal Analysis
  output$smap_seasonal <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Surface = data$surface_means,
      Rootzone = data$rootzone_means,
      Profile = data$profile_means
    )
    
    # Add month column
    ts_df$Month <- month(ts_df$Date, label = TRUE)
    
    # Calculate monthly statistics
    monthly_stats <- ts_df %>%
      group_by(Month) %>%
      summarise(
        Surface_Mean = mean(Surface, na.rm = TRUE),
        Rootzone_Mean = mean(Rootzone, na.rm = TRUE),
        Profile_Mean = mean(Profile, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(monthly_stats) %>%
      add_trace(x = ~Month, y = ~Surface_Mean, name = 'Surface', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_maroon)) %>%
      add_trace(x = ~Month, y = ~Rootzone_Mean, name = 'Root Zone', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_gold)) %>%
      add_trace(x = ~Month, y = ~Profile_Mean, name = 'Profile', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_dark_maroon)) %>%
      layout(
        title = "Monthly Average Soil Moisture",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Soil Moisture (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SMAP Anomaly Analysis
  output$smap_anomaly <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Surface = data$surface_means,
      Rootzone = data$rootzone_means,
      Profile = data$profile_means
    )
    
    # Calculate monthly climatology
    ts_df$Month <- month(ts_df$Date)
    monthly_clim <- ts_df %>%
      group_by(Month) %>%
      summarise(
        Surface_Clim = mean(Surface, na.rm = TRUE),
        Rootzone_Clim = mean(Rootzone, na.rm = TRUE),
        Profile_Clim = mean(Profile, na.rm = TRUE)
      )
    
    # Calculate anomalies
    ts_df <- ts_df %>%
      left_join(monthly_clim, by = "Month") %>%
      mutate(
        Surface_Anomaly = Surface - Surface_Clim,
        Rootzone_Anomaly = Rootzone - Rootzone_Clim,
        Profile_Anomaly = Profile - Profile_Clim
      )
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Surface_Anomaly, name = 'Surface', type = 'scatter', mode = 'lines',
                line = list(color = asu_maroon)) %>%
      add_trace(x = ~Date, y = ~Rootzone_Anomaly, name = 'Root Zone', type = 'scatter', mode = 'lines',
                line = list(color = asu_gold)) %>%
      add_trace(x = ~Date, y = ~Profile_Anomaly, name = 'Profile', type = 'scatter', mode = 'lines',
                line = list(color = asu_dark_maroon)) %>%
      layout(
        title = "Soil Moisture Anomalies",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Anomaly (m³/m³)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        shapes = list(
          list(type = "line", x0 = min(ts_df$Date), x1 = max(ts_df$Date),
               y0 = 0, y1 = 0, line = list(dash = "dash"))
        )
      )
  })
  
  # SWE Time Series
  output$swe_timeseries <- renderPlotly({
    data <- snotel_data()
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    if (is.null(data)) return(NULL)
    
    # Filter and process data
    ts_data <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      group_by(date) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(ts_data) %>%
      add_trace(x = ~date, y = ~mean_value, name = 'Mean', type = 'scatter', mode = 'lines',
                line = list(color = asu_maroon, width = 2)) %>%
      add_trace(x = ~date, y = ~max_value, name = 'Max', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash', color = asu_gold, width = 1)) %>%
      add_trace(x = ~date, y = ~min_value, name = 'Min', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash', color = asu_dark_maroon, width = 1)) %>%
      layout(
        title = paste("Daily", selected_metric, "Time Series"),
        xaxis = list(title = "Date"),
        yaxis = list(title = selected_metric),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SWE Monthly Statistics
  output$swe_monthly <- renderPlotly({
    data <- snotel_data()
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    if (is.null(data)) return(NULL)
    
    # Calculate monthly statistics
    monthly_stats <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      mutate(month = month(date, label = TRUE)) %>%
      group_by(month) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        max_value = max(value, na.rm = TRUE),
        min_value = min(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(monthly_stats) %>%
      add_trace(x = ~month, y = ~mean_value, name = 'Mean', type = 'scatter', mode = 'lines+markers',
                line = list(color = asu_maroon, width = 2)) %>%
      add_trace(x = ~month, y = ~max_value, name = 'Max', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash', color = asu_gold, width = 1)) %>%
      add_trace(x = ~month, y = ~min_value, name = 'Min', type = 'scatter', mode = 'lines',
                line = list(dash = 'dash', color = asu_dark_maroon, width = 1)) %>%
      layout(
        title = paste("Monthly", selected_metric, "Statistics"),
        xaxis = list(title = "Month"),
        yaxis = list(title = selected_metric),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SWE Elevation Analysis
  output$swe_elevation <- renderPlotly({
    data <- snotel_data()
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    if (is.null(data)) return(NULL)
    
    # Calculate elevation-based statistics
    elev_stats <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      group_by(elevation) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(elevation)
    
    plot_ly(elev_stats, x = ~elevation, y = ~mean_value, type = 'scatter', mode = 'markers+lines',
            marker = list(color = asu_maroon, size = 8),
            line = list(color = asu_gold, width = 2)) %>%
      layout(
        title = paste(selected_metric, "vs Elevation"),
        xaxis = list(title = "Elevation (m)"),
        yaxis = list(title = selected_metric),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # SWE Spatial Distribution
  output$swe_spatial <- renderPlotly({
    data <- snotel_data()
    selected_year <- input$swe_year
    selected_metric <- input$swe_metric
    
    if (is.null(data)) return(NULL)
    
    # Calculate spatial statistics
    spatial_stats <- data %>%
      filter(year(date) == selected_year,
             variable == selected_metric) %>%
      group_by(latitude, longitude) %>%
      summarise(
        mean_value = mean(value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(spatial_stats, x = ~longitude, y = ~latitude, z = ~mean_value,
            type = 'scatter3d', mode = 'markers',
            marker = list(
              color = ~mean_value,
              colorscale = 'Viridis',
              size = 8,
              showscale = TRUE
            )) %>%
      layout(
        title = paste("Spatial Distribution of", selected_metric),
        scene = list(
          xaxis = list(title = "Longitude"),
          yaxis = list(title = "Latitude"),
          zaxis = list(title = selected_metric)
        ),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Precipitation Map
  output$precip_map <- renderPlotly({
    data <- precip_data()
    basin <- basin_data()
    precip_type <- input$precip_type
    selected_year <- input$precip_year
    
    if (is.null(data) || is.null(basin)) return(NULL)
    
    # Get the time index for the selected year
    year_indices <- which(year(data$dates) == selected_year)
    if (length(year_indices) == 0) return(NULL)
    
    # Get the data for selected type
    if (precip_type == "total") {
      last_step <- data$total[,,max(year_indices)]
      title <- "Total Precipitation"
    } else {
      last_step <- data$rainfall[,,max(year_indices)]
      title <- "Rainfall"
    }
    
    # Create spatial data frame
    spatial_df <- expand.grid(
      lon = data$lon,
      lat = data$lat
    )
    spatial_df$value <- as.vector(last_step)
    
    # Remove NA values
    spatial_df <- spatial_df[!is.na(spatial_df$value),]
    
    # Create the plot
    p <- ggplot() +
      # Add basin boundary
      geom_sf(data = basin, fill = NA, color = asu_maroon, size = 1) +
      # Add precipitation data
      geom_tile(data = spatial_df, aes(x = lon, y = lat, fill = value)) +
      scale_fill_viridis_c(option = "viridis", na.value = "transparent",
                          name = "Precipitation (mm)") +
      labs(
        title = paste(title, "- Colorado River Basin"),
        subtitle = paste("Year:", selected_year),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    ggplotly(p) %>%
      layout(
        autosize = TRUE,
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # Precipitation Time Series
  output$precip_timeseries <- renderPlotly({
    data <- precip_data()
    precip_type <- input$precip_type
    selected_year <- input$precip_year
    
    if (is.null(data)) return(NULL)
    
    # Get the time index for the selected year
    year_indices <- which(year(data$dates) == selected_year)
    if (length(year_indices) == 0) return(NULL)
    
    # Calculate spatial mean for each time step
    if (precip_type == "total") {
      means <- apply(data$total[,,year_indices], 3, mean, na.rm = TRUE)
    } else {
      means <- apply(data$rainfall[,,year_indices], 3, mean, na.rm = TRUE)
    }
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates[year_indices],
      Value = means
    )
    
    plot_ly(ts_df, x = ~Date, y = ~Value, type = 'scatter', mode = 'lines',
            line = list(color = asu_maroon)) %>%
      layout(
        title = paste("Daily", ifelse(precip_type == "total", "Total", "Rainfall"), "Precipitation"),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Precipitation (mm)"),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Precipitation Monthly Statistics
  output$precip_monthly <- renderPlotly({
    data <- precip_data()
    precip_type <- input$precip_type
    selected_year <- input$precip_year
    
    if (is.null(data)) return(NULL)
    
    # Get the time index for the selected year
    year_indices <- which(year(data$dates) == selected_year)
    if (length(year_indices) == 0) return(NULL)
    
    # Calculate spatial mean for each time step
    if (precip_type == "total") {
      means <- apply(data$total[,,year_indices], 3, mean, na.rm = TRUE)
    } else {
      means <- apply(data$rainfall[,,year_indices], 3, mean, na.rm = TRUE)
    }
    
    # Create monthly statistics data frame
    monthly_df <- data.frame(
      Date = data$dates[year_indices],
      Value = means
    ) %>%
      mutate(Month = month(Date, label = TRUE)) %>%
      group_by(Month) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(monthly_df) %>%
      add_trace(x = ~Month, y = ~Mean, type = 'scatter', mode = 'lines+markers',
                name = 'Mean', line = list(color = asu_maroon)) %>%
      add_trace(x = ~Month, y = ~Min, type = 'scatter', mode = 'lines',
                name = 'Min', line = list(dash = 'dash', color = asu_gold)) %>%
      add_trace(x = ~Month, y = ~Max, type = 'scatter', mode = 'lines',
                name = 'Max', line = list(dash = 'dash', color = asu_dark_maroon)) %>%
      layout(
        title = paste("Monthly", ifelse(precip_type == "total", "Total", "Rainfall"), "Precipitation Statistics"),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Precipitation (mm)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Precipitation Annual Trends
  output$precip_trends <- renderPlotly({
    data <- precip_data()
    precip_type <- input$precip_type
    
    if (is.null(data)) return(NULL)
    
    # Calculate annual means
    if (precip_type == "total") {
      means <- apply(data$total, 3, mean, na.rm = TRUE)
    } else {
      means <- apply(data$rainfall, 3, mean, na.rm = TRUE)
    }
    
    # Create annual statistics data frame
    annual_df <- data.frame(
      Date = data$dates,
      Value = means
    ) %>%
      mutate(Year = year(Date)) %>%
      group_by(Year) %>%
      summarise(
        Total = sum(Value, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate trend line
    trend_line <- lm(Total ~ Year, data = annual_df)
    annual_df$Trend <- predict(trend_line)
    
    plot_ly(annual_df) %>%
      add_trace(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines+markers',
                name = 'Annual Total', line = list(color = asu_maroon)) %>%
      add_trace(x = ~Year, y = ~Trend, type = 'scatter', mode = 'lines',
                name = 'Trend', line = list(dash = 'dot', color = asu_gold)) %>%
      layout(
        title = paste("Annual", ifelse(precip_type == "total", "Total", "Rainfall"), "Precipitation Trends"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Annual Precipitation (mm)"),
        legend = list(x = 0.1, y = 0.9),
        plot_bgcolor = 'white',
        paper_bgcolor = 'white'
      )
  })
  
  # Update the variable choices in the UI
  output$vic_variable <- renderUI({
    selectInput("vic_variable", "Select Variable:",
                choices = c("Precipitation" = "OUT_PREC",
                          "Rainfall" = "OUT_RAINF",
                          "Evapotranspiration" = "OUT_EVAP",
                          "Runoff" = "OUT_RUNOFF",
                          "Baseflow" = "OUT_BASEFLOW",
                          "Soil Moisture" = "OUT_SOIL_MOIST"))
  })

  # Add output for selected year display
  output$selected_soil_year <- renderText({
    input$soil_year
  })
}

# Run the application with increased memory limit
options(shiny.maxRequestSize = 10000*1024^2)  # Increase to 1GB
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE, port = 3838)