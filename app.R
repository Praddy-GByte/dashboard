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
library(markdown)

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
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
    }
    
    /* Header navbar */
    .skin-blue .main-header .navbar {
      background-color: white;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
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
      border-left: 4px solid ", asu_maroon, ";
      border-right: 4px solid ", asu_gold, ";
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

# Function to load VIC data with optimization
load_vic_data <- function() {
  # Check for optimized data first
  optimized_file <- file.path("data_optimized", "VICOut2_compressed.nc")
  if (file.exists(optimized_file)) {
    nc_file <- nc_open(optimized_file)
  } else {
    nc_file <- nc_open("data/VICOut2.nc")
  }
  return(nc_file)
}

# Function to load GRACE data with optimization
load_grace_data <- function() {
  optimized_file <- file.path("data_optimized", "GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI_compressed.nc")
  if (file.exists(optimized_file)) {
    nc_file <- nc_open(optimized_file)
  } else {
    nc_file <- nc_open("data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc")
  }
  return(nc_file)
}

# Function to load SMAP data with optimization
load_smap_data <- function() {
  optimized_file <- file.path("data_optimized", "SPL4SMGP.007_9km_aid0001_compressed.nc")
  if (file.exists(optimized_file)) {
    nc_file <- nc_open(optimized_file)
  } else {
    nc_file <- nc_open("data/SPL4SMGP.007_9km_aid0001.nc")
  }
  return(nc_file)
}

# Function to load calibrated VIC data
load_calibrated_vic_data <- function(year) {
  file_path <- paste0("data/CRB_PRISM_Calibrated.", year, "-01-01.nc")
  if (file.exists(file_path)) {
    nc_file <- nc_open(file_path)
    return(nc_file)
  }
  return(NULL)
}

# Function to extract time series data
extract_time_series <- function(nc_file, var_name, lat_idx = NULL, lon_idx = NULL) {
  if (is.null(lat_idx)) lat_idx <- 1
  if (is.null(lon_idx)) lon_idx <- 1
  
  var_data <- ncvar_get(nc_file, var_name)
  time <- ncvar_get(nc_file, "time")
  
  # Check if we have valid data
  if (length(dim(var_data)) == 0 || length(time) == 0) {
    return(NULL)
  }
  
  # Handle different data dimensions
  if (var_name == "OUT_SOIL_MOIST") {
    # For soil moisture, average across layers
    if (length(dim(var_data)) == 4) {
      data <- apply(var_data[lon_idx, lat_idx, , ], 2, mean, na.rm = TRUE)
    } else {
      data <- var_data[lon_idx, lat_idx, ]
    }
  } else if (length(dim(var_data)) == 3) {
    # For 3D data (lon, lat, time)
    data <- var_data[lon_idx, lat_idx, ]
  } else if (length(dim(var_data)) == 4) {
    # For 4D data (lon, lat, layer, time)
    data <- var_data[lon_idx, lat_idx, 1, ]  # Using first layer
  } else {
    return(NULL)
  }
  
  # Create data frame with proper structure
  result <- data.frame(
    time = time,
    value = as.vector(data)
  )
  
  return(result)
}

# Function to convert VIC time to date
convert_vic_time <- function(time) {
  # VIC time is in days since 0001-01-01
  # Convert to days since 1982-01-01 (start of our data)
  days_since_1982 <- time - as.numeric(as.Date("1982-01-01") - as.Date("0001-01-01"))
  as.Date(days_since_1982, origin = "1982-01-01")
}

# Function to get variable metadata
get_vic_metadata <- function(var_name) {
  metadata <- list(
    "OUT_PREC" = list(
      name = "Total Precipitation",
      unit = "mm/day",
      description = "Total incoming precipitation (rain + snow)"
    ),
    "OUT_RAINF" = list(
      name = "Rainfall",
      unit = "mm/day",
      description = "Liquid rainfall amount"
    ),
    "OUT_SNOWF" = list(
      name = "Snowfall",
      unit = "mm/day",
      description = "Snowfall amount"
    ),
    "OUT_EVAP" = list(
      name = "Total Evaporation",
      unit = "mm/day",
      description = "Total net evaporation"
    ),
    "OUT_EVAP_BARE" = list(
      name = "Bare Soil Evaporation",
      unit = "mm/day",
      description = "Evaporation from bare soil"
    ),
    "OUT_EVAP_CANOP" = list(
      name = "Canopy Evaporation",
      unit = "mm/day",
      description = "Evaporation from canopy"
    ),
    "OUT_TRANSP_VEG" = list(
      name = "Vegetation Transpiration",
      unit = "mm/day",
      description = "Transpiration from vegetation"
    ),
    "OUT_PET" = list(
      name = "Potential ET",
      unit = "mm/day",
      description = "Potential evapotranspiration"
    ),
    "OUT_RUNOFF" = list(
      name = "Runoff",
      unit = "mm/day",
      description = "Surface runoff"
    ),
    "OUT_BASEFLOW" = list(
      name = "Baseflow",
      unit = "mm/day",
      description = "Subsurface runoff"
    ),
    "OUT_SOIL_MOIST" = list(
      name = "Soil Moisture",
      unit = "mm",
      description = "Soil total moisture content"
    ),
    "OUT_SOIL_WET" = list(
      name = "Soil Wetness",
      unit = "fraction",
      description = "Soil wetness fraction"
    ),
    "OUT_SOIL_TEMP" = list(
      name = "Soil Temperature",
      unit = "°C",
      description = "Soil temperature"
    ),
    "OUT_SWE" = list(
      name = "Snow Water Equivalent",
      unit = "mm",
      description = "Snow water equivalent in snow pack"
    ),
    "OUT_SNOW_MELT" = list(
      name = "Snow Melt",
      unit = "mm/day",
      description = "Snow melt rate"
    ),
    "OUT_SUB_SNOW" = list(
      name = "Sub-Snow",
      unit = "mm",
      description = "Water content below snow pack"
    ),
    "OUT_SNOW_SURF_TEMP" = list(
      name = "Snow Surface Temperature",
      unit = "°C",
      description = "Temperature at snow surface"
    ),
    "OUT_SNOW_PACK_TEMP" = list(
      name = "Snow Pack Temperature",
      unit = "°C",
      description = "Temperature of snow pack"
    ),
    "OUT_AIR_TEMP" = list(
      name = "Air Temperature",
      unit = "°C",
      description = "Air temperature at 2m height"
    ),
    "OUT_SURF_TEMP" = list(
      name = "Surface Temperature",
      unit = "°C",
      description = "Surface temperature"
    ),
    "OUT_BARESOILT" = list(
      name = "Bare Soil Temperature",
      unit = "°C",
      description = "Temperature of bare soil"
    ),
    "OUT_VEGT" = list(
      name = "Vegetation Temperature",
      unit = "°C",
      description = "Vegetation temperature"
    ),
    "OUT_SURFSTOR" = list(
      name = "Surface Storage",
      unit = "mm",
      description = "Surface water storage"
    )
  )
  return(metadata[[var_name]])
}

# Function to get VIC data range
get_vic_years_range <- function() {
  nc_file <- nc_open("data/VICOut2.nc")
  time <- ncvar_get(nc_file, "time")
  dates <- convert_vic_time(time)
  years <- as.numeric(unique(format(dates, "%Y")))
  nc_close(nc_file)
  return(c(min(years), max(years)))
}

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Colorado River Basin Dashboard",
    titleWidth = 300,
    tags$li(class = "dropdown",
      tags$img(src = "logos/nasa.png", height = "40px", 
              style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
    ),
    tags$li(class = "dropdown",
      tags$img(src = "logos/asu.jpg", height = "40px", 
              style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
    ),
    tags$li(class = "dropdown",
      tags$img(src = "logos/cap.jpg", height = "40px", 
              style = "padding: 5px; border: 2px solid #FFC627; border-radius: 50%;")
    )
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
      menuItem("Soil Moisture", tabName = "soil", icon = icon("seedling")),
      menuItem("SWE Anomalies", tabName = "swe_anomalies", icon = icon("snowflake")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
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
                  ),
                  tabPanel("Historical Analysis",
                           box(width = 12, title = "Historical Analysis Trends",
                               div(class = "info-tile",
                                   h4("Historical Trends"),
                                   p("Analysis of historical trends in snowpack, precipitation, and water resources."),
                                   p("Provides insights into long-term changes and patterns in the basin.")
                               ),
                               fluidRow(
                                 column(width = 8,
                                   div(class = "map-container",
                                       tags$img(src = "images/historical_trend.png",
                                               alt = "Historical Analysis Map",
                                               style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                       div(class = "map-caption",
                                           p("Historical trends analysis across the Colorado River Basin."),
                                           p("Shows long-term changes in key water resource indicators.")
                                       )
                                   )
                                 ),
                                 column(width = 4,
                                   infoBox(
                                     title = "Time Period",
                                     value = "1982-2024",
                                     icon = icon("calendar"),
                                     color = "maroon",
                                     width = 12,
                                     subtitle = "Analysis period"
                                   ),
                                   infoBox(
                                     title = "Key Indicators",
                                     value = "5+",
                                     icon = icon("chart-line"),
                                     color = "blue",
                                     width = 12,
                                     subtitle = "Snowpack, precip, flow"
                                   ),
                                   infoBox(
                                     title = "Update Frequency",
                                     value = "Annual",
                                     icon = icon("sync"),
                                     color = "green",
                                     width = 12,
                                     subtitle = "Trend analysis"
                                   )
                                 )
                               ),
                               tabsetPanel(
                                 tabPanel("Snowpack Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/snotel_trend.png",
                                                          alt = "Snowpack Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Long-term trends in snowpack accumulation and melt."),
                                                      p("Shows changes in snow water equivalent over time.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Trend Direction",
                                                value = "Decreasing",
                                                icon = icon("arrow-down"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Overall trend"
                                              ),
                                              infoBox(
                                                title = "Rate of Change",
                                                value = "-2.5%/decade",
                                                icon = icon("chart-line"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Average change"
                                              ),
                                              infoBox(
                                                title = "Significance",
                                                value = "95%",
                                                icon = icon("chart-bar"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Confidence level"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Precipitation Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/vic_trend.png",
                                                          alt = "Precipitation Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Historical trends in precipitation patterns."),
                                                      p("Analyzes changes in rainfall and snowfall over time.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Annual Change",
                                                value = "-1.2%/decade",
                                                icon = icon("cloud-rain"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Precipitation trend"
                                              ),
                                              infoBox(
                                                title = "Seasonal Shift",
                                                value = "Earlier",
                                                icon = icon("calendar-alt"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Peak timing"
                                              ),
                                              infoBox(
                                                title = "Variability",
                                                value = "Increasing",
                                                icon = icon("random"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Year-to-year variation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Water Resources",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/grace_trend.png",
                                                          alt = "Water Resources Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Changes in water availability and usage."),
                                                      p("Tracks long-term trends in water resources.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Storage Change",
                                                value = "-15%",
                                                icon = icon("water"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Reservoir levels"
                                              ),
                                              infoBox(
                                                title = "Demand Growth",
                                                value = "+2%/year",
                                                icon = icon("users"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Water usage"
                                              ),
                                              infoBox(
                                                title = "Efficiency",
                                                value = "+1.5%/year",
                                                icon = icon("tint"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Water use efficiency"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Soil Moisture Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/smap_surface_trend.png",
                                                          alt = "Soil Moisture Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Historical trends in soil moisture patterns."),
                                                      p("Analyzes changes in surface and root zone soil moisture.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Surface Change",
                                                value = "-1.8%/decade",
                                                icon = icon("tint"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Surface soil moisture"
                                              ),
                                              infoBox(
                                                title = "Root Zone Change",
                                                value = "-2.1%/decade",
                                                icon = icon("seedling"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Root zone moisture"
                                              ),
                                              infoBox(
                                                title = "Seasonal Impact",
                                                value = "Increasing",
                                                icon = icon("calendar-alt"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Seasonal variation"
                                              )
                                            )
                                          )
                                 ),
                                 tabPanel("Combined Trends",
                                          fluidRow(
                                            column(width = 8,
                                              div(class = "plot-container",
                                                  tags$img(src = "images/combined_trends.png",
                                                          alt = "Combined Trends",
                                                          style = "width: 100%; height: auto; max-height: 600px; object-fit: contain;"),
                                                  div(class = "plot-caption",
                                                      p("Combined analysis of all water resource trends."),
                                                      p("Shows the relationship between different water resource indicators.")
                                                  )
                                              )
                                            ),
                                            column(width = 4,
                                              infoBox(
                                                title = "Correlation",
                                                value = "0.85",
                                                icon = icon("link"),
                                                color = "maroon",
                                                width = 12,
                                                subtitle = "Trend correlation"
                                              ),
                                              infoBox(
                                                title = "Consistency",
                                                value = "High",
                                                icon = icon("check-circle"),
                                                color = "blue",
                                                width = 12,
                                                subtitle = "Trend consistency"
                                              ),
                                              infoBox(
                                                title = "Significance",
                                                value = "99%",
                                                icon = icon("chart-bar"),
                                                color = "green",
                                                width = 12,
                                                subtitle = "Statistical significance"
                                              )
                                            )
                                          )
                                 )
                               ),
                               div(class = "source-info",
                                   p("Source: USGS, NOAA, and Bureau of Reclamation - 2024",
                                     style = "font-size: 10px; font-style: italic; text-align: right;")
                               )
                           )
                  )
                )
              )
      ),
      
      # VIC Model Tab (Combined)
      tabItem(tabName = "vic",
              # Header with ASU colors
              div(class = "vic-header",
                  style = "border-top: 4px solid #8C1D40; border-bottom: 4px solid #FFC627; margin-bottom: 20px;",
                  h2("VIC Model Analysis", 
                     style = "color: #8C1D40; text-align: center; padding: 10px;")
              ),
              
              # Info Boxes Row
              fluidRow(
                infoBox(
                  title = "Model Resolution",
                  value = "4km",
                  icon = icon("ruler"),
                  color = "orange",
                  width = 3,
                  fill = FALSE,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Temporal Resolution",
                  value = "Daily",
                  icon = icon("clock"),
                  color = "red",
                  width = 3,
                  fill = FALSE,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Data Range",
                  value = "1982-2024",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  fill = FALSE,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Outputs</span>")
                ),
                infoBox(
                  title = "Variables",
                  value = "7",
                  icon = icon("list"),
                  color = "purple",
                  width = 3,
                  fill = FALSE,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: VIC Model Documentation - 2024</span>")
                )
              ),
              
              # Model Information Box
              fluidRow(
                box(
                  title = "VIC Model Information",
                  width = 12,
                  status = NULL,
                  solidHeader = TRUE,
                  style = "background-color: white; border: 1px solid #ddd;",
                  HTML("
                    <div style='padding: 15px;'>
                      <p><strong>Model Description:</strong> The VIC (Variable Infiltration Capacity) model provides comprehensive hydrological simulations.</p>
                      <p><strong>Key variables:</strong> precipitation, evapotranspiration, runoff, soil moisture, and snow water equivalent.</p>
                      <p><strong>Spatial resolution:</strong> 4km</p>
                      <p><strong>Temporal resolution:</strong> Daily</p>
                      <p><strong>Data Range:</strong> 1982-2024</p>
                    </div>
                  ")
                )
              ),
              
              # Data Selection Box
              fluidRow(
                box(
                  title = "Data Selection",
                  width = 12,
                  status = NULL,
                  solidHeader = TRUE,
                  style = "background-color: white; border: 1px solid #ddd;",
                  fluidRow(
                    column(width = 4,
                           selectInput("vic_variable", "Select Variable:",
                                     choices = c(
                                       "Precipitation" = "OUT_PREC",
                                       "Rainfall" = "OUT_RAINF",
                                       "Snow Water Equivalent" = "OUT_SWE",
                                       "Runoff" = "OUT_RUNOFF",
                                       "Baseflow" = "OUT_BASEFLOW",
                                       "Evapotranspiration" = "OUT_EVAP",
                                       "Soil Moisture" = "OUT_SOIL_MOIST",
                                       "Air Temperature" = "OUT_AIR_TEMP"
                                     ),
                                     selected = "OUT_PREC")
                    ),
                    column(width = 4,
                           sliderInput("vic_year", "Select Year:",
                                     min = 1982, max = 2024, value = 2024,
                                     step = 1, sep = "")
                    ),
                    column(width = 4,
                           sliderInput("vic_time_index", "Select Time Index:",
                                     min = 1, max = 365, value = 1,
                                     step = 1, sep = "")
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                           div(class = "info-tile",
                               h4("Variable Information"),
                               p("Select a variable, year, and time index to view the data."),
                               p("Time index represents the day of the year (1-365).")
                           )
                    )
                  )
                )
              ),
              
              # Analysis Tabs
              fluidRow(
                tabBox(
                  title = "VIC Model Analysis",
                  width = 12,
                  tabPanel("Spatial Distribution",
                           fluidRow(
                             column(width = 8,
                                    div(class = "map-container",
                                        uiOutput("vic_map")
                                    )
                             ),
                             column(width = 4,
                                    infoBox(
                                      title = "Variable",
                                      value = textOutput("vic_var_name"),
                                      icon = icon("chart-line"),
                                      color = "maroon",
                                      width = 12
                                    ),
                                    infoBox(
                                      title = "Unit",
                                      value = textOutput("vic_var_unit"),
                                      icon = icon("ruler"),
                                      color = "yellow",
                                      width = 12
                                    ),
                                    infoBox(
                                      title = "Description",
                                      value = textOutput("vic_var_desc"),
                                      icon = icon("info-circle"),
                                      color = "maroon",
                                      width = 12
                                    )
                             )
                           )
                  ),
                  tabPanel("Time Series",
                           fluidRow(
                             column(width = 12,
                                    div(class = "plot-container",
                                        uiOutput("vic_timeseries")
                                    )
                             )
                           )
                  ),
                  tabPanel("Monthly Statistics",
                           fluidRow(
                             column(width = 12,
                                    div(class = "plot-container",
                                        uiOutput("vic_monthly")
                                    )
                             )
                           )
                  ),
                  tabPanel("Variable Statistics",
                           fluidRow(
                             column(width = 12,
                                    div(class = "table-container",
                                        tableOutput("vic_stats")
                                    )
                             )
                           )
                  ),
                  tabPanel("Annual Trends",
                           fluidRow(
                             column(width = 12,
                                    div(class = "plot-container",
                                        plotlyOutput("vic_trend")
                                    )
                             )
                           )
                  )
                )
              ),
              
              # Footer
              div(class = "vic-footer",
                  style = "border-top: 4px solid #FFC627; margin-top: 20px; padding: 10px; text-align: center;",
                  HTML("<p style='color: #8C1D40;'>VIC Model Analysis Dashboard | Arizona State University</p>")
              ),
              
              # VIC Map
              fluidRow(
                box(width = 12, title = "VIC Model Output Map",
                    uiOutput("vic_map_placeholder"),
                    plotlyOutput("vic_map", height = "600px"))
              ),
              
              # TIF Data Display
              fluidRow(
                box(width = 12, title = "TIF Data Visualization",
                    uiOutput("tif_data_placeholder"),
                    plotlyOutput("tif_data_plot", height = "400px"))
              )
      ),
      
      # SMAP Data Tab
      tabItem(tabName = "smap",
              fluidRow(
                div(class = "variable-info",
                    h4("SMAP (Soil Moisture Active Passive) Data"),
                    p("SMAP provides high-resolution global observations of soil moisture and freeze/thaw state."),
                    p("Key variables include soil moisture profile and root zone soil moisture."),
                    p("Spatial resolution: 9km, Temporal resolution: Daily")
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
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: SMAP Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Temporal Resolution",
                  value = "Daily",
                  icon = icon("clock"),
                  color = "yellow",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: SMAP Documentation - 2024</span>")
                ),
                infoBox(
                  title = "Data Range",
                  value = "2020",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: SMAP Data</span>")
                ),
                infoBox(
                  title = "Variables",
                  value = "2",
                  icon = icon("list"),
                  color = "blue",
                  width = 3,
                  subtitle = HTML("<span style='font-size: 8px; font-style: italic;'>Source: SMAP Documentation - 2024</span>")
                )
              ),
              # Add custom CSS for dropdown visibility
              tags$head(tags$style(HTML("
                .selectize-dropdown {
                  z-index: 9999 !important;
                }
                .selectize-input {
                  z-index: 9999 !important;
                }
                .box {
                  margin-bottom: 20px;
                }
                .selectize-control {
                  margin-bottom: 10px;
                }
              "))),
              fluidRow(
                box(width = 12, title = "SMAP Data Analysis",
                    style = "overflow: visible; z-index: 9999;",
                    fluidRow(
                      column(width = 4,
                             div(style = "z-index: 9999;",
                                 selectInput("smap_variable", "Select Variable:",
                                           choices = c(
                                             "Soil Moisture Profile" = "sm_profile",
                                             "Root Zone Soil Moisture" = "sm_rootzone"
                                           ),
                                           selectize = TRUE,
                                           width = "100%")
                             )
                      ),
                      column(width = 4,
                             sliderInput("smap_day", "Select Day:",
                                       min = 1,
                                       max = 365,
                                       value = 1,
                                       step = 1,
                                       animate = animationOptions(interval = 1000))
                      ),
                      column(width = 4,
                             div(style = "z-index: 9999;",
                                 selectInput("smap_analysis", "Select Analysis:",
                                           choices = c("Spatial Distribution" = "spatial",
                                                     "Time Series" = "timeseries",
                                                     "Monthly Statistics" = "monthly",
                                                     "Seasonal Analysis" = "seasonal",
                                                     "Anomaly Analysis" = "anomaly"),
                                           selectize = TRUE,
                                           width = "100%")
                             )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Spatial Distribution",
                    style = "margin-top: 20px;",
                    plotlyOutput("smap_map", height = "600px"))
              ),
              fluidRow(
                box(width = 12, title = "Time Series Analysis",
                    style = "margin-top: 20px;",
                    plotlyOutput("smap_timeseries", height = "400px"))
              ),
              fluidRow(
                box(width = 6, title = "Monthly Statistics",
                    style = "margin-top: 20px;",
                    plotlyOutput("smap_monthly", height = "400px")),
                box(width = 6, title = "Seasonal Analysis",
                    style = "margin-top: 20px;",
                    plotlyOutput("smap_seasonal", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "Anomaly Analysis",
                    style = "margin-top: 20px;",
                    plotlyOutput("smap_anomaly", height = "400px"))
              ),
              # SMAP Map
              fluidRow(
                box(width = 12, title = "SMAP Soil Moisture Map",
                    uiOutput("smap_map_placeholder"),
                    plotlyOutput("smap_map", height = "600px"))
              ),
              
              # SMAP TIF Data Display
              fluidRow(
                box(width = 12, title = "SMAP TIF Data Visualization",
                    uiOutput("smap_tif_placeholder"),
                    plotlyOutput("smap_tif_plot", height = "400px"))
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
                    uiOutput("grace_map_placeholder"),
                    plotlyOutput("grace_map", height = "600px"))
              ),
              fluidRow(
                box(width = 6, title = "Time Series Analysis",
                    uiOutput("grace_timeseries_placeholder"),
                    plotlyOutput("grace_timeseries", height = "400px")),
                box(width = 6, title = "Seasonal Analysis",
                    uiOutput("grace_seasonal_placeholder"),
                    plotlyOutput("grace_seasonal", height = "400px"))
              ),
              fluidRow(
                box(width = 12, title = "Uncertainty Analysis",
                    uiOutput("grace_uncertainty_placeholder"),
                    plotlyOutput("grace_uncertainty", height = "400px"))
              ),
              # GRACE TIF Data Display
              fluidRow(
                box(width = 12, title = "GRACE TIF Data Visualization",
                    uiOutput("grace_tif_placeholder"),
                    plotlyOutput("grace_tif_plot", height = "400px"))
              )
      ),
      
      # Snow Water Equivalent Tab
      tabItem(tabName = "swe",
              fluidRow(
                div(class = "variable-info",
                    h4("Snow Water Equivalent Analysis"),
                    p("Compare VIC model snow water equivalent (SWE) with SNOTEL station observations."),
                    p("Analyze spatial patterns and temporal trends in snowpack conditions."),
                    p("Critical for water resource management and flood forecasting.")
                )
              ),
              fluidRow(
                # Add info tiles for data sources
                infoBox(
                  title = "VIC Model SWE",
                  value = "6km Resolution",
                  icon = icon("snowflake"),
                  color = "maroon",
                  width = 3,
                  subtitle = "Daily, 1982-2024"
                ),
                infoBox(
                  title = "SNOTEL Stations",
                  value = "100+",
                  icon = icon("map-marker-alt"),
                  color = "yellow",
                  width = 3,
                  subtitle = "Daily updates"
                ),
                infoBox(
                  title = "Analysis Period",
                  value = "1982-2024",
                  icon = icon("calendar"),
                  color = "green",
                  width = 3,
                  subtitle = "Historical coverage"
                ),
                infoBox(
                  title = "Variables",
                  value = "4",
                  icon = icon("chart-line"),
                  color = "blue",
                  width = 3,
                  subtitle = "SWE, depth, temp, precip"
                )
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel("Spatial Analysis",
                           box(width = 12, title = "Daily Snow Water Equivalent Map",
                               fluidRow(
                                 column(width = 3,
                                        sliderInput("swe_date", "Select Date:",
                                                  min = as.Date("1982-01-01"),
                                                  max = as.Date("2024-12-31"),
                                                  value = as.Date("2024-01-01"),
                                                  timeFormat = "%Y-%m-%d",
                                                  animate = animationOptions(interval = 1000))
                                 ),
                                 column(width = 3,
                                        checkboxGroupInput("swe_layers", "Map Layers:",
                                                         choices = c("SNOTEL Sites" = "snotel",
                                                                   "HUC-10 Boundaries" = "huc10",
                                                                   "Analysis Sub-basins" = "subbasins",
                                                                   "CRB/UCRB/LCRB" = "basins"),
                                                         selected = c("snotel", "huc10"))
                                 ),
                                 column(width = 3,
                                        selectInput("swe_metric", "Select Metric:",
                                                  choices = c("Snow Water Equivalent (mm)" = "SWE",
                                                            "Snow Depth (cm)" = "SNWD",
                                                            "Temperature (°C)" = "TOBS",
                                                            "Precipitation (mm)" = "PREC"))
                                 ),
                                 column(width = 3,
                                        selectInput("swe_source", "Data Source:",
                                                  choices = c("VIC Model" = "vic",
                                                            "SNOTEL Stations" = "snotel",
                                                            "Both" = "both"))
                                 )
                               ),
                               plotlyOutput("swe_map", height = "600px")
                           )
                  ),
                  tabPanel("Time Series Analysis",
                           box(width = 12, title = "Snow Water Equivalent Time Series",
                               fluidRow(
                                 column(width = 4,
                                        selectInput("swe_station", "Select SNOTEL Station:",
                                                  choices = c("All Stations" = "all",
                                                            "Individual Stations" = "individual"))
                                 ),
                                 column(width = 4,
                                        sliderInput("swe_date_range", "Date Range:",
                                                  min = as.Date("1982-01-01"),
                                                  max = as.Date("2024-12-31"),
                                                  value = c(as.Date("1982-01-01"), as.Date("2024-12-31")),
                                                  timeFormat = "%Y-%m-%d")
                                 ),
                                 column(width = 4,
                                        selectInput("swe_aggregation", "Aggregation:",
                                                  choices = c("Daily" = "daily",
                                                            "Weekly" = "weekly",
                                                            "Monthly" = "monthly",
                                                            "Annual" = "annual"))
                                 )
                               ),
                               plotlyOutput("swe_timeseries", height = "400px")
                           )
                  ),
                  tabPanel("Statistical Analysis",
                           box(width = 12, title = "Snow Water Equivalent Statistics",
                               fluidRow(
                                 column(width = 6,
                                        plotlyOutput("swe_monthly_stats", height = "400px")
                                 ),
                                 column(width = 6,
                                        plotlyOutput("swe_elevation_stats", height = "400px")
                                 )
                               ),
                               fluidRow(
                                 column(width = 12,
                                        plotlyOutput("swe_trend_analysis", height = "400px")
                                 )
                               )
                           )
                  ),
                  tabPanel("Model Comparison",
                           box(width = 12, title = "VIC vs SNOTEL Comparison",
                               fluidRow(
                                 column(width = 6,
                                        plotlyOutput("swe_model_comparison", height = "400px")
                                 ),
                                 column(width = 6,
                                        plotlyOutput("swe_validation", height = "400px")
                                 )
                               ),
                               fluidRow(
                                 column(width = 12,
                                        dataTableOutput("swe_stats_table")
                                 )
                               )
                           )
                  )
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
      ),
      tabItem(
        tabName = "vic_data",
        # Header with ASU colors
        div(class = "vic-header",
            style = "border-top: 4px solid #8C1D40; border-bottom: 4px solid #FFC627; margin-bottom: 20px;",
            h2("VIC Model Data Analysis", 
               style = "color: #8C1D40; text-align: center; padding: 10px;")
        ),
        
        # Main content
        fluidRow(
          box(
            title = "VIC Model Information",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
            HTML("
              <div style='padding: 15px;'>
                <p style='color: #8C1D40;'><strong>Model Description:</strong> The VIC (Variable Infiltration Capacity) model provides comprehensive hydrological simulations.</p>
                <p style='color: #8C1D40;'><strong>Key variables:</strong> precipitation, evapotranspiration, runoff, soil moisture, and snow water equivalent.</p>
                <p style='color: #8C1D40;'><strong>Spatial resolution:</strong> 4km</p>
                <p style='color: #8C1D40;'><strong>Temporal resolution:</strong> Daily</p>
                <p style='color: #8C1D40;'><strong>Data Range:</strong> 1982-2024</p>
              </div>
            ")
          )
        ),
        
        # Data Selection Box
        fluidRow(
          box(
            title = "Data Selection",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
            column(4,
              div(style = "color: #8C1D40;",
                selectInput("vic_variable", "Select Variable:",
                          choices = c("Precipitation" = "OUT_PREC",
                                    "Rainfall" = "OUT_RAINF",
                                    "Evapotranspiration" = "OUT_EVAP",
                                    "Runoff" = "OUT_RUNOFF",
                                    "Baseflow" = "OUT_BASEFLOW",
                                    "Soil Moisture" = "OUT_SOIL_MOIST"))
              )
            ),
            column(4,
              sliderInput("vic_year", "Select Year:",
                        min = 1982,
                        max = 2024,
                        value = 2024,
                        step = 1,
                        sep = "",
                        animate = animationOptions(interval = 1000))
            ),
            column(4,
              div(style = "color: #8C1D40;",
                selectInput("vic_analysis", "Select Analysis:",
                          choices = c("Time Series" = "time_series",
                                    "Spatial Distribution" = "spatial",
                                    "Monthly Statistics" = "monthly"))
              )
            )
          )
        ),
        
        # Info Boxes Row
        fluidRow(
          infoBoxOutput("vic_var_name", width = 4),
          infoBoxOutput("vic_var_unit", width = 4),
          infoBoxOutput("vic_var_desc", width = 4)
        ),
        
        # Analysis Plots
        conditionalPanel(
          condition = "input.vic_analysis == 'time_series'",
          fluidRow(
            box(
              title = "Time Series Analysis",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
              plotlyOutput("vic_time_series")
            )
          )
        ),
        conditionalPanel(
          condition = "input.vic_analysis == 'spatial'",
          fluidRow(
            box(
              title = "Spatial Distribution",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
              plotlyOutput("vic_spatial")
            )
          )
        ),
        conditionalPanel(
          condition = "input.vic_analysis == 'monthly'",
          fluidRow(
            box(
              title = "Monthly Statistics",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
              plotlyOutput("vic_monthly")
            )
          )
        ),
        
        # Statistics Box
        fluidRow(
          box(
            title = "Variable Statistics",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            style = "border-top: 3px solid #8C1D40; border-bottom: 3px solid #FFC627;",
            tableOutput("vic_stats")
          )
        ),
        
        # Footer
        div(class = "vic-footer",
            style = "border-top: 4px solid #FFC627; margin-top: 20px; padding: 10px; text-align: center;",
            HTML("<p style='color: #8C1D40;'>VIC Model Data Analysis Dashboard | Arizona State University</p>")
        )
      ),
      
      # New tab for Statistics and Processed Data
      tabItem(tabName = "statistics",
              h2("Data Statistics and Processed Data"),
              
              # Statistics Section
              fluidRow(
                box(title = "VIC Model Statistics", width = 6,
                    DTOutput("vic_stats_table")),
                box(title = "SMAP Statistics", width = 6,
                    DTOutput("smap_stats_table"))
              ),
              
              fluidRow(
                box(title = "GRACE Statistics", width = 6,
                    DTOutput("grace_stats_table")),
                box(title = "SNOTEL Statistics", width = 6,
                    DTOutput("snotel_stats_table"))
              ),
              
              # Processed Data Section
              fluidRow(
                box(title = "VIC Processed Data", width = 12,
                    DTOutput("vic_processed_table"))
              ),
              
              fluidRow(
                box(title = "SMAP Processed Data", width = 12,
                    DTOutput("smap_processed_table"))
              ),
              
              fluidRow(
                box(title = "GRACE Processed Data", width = 12,
                    DTOutput("grace_processed_table"))
              ),
              
              # Pre-generated Visualizations
              fluidRow(
                box(title = "Basin HUC10 SNOTEL Interactive", width = 12,
                    htmlOutput("basin_huc10_snotel_viz")),
                box(title = "VIC Time Series", width = 12,
                    htmlOutput("vic_time_series_viz"))
              )
      ),
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "Help Documentation",
                  width = 12,
                  tabBox(
                    width = 12,
                    tabPanel("Overview",
                            includeMarkdown("help.md")
                    ),
                    tabPanel("Usage Guide",
                            includeMarkdown("help_usage.md")
                    ),
                    tabPanel("Data Structure",
                            includeMarkdown("help_data_structure.md")
                    ),
                    tabPanel("Troubleshooting",
                            includeMarkdown("help_troubleshooting.md")
                    ),
                    tabPanel("Contact",
                            includeMarkdown("help_contact.md")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "swe_anomalies",
              fluidRow(
                box(title = "April 1 SWE Anomalies (1985-2024)",
                    width = 12,
                    plotlyOutput("april1_swe_anomalies_plot"),
                    p("This visualization shows the April 1 SWE anomalies for the Colorado River Basin from 1985 to 2024. 
                      The anomalies are calculated relative to the 1985-2024 average. Years with positive anomalies 
                      indicate above-average snowpack, while negative anomalies indicate below-average snowpack.")
                )
              ),
              fluidRow(
                box(title = "April 1 SWE Trends",
                    width = 12,
                    plotlyOutput("april1_swe_trends_plot"),
                    p("This plot shows the linear trend in April 1 SWE over the study period. The trend line 
                      helps identify long-term changes in snowpack accumulation.")
                )
              )
      )
    )
  )
)

# Add custom CSS for VIC tab
tags$head(
  tags$style(HTML("
    .vic-header {
      background-color: white;
      padding: 10px;
      margin-bottom: 20px;
    }
    
    .vic-footer {
      background-color: white;
      padding: 10px;
      margin-top: 20px;
    }
    
    .box {
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      transition: all 0.3s ease;
    }
    
    .box:hover {
      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      transform: translateY(-2px);
    }
    
    .info-box {
      background-color: white !important;
      border: 1px solid #ddd !important;
      border-radius: 5px !important;
    }
    .info-box-icon[data-icon='ruler'] {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .info-box-icon[data-icon='clock'] {
      background-color: white !important;
      color: #FFC627 !important;
    }
    .info-box-icon[data-icon='calendar'] {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .info-box-icon[data-icon='list'] {
      background-color: white !important;
      color: #FFC627 !important;
    }
    .info-box-content {
      background-color: white !important;
    }
    .info-box-text {
      color: #8C1D40 !important;
    }
    .info-box-number {
      color: #8C1D40 !important;
    }
    .small-box {
      background-color: white !important;
      border: 1px solid #ddd !important;
      border-radius: 5px !important;
    }
    .small-box .icon {
      background-color: white !important;
      color: #8C1D40 !important;
    }
    .small-box .inner {
      background-color: white !important;
    }
    .small-box h3 {
      color: #8C1D40 !important;
    }
    .small-box p {
      color: #8C1D40 !important;
    }
  "))
)

# Server
server <- function(input, output, session) {
  # Add resource path for images
  addResourcePath("images", "images")
  
  # Check for required data files
  observe({
    req_files <- c(
      "data/VIC_outputs",
      "data/SMAP_outputs",
      "data/GRACE_outputs",
      "data/Analysis_Basin_Shapefiles/basins_all_unique.shp",
      "data/huc10s/huc10.shp"
    )
    
    missing <- !file.exists(req_files)
    if (any(missing)) {
      showModal(modalDialog(
        title = "Missing Data Files",
        paste("The following required files/directories are missing:", 
              paste(req_files[missing], collapse = ", ")),
        footer = modalButton("OK")
      ))
    }
  })
  
  # Helper function for data validation
  validate_data <- function(data, data_type) {
    if (is.null(data)) {
      showNotification(paste(data_type, "data not available"), type = "error")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Helper function for file validation
  validate_file <- function(file_path, file_type) {
    if (!file.exists(file_path)) {
      showNotification(paste(file_type, "file not found:", file_path), type = "error")
      return(FALSE)
    }
    return(TRUE)
  }
  
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
  
  # Load VIC data with enhanced error handling
  vic_data <- reactive({
    selected_year <- input$vic_year
    selected_variable <- input$vic_variable
    
    if (is.null(selected_year) || is.null(selected_variable)) {
      showNotification("Please select both year and variable", type = "warning")
      return(NULL)
    }
    
    tryCatch({
      # First try to load optimized calibrated data
      optimized_file <- file.path("data_optimized", 
                                 paste0("CRB_PRISM_Calibrated.", selected_year, 
                                       "-01-01_compressed.nc"))
      if (file.exists(optimized_file)) {
        nc <- nc_open(optimized_file)
      } else {
        nc <- load_calibrated_vic_data(selected_year)
      }
      
      # If calibrated data not available, use main VIC output
      if (is.null(nc)) {
        nc <- load_vic_data()
      }
      
      on.exit(nc_close(nc))
      
      data <- ncvar_get(nc, input$vic_variable)
      if (input$vic_variable == "OUT_SOIL_MOIST") {
        data <- drop(data[1,,,])
      }
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error loading VIC data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Load GRACE data
  grace_data <- reactive({
    tryCatch({
      nc <- load_grace_data()
      on.exit(nc_close(nc))
      
      # Extract GRACE data
      tws <- ncvar_get(nc, "lwe_thickness")
      uncertainty <- ncvar_get(nc, "uncertainty")
      dates <- as.Date(ncvar_get(nc, "time"), origin = "2002-01-01")
      
      return(list(tws = tws, uncertainty = uncertainty, dates = dates))
    }, error = function(e) {
      showNotification(paste("Error loading GRACE data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Load SMAP data
  smap_data <- reactive({
    tryCatch({
      nc <- load_smap_data()
      on.exit(nc_close(nc))
      
      # Extract SMAP data
      sm_profile <- ncvar_get(nc, "sm_profile")
      sm_rootzone <- ncvar_get(nc, "sm_rootzone")
      dates <- as.Date(ncvar_get(nc, "time"), origin = "2015-01-01")
      
      return(list(sm_profile = sm_profile, sm_rootzone = sm_rootzone, dates = dates))
    }, error = function(e) {
      showNotification(paste("Error loading SMAP data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # VIC statistics table
  output$vic_stats <- renderTable({
    nc_file <- vic_data()
    var_data <- ncvar_get(nc_file, input$vic_variable)
    metadata <- get_vic_metadata(input$vic_variable)
    
    stats <- data.frame(
      Statistic = c("Minimum", "Maximum", "Mean", "Standard Deviation"),
      Value = c(
        min(var_data, na.rm = TRUE),
        max(var_data, na.rm = TRUE),
        mean(var_data, na.rm = TRUE),
        sd(var_data, na.rm = TRUE)
      )
    )
    
    # Add units to the values
    stats$Value <- paste0(round(stats$Value, 2), " ", metadata$unit)
    
    stats
  })
  
  # GRACE Uncertainty Analysis
  output$grace_uncertainty <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the selected year
    selected_year <- input$grace_year
    
    # Calculate spatial mean for each time step
    uncertainty_means <- apply(data$uncertainty, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      Uncertainty = uncertainty_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Uncertainty, type = 'scatter', mode = 'lines',
                name = 'Uncertainty', line = list(color = 'red')) %>%
      layout(
        title = paste("Uncertainty Analysis -", selected_year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Uncertainty (cm)"),
        showlegend = TRUE
      )
  })
  
  # GRACE Trend Analysis
  output$grace_trend <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the selected year
    selected_year <- input$grace_year
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    # Calculate trend line
    trend_line <- lm(TWS ~ as.numeric(Date), data = ts_df)
    ts_df$Trend <- predict(trend_line)
    
    # Calculate trend statistics
    trend_stats <- summary(trend_line)
    trend_slope <- trend_stats$coefficients[2,1]
    trend_pvalue <- trend_stats$coefficients[2,4]
    
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~TWS, type = 'scatter', mode = 'lines',
                name = 'TWS', line = list(color = 'blue')) %>%
      add_trace(x = ~Date, y = ~Trend, type = 'scatter', mode = 'lines',
                name = 'Trend', line = list(color = 'red', dash = 'dash')) %>%
      layout(
        title = paste("Trend Analysis -", selected_year,
                     "<br>Slope:", round(trend_slope, 4), "cm/month",
                     "<br>p-value:", round(trend_pvalue, 4)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "TWS (cm)"),
        showlegend = TRUE
      )
  })
  
  # GRACE Soil Moisture Profile
  output$grace_soil_moisture <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the selected year
    selected_year <- input$grace_year
    
    # Calculate spatial mean for each time step
    tws_means <- apply(data$tws, 3, mean, na.rm = TRUE)
    
    # Create time series data frame
    ts_df <- data.frame(
      Date = data$dates,
      TWS = tws_means,
      Year = format(data$dates, "%Y")
    )
    
    # Filter for selected year
    ts_df <- ts_df[ts_df$Year == selected_year,]
    
    # Calculate monthly climatology
    monthly_clim <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      group_by(Month) %>%
      summarise(Climatology = mean(TWS, na.rm = TRUE))
    
    # Calculate anomalies (soil moisture variations)
    ts_df <- ts_df %>%
      mutate(Month = format(Date, "%B")) %>%
      left_join(monthly_clim, by = "Month") %>%
      mutate(Soil_Moisture = TWS - Climatology)
    
    # Create soil moisture profile plot
    plot_ly(ts_df) %>%
      add_trace(x = ~Date, y = ~Soil_Moisture, type = 'scatter', mode = 'lines',
                name = 'Soil Moisture', line = list(color = 'green')) %>%
      add_hline(y = 0, line = list(dash = "dash"), name = "Climatology") %>%
      layout(
        title = paste("Soil Moisture Profile -", selected_year),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Soil Moisture Anomaly (cm)"),
        showlegend = TRUE
      )
  })
  
  # Add these new outputs in the server section
  output$grace_map_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("GRACE data is currently not available. Please check back later or contact the administrator."),
          p("Expected data format: NetCDF files containing terrestrial water storage measurements.")
      )
    }
  })
  
  output$grace_timeseries_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Time series data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })
  
  output$grace_seasonal_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Seasonal analysis data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })
  
  output$grace_uncertainty_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("Uncertainty analysis data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  # TIF Data Display with enhanced error handling
  output$tif_data_placeholder <- renderUI({
    if (is.null(vic_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$tif_data_plot <- renderPlotly({
    data <- vic_data()
    if (!validate_data(data, "VIC")) return(NULL)
    
    # Get the TIF file path based on selected variable and year
    selected_year <- input$vic_year
    selected_variable <- input$vic_variable
    
    if (is.null(selected_year) || is.null(selected_variable)) {
      return(plotly_empty() %>% 
        layout(title = "Please select year and variable",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    }
    
    tif_file <- file.path("data/VIC_outputs", 
                         paste0(selected_variable, "_", selected_year, "_annual.tif"))
    
    if (!validate_file(tif_file, "TIF")) {
      return(plotly_empty() %>% 
        layout(title = "TIF file not found",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    }
    
    tryCatch({
      # Read the TIF file
      r <- raster(tif_file)
      
      # Convert to data frame for plotting
      df <- as.data.frame(r, xy = TRUE)
      names(df) <- c("x", "y", "value")
      
      # Create the plot
      plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
              colors = viridis::viridis(100)) %>%
        layout(title = paste("Annual", selected_variable, "Distribution"),
               xaxis = list(title = "Longitude"),
               yaxis = list(title = "Latitude"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white')
    }, error = function(e) {
      showNotification(paste("Error plotting TIF data:", e$message), type = "error")
      return(plotly_empty() %>% 
        layout(title = "Error plotting data",
               plot_bgcolor = 'white',
               paper_bgcolor = 'white'))
    })
  })

  # SMAP TIF Data Display
  output$smap_tif_placeholder <- renderUI({
    if (is.null(smap_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("SMAP TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$smap_tif_plot <- renderPlotly({
    data <- smap_data()
    if (is.null(data)) return(NULL)
    
    # Get the TIF file path
    tif_file <- file.path("data/SMAP_outputs", "smap_surface_annual.tif")
    
    if (!file.exists(tif_file)) {
      return(NULL)
    }
    
    # Read the TIF file
    r <- raster(tif_file)
    
    # Convert to data frame for plotting
    df <- as.data.frame(r, xy = TRUE)
    names(df) <- c("x", "y", "value")
    
    # Create the plot
    plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
            colors = viridis::viridis(100)) %>%
      layout(title = "SMAP Surface Soil Moisture Distribution",
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             plot_bgcolor = 'white',
             paper_bgcolor = 'white')
  })

  # GRACE TIF Data Display
  output$grace_tif_placeholder <- renderUI({
    if (is.null(grace_data())) {
      div(class = "alert alert-warning",
          h4("Data Not Available"),
          p("GRACE TIF data is currently not available. Please check back later or contact the administrator.")
      )
    }
  })

  output$grace_tif_plot <- renderPlotly({
    data <- grace_data()
    if (is.null(data)) return(NULL)
    
    # Get the TIF file path
    tif_file <- file.path("data/GRACE_outputs", "grace_annual.tif")
    
    if (!file.exists(tif_file)) {
      return(NULL)
    }
    
    # Read the TIF file
    r <- raster(tif_file)
    
    # Convert to data frame for plotting
    df <- as.data.frame(r, xy = TRUE)
    names(df) <- c("x", "y", "value")
    
    # Create the plot
    plot_ly(data = df, x = ~x, y = ~y, z = ~value, type = "heatmap",
            colors = viridis::viridis(100)) %>%
      layout(title = "GRACE Terrestrial Water Storage Distribution",
             xaxis = list(title = "Longitude"),
             yaxis = list(title = "Latitude"),
             plot_bgcolor = 'white',
             paper_bgcolor = 'white')
  })

  # Statistics Tables
  output$vic_stats_table <- renderDT({
    if (file.exists("vic_statistics.csv")) {
      datatable(read.csv("vic_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$smap_stats_table <- renderDT({
    if (file.exists("smap_statistics.csv")) {
      datatable(read.csv("smap_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$grace_stats_table <- renderDT({
    if (file.exists("grace_statistics.csv")) {
      datatable(read.csv("grace_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  output$snotel_stats_table <- renderDT({
    if (file.exists("snotel_statistics.csv")) {
      datatable(read.csv("snotel_statistics.csv"), 
                options = list(pageLength = 5, scrollX = TRUE))
    }
  })
  
  # Processed Data Tables
  output$vic_processed_table <- renderDT({
    processed_files <- list.files("data/vic_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      # Read and combine all processed files
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$smap_processed_table <- renderDT({
    processed_files <- list.files("data/smap_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  output$grace_processed_table <- renderDT({
    processed_files <- list.files("data/grace_processed", pattern = "\\.csv$", full.names = TRUE)
    if (length(processed_files) > 0) {
      all_data <- lapply(processed_files, function(f) {
        read.csv(f)
      })
      combined_data <- do.call(rbind, all_data)
      datatable(combined_data, 
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Pre-generated Visualizations
  output$basin_huc10_snotel_viz <- renderUI({
    if (file.exists("basin_huc10_snotel_interactive.html")) {
      includeHTML("basin_huc10_snotel_interactive.html")
    }
  })
  
  output$vic_time_series_viz <- renderUI({
    if (file.exists("vic_time_series.html")) {
      includeHTML("vic_time_series.html")
    }
  })
  
  # Snow Water Equivalent Analysis
  output$swe_map <- renderPlotly({
    # Get selected date and data source
    selected_date <- input$swe_date
    selected_source <- input$swe_source
    selected_metric <- input$swe_metric
    
    # Load VIC data for the selected date
    vic_data <- load_processed_data("vic", format(selected_date, "%Y"))
    if (!is.null(vic_data)) {
      vic_data <- vic_data %>%
        filter(date == selected_date) %>%
        select(lon, lat, value = selected_metric)
    }
    
    # Load SNOTEL data for the selected date
    snotel_data <- load_processed_data("snotel")
    if (!is.null(snotel_data)) {
      snotel_data <- snotel_data %>%
        filter(date == selected_date) %>%
        select(lon = longitude, lat = latitude, value = selected_metric)
    }
    
    # Create base map
    p <- plot_ly() %>%
      layout(
        title = paste("Snow Water Equivalent -", selected_date),
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
    
    # Add VIC data if selected
    if (selected_source %in% c("vic", "both") && !is.null(vic_data)) {
      p <- p %>%
        add_trace(
          data = vic_data,
          x = ~lon,
          y = ~lat,
          z = ~value,
          type = "heatmap",
          colorscale = "Viridis",
          name = "VIC Model"
        )
    }
    
    # Add SNOTEL points if selected
    if (selected_source %in% c("snotel", "both") && !is.null(snotel_data)) {
      p <- p %>%
        add_trace(
          data = snotel_data,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 10,
            color = ~value,
            colorscale = "Viridis",
            showscale = TRUE
          ),
          name = "SNOTEL Stations"
        )
    }
    
    # Add selected layers
    if ("huc10" %in% input$swe_layers) {
      # Add HUC-10 boundaries
      p <- p %>%
        add_trace(
          data = huc10_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 1),
          name = "HUC-10 Boundaries"
        )
    }
    
    if ("subbasins" %in% input$swe_layers) {
      # Add analysis sub-basins
      p <- p %>%
        add_trace(
          data = subbasin_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "blue", width = 2),
          name = "Analysis Sub-basins"
        )
    }
    
    if ("basins" %in% input$swe_layers) {
      # Add CRB/UCRB/LCRB boundaries
      p <- p %>%
        add_trace(
          data = basin_boundaries,
          x = ~lon,
          y = ~lat,
          type = "scatter",
          mode = "lines",
          line = list(color = "red", width = 2),
          name = "CRB/UCRB/LCRB"
        )
    }
    
    p
  })
  
  output$swe_timeseries <- renderPlotly({
    # Get selected parameters
    selected_station <- input$swe_station
    date_range <- input$swe_date_range
    aggregation <- input$swe_aggregation
    
    # Load and filter data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Filter by date range
      snotel_data <- snotel_data %>%
        filter(date >= date_range[1], date <= date_range[2])
      
      vic_data <- vic_data %>%
        filter(date >= date_range[1], date <= date_range[2])
      
      # Aggregate data based on selection
      if (aggregation == "weekly") {
        snotel_data <- snotel_data %>%
          mutate(week = floor_date(date, "week")) %>%
          group_by(week) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(week = floor_date(date, "week")) %>%
          group_by(week) %>%
          summarise(value = mean(value, na.rm = TRUE))
      } else if (aggregation == "monthly") {
        snotel_data <- snotel_data %>%
          mutate(month = floor_date(date, "month")) %>%
          group_by(month) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(month = floor_date(date, "month")) %>%
          group_by(month) %>%
          summarise(value = mean(value, na.rm = TRUE))
      } else if (aggregation == "annual") {
        snotel_data <- snotel_data %>%
          mutate(year = floor_date(date, "year")) %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE))
        
        vic_data <- vic_data %>%
          mutate(year = floor_date(date, "year")) %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE))
      }
      
      # Create plot
      p <- plot_ly() %>%
        layout(
          title = "Snow Water Equivalent Time Series",
          xaxis = list(title = "Date"),
          yaxis = list(title = "SWE (mm)")
        )
      
      # Add SNOTEL data
      if (selected_station == "all") {
        p <- p %>%
          add_trace(
            data = snotel_data,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "lines",
            name = "SNOTEL Average"
          )
      } else {
        # Add individual station data
        for (station in unique(snotel_data$station_id)) {
          station_data <- snotel_data %>%
            filter(station_id == station)
          
          p <- p %>%
            add_trace(
              data = station_data,
              x = ~date,
              y = ~value,
              type = "scatter",
              mode = "lines",
              name = paste("SNOTEL", station)
            )
        }
      }
      
      # Add VIC data
      p <- p %>%
        add_trace(
          data = vic_data,
          x = ~date,
          y = ~value,
          type = "scatter",
          mode = "lines",
          name = "VIC Model",
          line = list(dash = "dash")
        )
      
      p
    }
  })
  
  output$swe_monthly_stats <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate monthly statistics
      snotel_monthly <- snotel_data %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(
          mean = mean(value, na.rm = TRUE),
          min = min(value, na.rm = TRUE),
          max = max(value, na.rm = TRUE)
        )
      
      vic_monthly <- vic_data %>%
        mutate(month = month(date)) %>%
        group_by(month) %>%
        summarise(
          mean = mean(value, na.rm = TRUE),
          min = min(value, na.rm = TRUE),
          max = max(value, na.rm = TRUE)
        )
      
      # Create plot
      plot_ly() %>%
        add_trace(
          data = snotel_monthly,
          x = ~month,
          y = ~mean,
          type = "scatter",
          mode = "lines+markers",
          name = "SNOTEL Mean",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = ~max - ~mean,
            arrayminus = ~mean - ~min
          )
        ) %>%
        add_trace(
          data = vic_monthly,
          x = ~month,
          y = ~mean,
          type = "scatter",
          mode = "lines+markers",
          name = "VIC Mean",
          line = list(dash = "dash")
        ) %>%
        layout(
          title = "Monthly Snow Water Equivalent Statistics",
          xaxis = list(title = "Month"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_elevation_stats <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    
    if (!is.null(snotel_data)) {
      # Create elevation vs SWE plot
      plot_ly(
        data = snotel_data,
        x = ~elevation,
        y = ~value,
        type = "scatter",
        mode = "markers",
        text = ~paste("Station:", station_id, "<br>Elevation:", elevation, "m"),
        marker = list(
          size = 8,
          color = ~value,
          colorscale = "Viridis",
          showscale = TRUE
        )
      ) %>%
        layout(
          title = "Elevation vs Snow Water Equivalent",
          xaxis = list(title = "Elevation (m)"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_trend_analysis <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate annual trends
      snotel_trend <- snotel_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        mutate(source = "SNOTEL")
      
      vic_trend <- vic_data %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        mutate(source = "VIC")
      
      # Combine data
      trend_data <- bind_rows(snotel_trend, vic_trend)
      
      # Create plot
      plot_ly(
        data = trend_data,
        x = ~year,
        y = ~value,
        color = ~source,
        type = "scatter",
        mode = "lines+markers"
      ) %>%
        layout(
          title = "Annual Snow Water Equivalent Trends",
          xaxis = list(title = "Year"),
          yaxis = list(title = "SWE (mm)")
        )
    }
  })
  
  output$swe_model_comparison <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Create scatter plot comparing VIC and SNOTEL
      plot_ly(
        data = snotel_data,
        x = ~value,
        y = ~vic_value,
        type = "scatter",
        mode = "markers",
        text = ~paste("Station:", station_id, "<br>Date:", date),
        marker = list(
          size = 8,
          color = ~elevation,
          colorscale = "Viridis",
          showscale = TRUE
        )
      ) %>%
        add_trace(
          x = c(0, max(snotel_data$value, na.rm = TRUE)),
          y = c(0, max(snotel_data$value, na.rm = TRUE)),
          type = "scatter",
          mode = "lines",
          line = list(dash = "dash", color = "black"),
          name = "1:1 Line"
        ) %>%
        layout(
          title = "VIC vs SNOTEL SWE Comparison",
          xaxis = list(title = "SNOTEL SWE (mm)"),
          yaxis = list(title = "VIC SWE (mm)")
        )
    }
  })
  
  output$swe_validation <- renderPlotly({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate validation metrics
      validation_data <- snotel_data %>%
        left_join(vic_data, by = c("date", "station_id")) %>%
        mutate(
          error = vic_value - value,
          abs_error = abs(error),
          rel_error = error / value * 100
        )
      
      # Create error distribution plot
      plot_ly(
        data = validation_data,
        x = ~error,
        type = "histogram",
        nbinsx = 30
      ) %>%
        layout(
          title = "VIC Model Error Distribution",
          xaxis = list(title = "Error (mm)"),
          yaxis = list(title = "Count")
        )
    }
  })
  
  output$swe_stats_table <- renderDataTable({
    # Load and process data
    snotel_data <- load_processed_data("snotel")
    vic_data <- load_processed_data("vic")
    
    if (!is.null(snotel_data) && !is.null(vic_data)) {
      # Calculate statistics
      stats <- snotel_data %>%
        left_join(vic_data, by = c("date", "station_id")) %>%
        summarise(
          `Mean Error (mm)` = mean(vic_value - value, na.rm = TRUE),
          `Mean Absolute Error (mm)` = mean(abs(vic_value - value), na.rm = TRUE),
          `Root Mean Square Error (mm)` = sqrt(mean((vic_value - value)^2, na.rm = TRUE)),
          `Correlation Coefficient` = cor(value, vic_value, use = "complete.obs"),
          `Bias (%)` = mean((vic_value - value) / value * 100, na.rm = TRUE)
        )
      
      # Format table
      datatable(
        stats,
        options = list(
          dom = 't',
          pageLength = 5
        )
      )
    }
  })
  
  # April 1 SWE Anomalies Plot
  output$april1_swe_anomalies_plot <- renderPlotly({
    # Calculate April 1 SWE anomalies
    april1_swe <- swe_data %>%
      filter(month == 4, day == 1) %>%
      group_by(year) %>%
      summarise(mean_swe = mean(swe_value, na.rm = TRUE)) %>%
      mutate(anomaly = mean_swe - mean(mean_swe, na.rm = TRUE))
    
    # Create the plot
    plot_ly(april1_swe, x = ~year, y = ~anomaly, type = 'bar',
            marker = list(color = ~ifelse(anomaly >= 0, 'rgb(0, 128, 255)', 'rgb(255, 0, 0)'))) %>%
      layout(title = "April 1 SWE Anomalies",
             xaxis = list(title = "Year"),
             yaxis = list(title = "SWE Anomaly (mm)"),
             showlegend = FALSE)
  })
  
  # April 1 SWE Trends Plot
  output$april1_swe_trends_plot <- renderPlotly({
    # Calculate April 1 SWE trends
    april1_swe <- swe_data %>%
      filter(month == 4, day == 1) %>%
      group_by(year) %>%
      summarise(mean_swe = mean(swe_value, na.rm = TRUE))
    
    # Fit linear trend
    trend <- lm(mean_swe ~ year, data = april1_swe)
    
    # Create the plot
    plot_ly(april1_swe, x = ~year, y = ~mean_swe, type = 'scatter', mode = 'markers',
            marker = list(color = 'rgb(0, 128, 255)')) %>%
      add_lines(x = ~year, y = fitted(trend), line = list(color = 'rgb(255, 0, 0)')) %>%
      layout(title = "April 1 SWE Trends",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Mean SWE (mm)"))
  })
}

# Run the application with increased memory limit
options(shiny.maxRequestSize = 80000*1024^2)  # Increase to 80GB
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE, port = 3839)

source("deploy.R")
