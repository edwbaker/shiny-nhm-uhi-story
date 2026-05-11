library(shiny)
library(shinynhm)
library(plotly)

palette <- "default"
cols    <- nhm_colours(palette)

demo_data_path <- function(filename) {
  candidates <- c(
    file.path("data", filename),
    file.path("inst", "demo-heat", "data", filename)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }

  stop("Could not locate demo data file: ", filename, call. = FALSE)
}

demo_data_dir <- function() {
  candidates <- c(
    "data",
    file.path("inst", "demo-heat", "data")
  )
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }

  stop("Could not locate demo data directory.", call. = FALSE)
}

# ── Load data ───────────────────────────────────────────────────
heat <- readRDS(file.path("data", "heat_cities.rds"))

format_scenario_label <- function(x) {
  out <- tools::toTitleCase(gsub("_", " ", x))

  historical_idx <- !is.na(x) & x == "historical"
  out[historical_idx] <- "Historical"

  rcp_idx <- !is.na(x) & grepl("^rcp[0-9]+$", x)
  if (any(rcp_idx)) {
    digits <- sub("^rcp", "", x[rcp_idx])
    out[rcp_idx] <- paste0(
      "RCP ", substr(digits, 1, 1), ".", substr(digits, 2, nchar(digits))
    )
  }

  ssp_idx <- !is.na(x) & grepl("^ssp[0-9]+$", x)
  if (any(ssp_idx)) {
    digits <- sub("^ssp", "", x[ssp_idx])
    out[ssp_idx] <- paste0(
      "SSP", substr(digits, 1, 1), "-",
      substr(digits, 2, nchar(digits) - 1), ".",
      substr(digits, nchar(digits), nchar(digits))
    )
  }

  out
}

format_pathway_label <- function(x) {
  out <- tools::toTitleCase(gsub("_", " ", x))
  out[x == "cmip5_rcp85"] <- "CMIP5 RCP 8.5 (Historical + RCP 8.5)"
  out[x == "cmip6_ssp585"] <- "CMIP6 SSP5-8.5 (Historical + SSP5-8.5)"
  out
}

preferred_pathways <- c("cmip5_rcp85", "cmip6_ssp585")
pathway_levels <- unique(heat$pathway)
pathway_levels <- c(
  preferred_pathways[preferred_pathways %in% pathway_levels],
  sort(setdiff(pathway_levels, preferred_pathways))
)
pathway_choices <- stats::setNames(
  pathway_levels,
  vapply(pathway_levels, format_pathway_label, character(1))
)

# ── Load UK3 raw city-model annual heat metrics ───────────────
uk3_heat <- readRDS(file.path("data", "c40_uk3_city_model_annual_heat_metrics_raw.rds"))

uk3_city_levels <- sort(unique(uk3_heat$city_name))
if (length(uk3_city_levels) > 3) uk3_city_levels <- uk3_city_levels[1:3]
uk3_heat     <- uk3_heat[uk3_heat$city_name %in% uk3_city_levels, ]
uk3_year_min <- min(uk3_heat$year, na.rm = TRUE)
uk3_year_max <- max(uk3_heat$year, na.rm = TRUE)
uk3_has_data <- nrow(uk3_heat) > 0

# ── Load station data ───────────────────────────────────────────
stations    <- readRDS(file.path("data", "met_office_stations.rds"))
sensors     <- readRDS(file.path("data", "nhm_sensors.rds"))
wmo_stations <- readRDS(file.path("data", "wmo_stations.rds"))

bt_default_end <- Sys.Date()
bt_default_start <- bt_default_end - 29

# ── Load GeoJSON boundary data for outline map ──────────────────
# GeoJSON files served as static assets — mapbox-gl loads them by URL
shiny::addResourcePath("geodata",
                       demo_data_dir()
)

# ── Mean distance between devices (from bounding box area / n) ───
bbox_mean_dist <- function(lat, lon) {
  R <- 6371
  lat_r <- range(lat) * pi / 180
  lon_r <- range(lon) * pi / 180
  # Spherical rectangle area
  area_km2 <- R^2 * abs(sin(lat_r[2]) - sin(lat_r[1])) *
    abs(lon_r[2] - lon_r[1])
  sqrt(area_km2 / length(lat))
}

dist_world <- round(bbox_mean_dist(wmo_stations$lat, wmo_stations$lon), 1)
dist_uk    <- round(bbox_mean_dist(stations$lat, stations$lon), 1)
dist_nhm   <- round(bbox_mean_dist(sensors$lat, sensors$lon) * 1000) # metres

# ── Log-scale positions for distance visualisation ───────────
log_x_min <- 15; log_x_max <- 205
log_min <- 1; log_max <- 5.5  # log10(10m) to log10(~316km)
log_ppd <- (log_x_max - log_x_min) / (log_max - log_min)
log_pos <- function(m) log_x_min + (log10(m) - log_min) * log_ppd
# Tick positions
tk_10m   <- round(log_pos(10), 1)
tk_100m  <- round(log_pos(100), 1)
tk_1km   <- round(log_pos(1000), 1)
tk_10km  <- round(log_pos(10000), 1)
tk_100km <- round(log_pos(100000), 1)
# Data positions
xp_nhm   <- round(log_pos(dist_nhm), 1)
xp_uk    <- round(log_pos(dist_uk * 1000), 1)
xp_world <- round(log_pos(dist_world * 1000), 1)

# ── Linear-scale positions for page 3 ───────────────────────────
lin_x_min <- 30; lin_x_max <- 1170
lin_max_km <- dist_world
lin_ppk   <- (lin_x_max - lin_x_min) / lin_max_km  # px per km
lp_nhm    <- round(lin_x_min + (dist_nhm / 1000) * lin_ppk, 1)
lp_uk     <- round(lin_x_min + dist_uk * lin_ppk, 1)
lp_world  <- round(lin_x_min + dist_world * lin_ppk, 1)
# Tick positions (linear)
lt_50km   <- round(lin_x_min + 50 * lin_ppk, 1)
lt_100km  <- round(lin_x_min + 100 * lin_ppk, 1)
lt_200km  <- round(lin_x_min + 200 * lin_ppk, 1)
lp_end    <- round(lin_x_min + lin_max_km * lin_ppk, 1)

# ── UI helpers ──────────────────────────────────────────────────
# Wrap repeated scattermapbox trace construction
add_map_trace <- function(p, data, text, colour, size, opacity,
                          visible = TRUE) {
  plotly::add_trace(p,
                    type       = "scattermapbox",
                    mode       = "markers",
                    data       = data,
                    lat        = ~lat,
                    lon        = ~lon,
                    marker     = list(size = size, color = colour, opacity = opacity),
                    text       = text,
                    hoverinfo  = "text",
                    visible    = visible,
                    showlegend = FALSE
  )
}

# ── UI ──────────────────────────────────────────────────────────
ui <- nhm_page(
  title       = "World Cities Heat Projections",
  subbrand    = "NATURAL HISTORY MUSEUM",
  description = paste0(
    "Projected hottest 90-day average daily maximum temperature across historical and emissions scenarios"
  ),
  footer  = FALSE,
  palette = palette,

  nhm_map_zoom_js(),

  nhm_flipbook(
    id = "demo",

    # ── Page 1: The Initiative ────────────────────────────────
    nhm_flipbook_page(
      title = "The Initiative",
      nhm_panel(
        title = "The Initiative",
        shiny::fluidRow(
          shiny::column(
            4,
            nhm_flip_card(
              front_title = "The Problem",
              tag = "URBAN HEAT",
              bg_image = "images/cards/urban-heat-problem.jpg",
              front = shiny::tagList(
                shiny::tags$p(class = "stat-highlight", "970 cities"),
                shiny::tags$p(
                  class = "stat-caption",
                  "projected to hit 35C average summer highs by 2050"
                )
              ),
              back = shiny::tagList(
                shiny::tags$p(
                  "As cities expand, ",
                  shiny::tags$strong("urban heat islands"),
                  " intensify - concrete and asphalt absorb solar energy,",
                  " raising temperatures by up to 10C compared to",
                  " surrounding green areas."
                ),
                shiny::tags$p(
                  "This creates cascading effects on ",
                  shiny::tags$strong("biodiversity"),
                  ", public health, and energy consumption."
                )
              )
            )
          ),
          shiny::column(
            4,
            nhm_flip_card(
              front_title = "What We're Doing",
              tag = "RESEARCH",
              bg_image = "images/cards/nhm-research-pond.jpg",
              front = shiny::tagList(
                shiny::tags$p(class = "stat-highlight", "8 million+"),
                shiny::tags$p(
                  class = "stat-caption",
                  "temperature recordings across the NHM gardens"
                )
              ),
              back = shiny::tagList(
                shiny::tags$p(
                  "A network of ",
                  shiny::tags$strong("24 sensors"),
                  " placed across the Museum's gardens records",
                  " soil and surface temperatures every 30 seconds."
                ),
                shiny::tags$ul(
                  shiny::tags$li("Soil at 15 cm depth"),
                  shiny::tags$li("Surface temperature"),
                  shiny::tags$li("Shaded vs exposed readings")
                )
              )
            )
          ),
          shiny::column(
            4,
            nhm_flip_card(
              front_title = "From Insight to Action",
              tag = "SOLUTIONS",
              bg_image = "images/cards/nhm-solutions-ecologist.jpg",
              front = shiny::tagList(
                shiny::tags$p(class = "stat-highlight", "Species level"),
                shiny::tags$p(
                  class = "stat-caption",
                  "fine-tuning strategies from plant selection to soil cover"
                )
              ),
              back = shiny::tagList(
                shiny::tags$p(
                  "Data-driven decisions help identify which ",
                  shiny::tags$strong("plant species"),
                  " and ground covers are most effective at reducing",
                  " local temperatures."
                ),
                shiny::tags$p(
                  "Results inform urban greening strategies for ",
                  shiny::tags$strong("cities worldwide"), "."
                )
              )
            )
          )
        )
      )
    ),

    # ── Page 2: Heat map timeline ─────────────────────────────
    nhm_flipbook_page(
      title = "Global Heat Map",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::selectInput(
              inputId  = "pathway",
              label    = "Climate Model Pathway",
              choices  = pathway_choices,
              selected = if ("cmip6_ssp585" %in% pathway_levels) "cmip6_ssp585" else pathway_levels[[1]]
            ),
            shiny::hr(),
            shiny::radioButtons(
              inputId  = "mode",
              label    = "Display",
              choices  = c("Temperature" = "absolute",
                           "Predicted change" = "change"),
              selected = "absolute",
              inline   = TRUE
            ),
            shiny::hr(),
            shiny::checkboxInput(
              inputId = "filter_hot",
              label   = "Only show cities ≥ 35°C",
              value   = FALSE
            ),
            shiny::hr(),
            shiny::uiOutput("year_control"),
            shiny::hr(),
            shiny::tags$p(
              class = "nhm-value-label", "SELECTED YEAR"
            ),
            shiny::tags$p(
              style = paste0(
                "font-size:2.5rem; font-weight:700; color:",
                cols$cyan, "; margin:4px 0;"
              ),
              shiny::textOutput("year_display", inline = TRUE)
            )
          ),
          nhm_panel(
            title = "Site Details",
            shiny::uiOutput("city_detail"),
            plotly::plotlyOutput("city_timeseries", height = "200px")
          ),
          nhm_panel(
            title = "Stats",
            shiny::uiOutput("year_stats")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Projected Peak Temperature by City",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("heat_map", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 3: UK3 city temperature trends ───────────────────
    nhm_flipbook_page(
      title = "UK3 Temperatures Over Time",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::sliderInput(
              inputId = "uk3_year_range",
              label = "Year range",
              min = uk3_year_min,
              max = uk3_year_max,
              value = c(uk3_year_min, uk3_year_max),
              step = 1,
              sep = ""
            ),
            shiny::checkboxInput(
              inputId = "uk3_show_models",
              label = "Show individual models",
              value = FALSE
            ),
            shiny::radioButtons(
              inputId = "uk3_trend_type",
              label = "Trend line",
              choices = c("None" = "none", "Smooth (GAM)" = "gam"),
              selected = "none"
            ),
            shiny::hr(),
            shiny::tags$p(class = "nhm-value-label", "CITIES"),
            shiny::tags$p(
              style = paste0("color:", cols$text, "; margin-top:6px;"),
              if (length(uk3_city_levels) > 0) {
                paste(uk3_city_levels, collapse = ", ")
              } else {
                "No UK3 city data available"
              }
            )
          ),
          nhm_panel(
            title = "Summary",
            shiny::uiOutput("uk3_summary")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Annual Hottest 90-Day Mean Max Temperature",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("uk3_temp_timeseries", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 4: Fly to London ─────────────────────────────────
    nhm_flipbook_page(
      title = "Scales of measurement",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Measurement Locations",
            shiny::tags$p(
              style = paste0("color:", cols$muted, ";"),
              "Zoom between three views: the whole world, the United",
              " Kingdom, and the Natural History Museum in London."
            ),
            shiny::hr(),
            shiny::actionButton(
              "fly_world", "OSCAR Stations",
              icon  = shiny::icon("globe"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 14px;",
                "font-weight:700;width:100%;margin-bottom:10px;",
                "display:flex;align-items:center;justify-content:flex-start;",
                "gap:8px;text-align:left;white-space:normal;",
                "word-break:break-word;line-height:1.2;height:auto;",
                "min-height:48px;"
              )
            ),
            shiny::actionButton(
              "fly_uk", "Met Office Stations",
              icon  = shiny::icon("map"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 14px;",
                "font-weight:700;width:100%;margin-bottom:10px;",
                "display:flex;align-items:center;justify-content:flex-start;",
                "gap:8px;text-align:left;white-space:normal;",
                "word-break:break-word;line-height:1.2;height:auto;",
                "min-height:48px;"
              )
            ),
            shiny::actionButton(
              "fly_nhm", "Urban Research Station",
              icon  = shiny::icon("building-columns"),
              class = "nhm-btn",
              style = paste0(
                "background:", cols$cyan, ";color:", cols$deep, ";",
                "border:none;border-radius:4px;padding:10px 14px;",
                "font-weight:700;width:100%;margin-bottom:10px;",
                "display:flex;align-items:center;justify-content:flex-start;",
                "gap:8px;text-align:left;white-space:normal;",
                "word-break:break-word;line-height:1.2;height:auto;",
                "min-height:48px;"
              )
            ),
            shiny::hr(),
            shiny::tags$p(class = "nhm-value-label", "CURRENT VIEW"),
            shiny::tags$p(
              style = paste0(
                "font-size:1.3rem;font-weight:700;color:",
                cols$cyan, ";margin:4px 0;"
              ),
              shiny::textOutput("fly_status", inline = TRUE)
            )
          ),
          nhm_panel(
            title = "Mean Distance Between Devices",
            shiny::HTML(sprintf('
              <svg viewBox="0 0 220 115" width="100%%"
                   xmlns="http://www.w3.org/2000/svg"
                   style="display:block;margin:0 auto;">
                <!-- Scale bar line -->
                <line x1="%s" y1="55" x2="%s" y2="55"
                  stroke="white" stroke-width="1" opacity="0.2"/>
                <!-- Major ticks: 10m, 10km, 100km -->
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">10 m</text>
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">10 km</text>
                <line x1="%s" y1="49" x2="%s" y2="61"
                  stroke="white" stroke-width="1" opacity="0.35"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="8"
                  text-anchor="middle" opacity="0.5">100 km</text>
                <!-- Minor ticks: 100m, 1km -->
                <line x1="%s" y1="51" x2="%s" y2="59"
                  stroke="white" stroke-width="0.7" opacity="0.2"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="7"
                  text-anchor="middle" opacity="0.35">100 m</text>
                <line x1="%s" y1="51" x2="%s" y2="59"
                  stroke="white" stroke-width="0.7" opacity="0.2"/>
                <text x="%s" y="73" fill="white"
                  font-family="sans-serif" font-size="7"
                  text-anchor="middle" opacity="0.35">1 km</text>
                <!-- NHM marker -->
                <line x1="%s" y1="34" x2="%s" y2="50"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="26" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="middle">NHM</text>
                <text x="%s" y="16" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="middle">%s m</text>
                <!-- Met Office marker (label below) -->
                <line x1="%s" y1="60" x2="%s" y2="76"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="88" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="middle">Met Office</text>
                <text x="%s" y="98" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="middle">%s km</text>
                <!-- World marker -->
                <line x1="%s" y1="34" x2="%s" y2="50"
                  stroke="%s" stroke-width="1.5" opacity="0.5"/>
                <circle cx="%s" cy="55" r="5"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="1.5"/>
                <text x="%s" y="26" fill="%s"
                  font-family="sans-serif" font-size="10"
                  font-weight="700" text-anchor="end">World</text>
                <text x="%s" y="16" fill="%s"
                  font-family="sans-serif" font-size="9"
                  font-weight="600" text-anchor="end">%s km</text>
                <!-- Scale annotation -->
                <text x="110" y="112" fill="white"
                  font-family="sans-serif" font-size="7.5"
                  text-anchor="middle" opacity="0.35">logarithmic scale</text>
              </svg>',
                                log_x_min, log_x_max,
                                tk_10m, tk_10m, tk_10m,
                                tk_10km, tk_10km, tk_10km,
                                tk_100km, tk_100km, tk_100km,
                                tk_100m, tk_100m, tk_100m,
                                tk_1km, tk_1km, tk_1km,
                                xp_nhm, xp_nhm, cols$lime,
                                xp_nhm, cols$lime, cols$lime,
                                xp_nhm, cols$lime,
                                xp_nhm, cols$lime, dist_nhm,
                                xp_uk, xp_uk, cols$cyan,
                                xp_uk, cols$cyan, cols$cyan,
                                xp_uk, cols$cyan,
                                xp_uk, cols$cyan, dist_uk,
                                xp_world, xp_world, cols$pink,
                                xp_world, cols$pink, cols$pink,
                                xp_world, cols$pink,
                                xp_world, cols$pink, dist_world
            ))
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Map",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("fly_map", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 5: Linear distance scale ──────────────────────────
    nhm_flipbook_page(
      title = "Linear Scale",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Mean Distance Between Devices (linear scale)",
            shiny::HTML(sprintf('
              <svg viewBox="0 0 1200 160" width="100%%"
                   xmlns="http://www.w3.org/2000/svg"
                   style="display:block;margin:0 auto;">
                <!-- Scale bar line -->
                <line x1="%s" y1="75" x2="%s" y2="75"
                  stroke="white" stroke-width="1.5" opacity="0.2"/>
                <!-- 0 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">0</text>
                <!-- 50 km tick -->
                <line x1="%s" y1="69" x2="%s" y2="81"
                  stroke="white" stroke-width="0.8" opacity="0.25"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="10"
                  text-anchor="middle" opacity="0.35">50 km</text>
                <!-- 100 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">100 km</text>
                <!-- 200 km tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <text x="%s" y="96" fill="white"
                  font-family="sans-serif" font-size="11"
                  text-anchor="middle" opacity="0.5">200 km</text>
                <!-- End tick -->
                <line x1="%s" y1="67" x2="%s" y2="83"
                  stroke="white" stroke-width="1" opacity="0.4"/>
                <!-- NHM marker (essentially at zero) -->
                <line x1="%s" y1="48" x2="%s" y2="70"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="38" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="start">NHM</text>
                <text x="%s" y="24" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="start">%s m</text>
                <!-- Met Office marker (below line) -->
                <line x1="%s" y1="80" x2="%s" y2="106"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="120" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="middle">Met Office</text>
                <text x="%s" y="134" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="middle">%s km</text>
                <!-- World marker -->
                <line x1="%s" y1="48" x2="%s" y2="70"
                  stroke="%s" stroke-width="2" opacity="0.6"/>
                <circle cx="%s" cy="75" r="7"
                  fill="%s" fill-opacity="0.85"
                  stroke="%s" stroke-width="2"/>
                <text x="%s" y="38" fill="%s"
                  font-family="sans-serif" font-size="14"
                  font-weight="700" text-anchor="end">World</text>
                <text x="%s" y="24" fill="%s"
                  font-family="sans-serif" font-size="12"
                  font-weight="600" text-anchor="end">%s km</text>
                <!-- Scale annotation -->
                <text x="600" y="155" fill="white"
                  font-family="sans-serif" font-size="10"
                  text-anchor="middle" opacity="0.35">linear scale</text>
              </svg>',
                                lin_x_min, lin_x_max,
                                lin_x_min, lin_x_min, lin_x_min,
                                lt_50km, lt_50km, lt_50km,
                                lt_100km, lt_100km, lt_100km,
                                lt_200km, lt_200km, lt_200km,
                                lp_end, lp_end,
                                lp_nhm, lp_nhm, cols$lime,
                                lp_nhm, cols$lime, cols$lime,
                                lp_nhm, cols$lime,
                                lp_nhm, cols$lime, dist_nhm,
                                lp_uk, lp_uk, cols$cyan,
                                lp_uk, cols$cyan, cols$cyan,
                                lp_uk, cols$cyan,
                                lp_uk, cols$cyan, dist_uk,
                                lp_world, lp_world, cols$pink,
                                lp_world, cols$pink, cols$pink,
                                lp_world, cols$pink,
                                lp_world, cols$pink, dist_world
            ))
          )
        )
      )
    ),

    # ── Page 6: ws-bluetooth daily heatmap ──────────────────────
    nhm_flipbook_page(
      title = "Bluetooth Daily Heatmap",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::tagList(
              shiny::tags$p(
                style = paste0("color:", cols$muted, ";"),
                "Bluetooth data loads when this page is used."
              ),
              shiny::selectInput(
                inputId = "bt_time_unit",
                label = "Time aggregation",
                choices = c(
                  "Day" = "day",
                  "Hour" = "hour",
                  "Week" = "week",
                  "Month" = "month"
                ),
                selected = "day"
              ),
              shiny::sliderInput(
                inputId = "bt_top_n",
                label = "Show top N devices",
                min = 5,
                max = 200,
                value = 50,
                step = 5
              ),
              shiny::checkboxInput(
                inputId = "bt_normalized",
                label = "Normalize per device (relative activity)",
                value = FALSE
              ),
              shiny::dateRangeInput(
                inputId = "bt_dates",
                label = "Date range",
                start = bt_default_start,
                end = bt_default_end,
                min = bt_default_start,
                max = bt_default_end
              ),
              shiny::tags$hr(),
              shiny::tags$p(
                style = paste0("color:", cols$muted, ";"),
                "Rows are devices and columns are time bins. Hover any cell for exact count."
              )
            )
          ),
          nhm_panel(
            title = "Summary",
            shiny::uiOutput("bt_summary")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Detection Heatmap (Device x Time)",
            plotly::plotlyOutput("bluetooth_heatmap", height = "650px")
          )
        )
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────
server <- function(input, output, session) {
  bt_state <- shiny::reactiveValues(
    loaded = FALSE,
    agg_hour = data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()),
    agg_day = data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()),
    agg_week = data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()),
    agg_month = data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()),
    device_totals = numeric(0),
    date_min = bt_default_start,
    date_max = bt_default_end,
    has_data = FALSE
  )

  selected_pathway <- reactive({
    shiny::req(input$pathway)
    input$pathway
  })

  pathway_heat <- reactive({
    heat[heat$pathway == selected_pathway(), ]
  })

  pathway_years <- reactive({
    years <- sort(unique(pathway_heat()$year))
    shiny::req(length(years) > 0)
    years
  })

  output$year_control <- shiny::renderUI({
    years <- pathway_years()
    selected_year <- isolate(input$year)
    if (is.null(selected_year) || !selected_year %in% years) {
      selected_year <- min(years)
    }

    nhm_timeline_input(
      inputId  = "year",
      label    = "Year",
      values   = years,
      selected = selected_year,
      interval = 600,
      palette  = palette
    )
  })

  build_time_bin <- function(datetime, time_unit) {
    switch(time_unit,
           "hour" = format(datetime, "%Y-%m-%d %H:00"),
           "day" = format(datetime, "%Y-%m-%d"),
           "week" = format(as.Date(cut(datetime, "week")), "%Y-%m-%d"),
           "month" = format(datetime, "%Y-%m"),
           stop("Unsupported time_unit")
    )
  }

  time_bin_to_date <- function(time_bin, time_unit) {
    switch(time_unit,
           "hour" = as.Date(substr(time_bin, 1, 10)),
           "day" = as.Date(time_bin),
           "week" = as.Date(time_bin),
           "month" = as.Date(paste0(time_bin, "-01")),
           stop("Unsupported time_unit")
    )
  }

  aggregate_bt_data <- function(data, time_unit) {
    data$time_bin <- build_time_bin(data$datetime, time_unit)
    agg <- aggregate(count ~ device + time_bin, data = data, FUN = sum, na.rm = TRUE)
    agg$date_bin <- time_bin_to_date(agg$time_bin, time_unit)
    agg
  }

  ensure_bluetooth_loaded <- function() {
    if (isTRUE(bt_state$loaded)) {
      return(invisible())
    }

    summary_file <- tryCatch(
      demo_data_path("bluetooth_heatmap_summary.rds"),
      error = function(e) ""
    )

    if (nzchar(summary_file) && file.exists(summary_file)) {
      summary_obj <- tryCatch(readRDS(summary_file), error = function(e) NULL)
      required_names <- c(
        "agg_hour", "agg_day", "agg_week", "agg_month",
        "device_totals", "date_min", "date_max"
      )
      if (is.list(summary_obj) && all(required_names %in% names(summary_obj))) {
        bt_state$agg_hour <- as.data.frame(summary_obj$agg_hour)
        bt_state$agg_day <- as.data.frame(summary_obj$agg_day)
        bt_state$agg_week <- as.data.frame(summary_obj$agg_week)
        bt_state$agg_month <- as.data.frame(summary_obj$agg_month)
        bt_state$device_totals <- summary_obj$device_totals
        bt_state$date_min <- as.Date(summary_obj$date_min)
        bt_state$date_max <- as.Date(summary_obj$date_max)
        bt_state$has_data <- nrow(bt_state$agg_day) > 0
        bt_state$loaded <- TRUE

        start_default <- max(bt_state$date_min, bt_state$date_max - 29)
        shiny::updateDateRangeInput(
          session,
          inputId = "bt_dates",
          start = start_default,
          end = bt_state$date_max,
          min = bt_state$date_min,
          max = bt_state$date_max
        )
        return(invisible())
      }
    }

    data_file <- tryCatch(
      demo_data_path("bluetooth.RData"),
      error = function(e) ""
    )
    if (!nzchar(data_file) || !file.exists(data_file)) {
      bt_state$loaded <- TRUE
      return(invisible())
    }

    env <- new.env(parent = emptyenv())
    loaded_names <- load(data_file, envir = env)
    if ("data" %in% loaded_names) {
      bluetooth_data <- as.data.frame(env$data)
    } else if (length(loaded_names) > 0) {
      bluetooth_data <- as.data.frame(env[[loaded_names[[1]]]])
    } else {
      bt_state$loaded <- TRUE
      return(invisible())
    }

    if (nrow(bluetooth_data) == 0) {
      bt_state$loaded <- TRUE
      return(invisible())
    }

    bluetooth_data <- bluetooth_data[
      !is.na(bluetooth_data$datetime) & !is.na(bluetooth_data$count),
      c("device", "datetime", "count")
    ]

    bt_device_totals <- sort(tapply(
      bluetooth_data$count,
      bluetooth_data$device,
      sum,
      na.rm = TRUE
    ), decreasing = TRUE)

    bt_state$agg_hour <- aggregate_bt_data(bluetooth_data, "hour")
    bt_state$agg_day <- aggregate_bt_data(bluetooth_data, "day")
    bt_state$agg_week <- aggregate_bt_data(bluetooth_data, "week")
    bt_state$agg_month <- aggregate_bt_data(bluetooth_data, "month")
    bt_state$device_totals <- bt_device_totals
    bt_state$date_min <- min(as.Date(bluetooth_data$datetime), na.rm = TRUE)
    bt_state$date_max <- max(as.Date(bluetooth_data$datetime), na.rm = TRUE)
    bt_state$has_data <- nrow(bt_state$agg_day) > 0
    bt_state$loaded <- TRUE

    summary_obj <- list(
      agg_hour = bt_state$agg_hour,
      agg_day = bt_state$agg_day,
      agg_week = bt_state$agg_week,
      agg_month = bt_state$agg_month,
      device_totals = bt_state$device_totals,
      date_min = bt_state$date_min,
      date_max = bt_state$date_max
    )

    cache_file <- if (nzchar(summary_file)) {
      summary_file
    } else {
      file.path(demo_data_dir(), "bluetooth_heatmap_summary.rds")
    }
    try(saveRDS(summary_obj, cache_file), silent = TRUE)

    start_default <- max(bt_state$date_min, bt_state$date_max - 29)
    shiny::updateDateRangeInput(
      session,
      inputId = "bt_dates",
      start = start_default,
      end = bt_state$date_max,
      min = bt_state$date_min,
      max = bt_state$date_max
    )
  }

  year_data <- reactive({
    shiny::req(input$year)
    df <- pathway_heat()
    df[df$year == input$year, ]
  })

  output$year_display <- renderText({
    input$year
  })

  is_change <- reactive({ input$mode == "change" })

  selected_city <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$pathway, {
    selected_city(NULL)
  }, ignoreInit = TRUE)

  output$heat_map <- plotly::renderPlotly({
    df <- year_data()
    if (isTRUE(input$filter_hot) && !is_change()) {
      df <- df[df$hottest_3mo_tasmax_c >= 35, ]
    }

    common <- list(
      palette       = palette,
      data          = df,
      lat           = ~lat,
      lon           = ~lon,
      show_colorbar = TRUE,
      marker_size   = 6,
      hover_size    = 14
    )

    if (is_change()) {
      p <- do.call(nhm_world_map, c(common, list(
        marker_values  = ~temp_change,
        colour_limits  = range(heat$temp_change, na.rm = TRUE),
        ramp_colours   = c("#2166AC", "#67A9CF", "#F7F7F7",
                           "#EF8A62", "#B2182B", "#67001F"),
        colorbar_title = "Change (\u00b0C)",
        label          = ~paste0(city_name, ": ",
                                 ifelse(temp_change >= 0, "+", ""),
                                 round(temp_change, 1), "\u00b0C"),
        customdata     = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Pathway: ", format_pathway_label(pathway),
          "<br>Period: ", format_scenario_label(scenario),
          "<br>Change: ", ifelse(temp_change >= 0, "+", ""),
          round(temp_change, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )))
    } else {
      p <- do.call(nhm_world_map, c(common, list(
        marker_values  = ~hottest_3mo_tasmax_c,
        colour_limits  = range(heat$hottest_3mo_tasmax_c, na.rm = TRUE),
        ramp_colours   = c("#2166AC", "#67A9CF", "#FDDBC7",
                           "#EF8A62", "#B2182B", "#67001F"),
        colorbar_title = "\u00b0C",
        label          = ~paste0(city_name, ": ",
                                 round(hottest_3mo_tasmax_c, 1), "\u00b0C"),
        customdata     = ~paste0(
          "<b>", city_name, "</b>",
          "<br>Country: ", country,
          "<br>Pathway: ", format_pathway_label(pathway),
          "<br>Period: ", format_scenario_label(scenario),
          "<br>Peak temp: ", round(hottest_3mo_tasmax_c, 1), "\u00b0C",
          "<br>Year: ", year
        )
      )))
    }

    p |>
      plotly::layout(autosize = TRUE) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }) |>
    shiny::bindCache(input$pathway, input$year, input$mode, input$filter_hot)

  shiny::observeEvent(plotly::event_data("plotly_click", source = "A"), {
    click <- plotly::event_data("plotly_click", source = "A")
    if (!is.null(click)) {
      # Find the city name from the current year's data
      df <- year_data()
      idx <- click$pointNumber + 1L
      if (idx >= 1 && idx <= nrow(df)) {
        selected_city(df$city_name[idx])
      }
    }
  })

  output$city_detail <- shiny::renderUI({
    click <- plotly::event_data("plotly_click", source = "A")
    if (is.null(click)) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "Click a city on the map."
      ))
    }
    shiny::HTML(click$customdata)
  })

  output$city_timeseries <- plotly::renderPlotly({
    city <- selected_city()
    if (is.null(city)) return(plotly::plotly_empty())

    city_df <- pathway_heat()
    city_df <- city_df[city_df$city_name == city, ]
    city_df <- city_df[order(city_df$year), ]

    y_col <- if (is_change()) "temp_change" else "hottest_3mo_tasmax_c"
    y_lab <- if (is_change()) "Change (\u00b0C)" else "\u00b0C"

    plotly::plot_ly(
      data   = city_df,
      x      = ~year,
      y      = as.formula(paste0("~", y_col)),
      type   = "scatter",
      mode   = "lines+markers",
      line   = list(color = cols$cyan, width = 2),
      marker = list(color = cols$cyan, size = 4),
      hoverinfo = "text",
      text   = ~paste0(year, ": ",
                       if (is_change()) paste0(ifelse(temp_change >= 0, "+", ""),
                                               round(temp_change, 1))
                       else round(hottest_3mo_tasmax_c, 1),
                       "\u00b0C")
    ) |>
      plotly::layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        xaxis = list(
          title = "",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        yaxis = list(
          title = y_lab,
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        margin = list(l = 40, r = 10, t = 10, b = 30),
        showlegend = FALSE
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$year_stats <- shiny::renderUI({
    df <- year_data()

    if (is_change()) {
      avg  <- round(mean(df$temp_change, na.rm = TRUE), 1)
      top  <- round(max(df$temp_change, na.rm = TRUE), 1)
      sign_avg <- ifelse(avg >= 0, "+", "")
      sign_top <- ifelse(top >= 0, "+", "")

      return(shiny::tagList(
        shiny::tags$div(
          style = "margin-bottom:10px;",
          shiny::tags$p(class = "nhm-value-label", "MEAN CHANGE"),
          shiny::tags$p(
            style = paste0("font-size:1.5rem;font-weight:700;color:",
                           cols$cyan, ";margin:2px 0;"),
            paste0(sign_avg, avg, "\u00b0C")
          )
        ),
        shiny::tags$div(
          style = "margin-bottom:10px;",
          shiny::tags$p(class = "nhm-value-label", "MAX CHANGE"),
          shiny::tags$p(
            style = paste0("font-size:1.5rem;font-weight:700;color:",
                           cols$pink, ";margin:2px 0;"),
            paste0(sign_top, top, "\u00b0C")
          )
        )
      ))
    }

    avg  <- round(mean(df$hottest_3mo_tasmax_c, na.rm = TRUE), 1)
    top  <- round(max(df$hottest_3mo_tasmax_c, na.rm = TRUE), 1)
    hot  <- sum(df$hottest_3mo_tasmax_c >= 35, na.rm = TRUE)

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "GLOBAL MEAN"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$cyan, ";margin:2px 0;"),
          paste0(avg, "\u00b0C")
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "HOTTEST CITY"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$pink, ";margin:2px 0;"),
          paste0(top, "\u00b0C")
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "CITIES \u2265 35\u00b0C"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$lime, ";margin:2px 0;"),
          hot
        )
      )
    )
  })

  uk3_filtered <- shiny::reactive({
    if (!uk3_has_data) {
      return(uk3_heat[0, ])
    }

    df <- uk3_heat
    shiny::req(input$uk3_year_range)

    df <- df[
      df$year >= input$uk3_year_range[[1]] &
        df$year <= input$uk3_year_range[[2]],
    ]
    df
  })

  output$uk3_temp_timeseries <- plotly::renderPlotly({
    if (!uk3_has_data) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            annotations = list(list(
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "UK3 source file not found",
              showarrow = FALSE,
              font = list(color = cols$muted, size = 14)
            )),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )
      )
    }

    df <- uk3_filtered()
    if (nrow(df) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::layout(
            annotations = list(list(
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "No records in this filter window",
              showarrow = FALSE,
              font = list(color = cols$muted, size = 14)
            )),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )
      )
    }

    mean_df <- stats::aggregate(
      annual_hottest90d_tasmax_c ~ city_name + scenario + year,
      data = df,
      FUN = mean,
      na.rm = TRUE
    )

    city_order <- uk3_city_levels[uk3_city_levels %in% unique(mean_df$city_name)]
    city_cols <- stats::setNames(
      c(cols$cyan, cols$lime, cols$pink)[seq_along(city_order)],
      city_order
    )

    p <- plotly::plot_ly()

    if (isTRUE(input$uk3_show_models)) {
      for (city in city_order) {
        city_df <- df[df$city_name == city, ]
        p <- p |>
          plotly::add_trace(
            data = city_df,
            x = ~year,
            y = ~annual_hottest90d_tasmax_c,
            split = ~model,
            type = "scatter",
            mode = "lines",
            line = list(color = city_cols[[city]], width = 1),
            opacity = 0.12,
            hoverinfo = "text",
            text = ~paste0(
              "<b>", city_name, "</b>",
              "<br>Scenario: ", scenario,
              "<br>Model: ", model,
              "<br>Year: ", year,
              "<br>Temp: ", round(annual_hottest90d_tasmax_c, 2), "\u00b0C"
            ),
            showlegend = FALSE,
            inherit = FALSE
          )
      }
    }

    if (input$uk3_trend_type == "gam") {
      # Fit all city GAMs first so cross-city hover comparisons are possible
      city_fits <- lapply(stats::setNames(city_order, city_order), function(city) {
        cm <- mean_df[mean_df$city_name == city, ]
        cm <- cm[order(cm$year), ]
        fit <- mgcv::gam(annual_hottest90d_tasmax_c ~ s(year), data = cm)
        list(
          data   = cm,
          fitted = as.numeric(mgcv::predict.gam(fit, newdata = cm))
        )
      })

      for (city in city_order) {
        cf       <- city_fits[[city]]
        cm       <- cf$data
        cm$trend <- cf$fitted
        other_cities <- setdiff(city_order, city)

        hover_lines <- vapply(seq_len(nrow(cm)), function(i) {
          yr   <- cm$year[[i]]
          temp <- cm$trend[[i]]
          parts <- paste0("<b>", city, "</b><br>Year: ", yr,
                          "<br>Trend: ", round(temp, 2), "\u00b0C")
          for (other in other_cities) {
            other_fitted <- city_fits[[other]]$fitted
            other_years  <- city_fits[[other]]$data$year
            diffs <- other_fitted - temp
            idx   <- which(diff(sign(diffs)) != 0)
            if (length(idx) > 0) {
              x0 <- other_years[[idx[[1]]]]
              x1 <- other_years[[idx[[1]] + 1]]
              d0 <- diffs[[idx[[1]]]]
              d1 <- diffs[[idx[[1]] + 1]]
              yr_other <- x0 - d0 * (x1 - x0) / (d1 - d0)
              diff_yr  <- round(yr_other - yr)
              sign_str <- if (diff_yr > 0) paste0("+", diff_yr) else as.character(diff_yr)
              parts <- paste0(parts, "<br>vs ", other, ": ", sign_str, " yrs")
            }
          }
          parts
        }, character(1))

        cm$hover_text <- hover_lines

        p <- p |>
          plotly::add_trace(
            data = cm,
            x = ~year,
            y = ~trend,
            type = "scatter",
            mode = "lines",
            name = city,
            line = list(color = city_cols[[city]], width = 2.5, dash = "solid"),
            hoverinfo = "text",
            text = ~hover_text,
            inherit = FALSE
          )
      }
    }

    for (city in city_order) {
      city_mean <- mean_df[mean_df$city_name == city, ]
      city_mean <- city_mean[order(city_mean$year), ]
      if (nrow(city_mean) > 0) {
        if (isTRUE(input$uk3_trend_type != "none")) {
          # already rendered above
        } else {
          p <- p |>
            plotly::add_trace(
              data = city_mean,
              x = ~year,
              y = ~annual_hottest90d_tasmax_c,
              type = "scatter",
              mode = "lines+markers",
              name = city,
              line = list(color = city_cols[[city]], width = 2.5),
              marker = list(color = city_cols[[city]], size = 5),
              hoverinfo = "text",
              text = ~paste0(
                "<b>", city_name, "</b>",
                "<br>Scenario: ", scenario,
                "<br>Year: ", year,
                "<br>Mean temp: ", round(annual_hottest90d_tasmax_c, 2), "\u00b0C"
              ),
              inherit = FALSE
            )
        }
      }
    }

    p |>
      plotly::layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        legend = list(orientation = "v", x = 1.02, y = 1, font = list(color = cols$text, size = 11)),
        margin = list(l = 60, r = 150, t = 10, b = 60),
        xaxis = list(
          title = "Year",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        yaxis = list(
          title = "Temperature (\u00b0C)",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        )
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }) |>
    shiny::bindCache(input$uk3_year_range, input$uk3_show_models, input$uk3_trend_type)

  output$uk3_summary <- shiny::renderUI({
    if (!uk3_has_data) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "The UK3 source file could not be found."
      ))
    }

    df <- uk3_filtered()
    if (nrow(df) == 0) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "No records match the current filter."
      ))
    }

    latest_year <- max(df$year, na.rm = TRUE)

    scenarios_shown <- sort(unique(df$scenario))
    scenario_labels <- vapply(scenarios_shown, format_scenario_label, character(1))
    scen_str <- paste(scenario_labels, collapse = " + ")

    series_df <- stats::aggregate(
      annual_hottest90d_tasmax_c ~ city_name + scenario + year,
      data = df,
      FUN = mean,
      na.rm = TRUE
    )
    period_start <- min(series_df$year, na.rm = TRUE)
    period_end <- max(series_df$year, na.rm = TRUE)
    city_deltas <- vapply(split(series_df, series_df$city_name), function(city_df) {
      city_df <- city_df[order(city_df$year), ]
      if (nrow(city_df) < 2) {
        return(NA_real_)
      }
      round(city_df$annual_hottest90d_tasmax_c[nrow(city_df)] - city_df$annual_hottest90d_tasmax_c[1], 1)
    }, numeric(1))
    mean_delta <- round(mean(city_deltas, na.rm = TRUE), 1)

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "LATEST YEAR"),
        shiny::tags$p(
          style = paste0("font-size:1.3rem;font-weight:700;color:", cols$cyan, ";margin:2px 0;"),
          latest_year
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "CHANGE BY CITY"),
        shiny::HTML(paste(
          vapply(names(city_deltas), function(c) {
            delta <- city_deltas[[c]]
            delta_label <- if (is.na(delta)) "N/A" else paste0(ifelse(delta >= 0, "+", ""), delta, "\u00b0C")
            paste0(
              "<div style='margin-bottom:5px; color:", cols$text, ";'>",
              "<span style='font-weight:700;'>", c, ":</span> ",
              "<span>", delta_label, "</span>",
              "</div>"
            )
          }, character(1)),
          collapse = ""
        ))
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "MEAN CHANGE ACROSS CITIES"),
        shiny::tags$p(
          style = paste0("font-size:1.3rem;font-weight:700;color:", cols$lime, ";margin:2px 0;"),
          paste0(period_start, " to ", period_end, ": ", ifelse(mean_delta >= 0, "+", ""), mean_delta, "\u00b0C")
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "SCENARIOS"),
        shiny::tags$p(
          style = paste0("color:", cols$text, ";margin:2px 0;font-size:0.9rem;"),
          scen_str
        )
      )
    )
  })

  # ── Page 6: Daily bluetooth heatmap ─────────────────────────

  bt_filtered <- shiny::reactive({
    ensure_bluetooth_loaded()
    if (!bt_state$has_data) {
      return(data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()))
    }

    shiny::req(input$bt_dates)
    d_start <- as.Date(input$bt_dates[[1]])
    d_end <- as.Date(input$bt_dates[[2]])

    agg_df <- switch(input$bt_time_unit,
                     "hour" = bt_state$agg_hour,
                     "day" = bt_state$agg_day,
                     "week" = bt_state$agg_week,
                     "month" = bt_state$agg_month,
                     bt_state$agg_day
    )

    totals <- bt_state$device_totals
    if (length(totals) == 0) {
      return(data.frame(device = character(), time_bin = character(), date_bin = as.Date(character()), count = integer()))
    }
    top_n <- min(as.integer(input$bt_top_n), length(totals))
    shown_devices <- names(totals)[seq_len(top_n)]

    agg_df <- agg_df[
      agg_df$device %in% shown_devices &
        agg_df$date_bin >= d_start & agg_df$date_bin <= d_end,
      c("device", "time_bin", "date_bin", "count")
    ]

    agg_df
  })

  build_bt_heatmap <- function(df) {
    if (nrow(df) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
               plotly::layout(
                 annotations = list(list(
                   text = "No data available for selected filters",
                   showarrow = FALSE,
                   x = 0.5,
                   y = 0.5,
                   xref = "paper",
                   yref = "paper",
                   font = list(color = cols$muted)
                 ))
               ))
    }

    devices <- sort(unique(df$device))
    time_bins <- sort(unique(df$time_bin))
    grid <- expand.grid(device = devices, time_bin = time_bins, stringsAsFactors = FALSE)
    merged <- merge(grid, df, by = c("device", "time_bin"), all.x = TRUE)
    merged$count[is.na(merged$count)] <- 0

    z <- xtabs(count ~ device + time_bin, data = merged)
    if (isTRUE(input$bt_normalized)) {
      z <- t(apply(z, 1, function(row) {
        max_val <- max(row)
        if (max_val > 0) row / max_val else row
      }))
    }

    hover_text <- outer(rownames(z), colnames(z), Vectorize(function(dev, tb) {
      v <- z[dev, tb]
      paste0(
        "<b>", dev, "</b>",
        "<br>Time: ", tb,
        if (isTRUE(input$bt_normalized)) {
          paste0("<br>Relative activity: ", sprintf("%.1f%%", 100 * v))
        } else {
          paste0("<br>Count: ", format(v, big.mark = ","))
        }
      )
    }))

    plotly::plot_ly(
      x = colnames(z),
      y = rownames(z),
      z = unname(z),
      type = "heatmap",
      text = hover_text,
      hoverinfo = "text",
      colors = c("#FFFFFF", "#FFF59D", "#FFB74D", "#E53935"),
      colorbar = list(title = if (isTRUE(input$bt_normalized)) "Relative" else "Count")
    ) |>
      plotly::layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "Time bin",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        yaxis = list(
          title = "Device",
          color = cols$muted,
          gridcolor = "rgba(255,255,255,0.08)"
        ),
        margin = list(l = 60, r = 20, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  output$bluetooth_heatmap <- plotly::renderPlotly({
    build_bt_heatmap(bt_filtered())
  })

  output$bt_summary <- shiny::renderUI({
    df <- bt_filtered()

    if (nrow(df) == 0) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "No data available for current filters."
      ))
    }

    total_count <- sum(df$count, na.rm = TRUE)
    devices_n <- length(unique(df$device))
    bins_n <- length(unique(df$time_bin))
    peak_idx <- which.max(df$count)
    peak_row <- df[peak_idx, ]

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "TOTAL DETECTIONS"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:",
                         cols$cyan, ";margin:2px 0;"
          ),
          format(total_count, big.mark = ",")
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "PEAK BIN"),
        shiny::tags$p(
          style = paste0("font-size:1.2rem;font-weight:700;color:",
                         cols$pink, ";margin:2px 0;"
          ),
          paste0(
            peak_row$device, " @ ", peak_row$time_bin,
            " (", format(peak_row$count, big.mark = ","), ")"
          )
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "HEATMAP SIZE"),
        shiny::tags$p(
          style = paste0("font-size:1.1rem;font-weight:700;color:",
                         cols$lime, ";margin:2px 0;"),
          paste0(devices_n, " devices x ", bins_n, " bins")
        )
      )
    )
  })

  # ── Page 2: Fly-to NHM ──────────────────────────────────────

  fly_view <- shiny::reactiveVal("Globe")

  output$fly_status <- shiny::renderText({ fly_view() })

  # Custom dark mapbox style — boundaries loaded by URL (async), not inline
  dark_style <- list(
    version = 8L,
    sources = list(
      `carto-dark` = list(
        type     = "raster",
        tiles    = list(
          "https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
        ),
        tileSize = 256L
      ),
      `world-110m` = list(
        type = "geojson",
        data = "geodata/world_110m.geojson"
      ),
      `world-50m` = list(
        type = "geojson",
        data = "geodata/world_50m.geojson"
      ),
      `uk-states` = list(
        type = "geojson",
        data = "geodata/united_kingdom_states.geojson"
      )
    ),
    layers  = list(
      list(id = "background", type = "background",
           paint = list(`background-color` = cols$card)),
      list(id = "world-110m-lines", type = "line",
           source = "world-110m",
           maxzoom = 4L,
           paint = list(`line-color` = cols$cyan,
                        `line-width` = 0.8,
                        `line-opacity` = 0.7)),
      list(id = "world-50m-lines", type = "line",
           source = "world-50m",
           minzoom = 4L, maxzoom = 7L,
           paint = list(`line-color` = cols$cyan,
                        `line-width` = 0.8,
                        `line-opacity` = 0.7)),
      list(id = "uk-states-lines", type = "line",
           source = "uk-states",
           minzoom = 7L,
           paint = list(`line-color` = cols$blue,
                        `line-width` = 1,
                        `line-opacity` = 0.7)),
      list(id = "carto-tiles", type = "raster",
           source = "carto-dark",
           minzoom = 11L,
           paint = list(`raster-opacity` = 1))
    )
  )

  # Mapbox outline map with NHM-themed GeoJSON boundaries

  output$fly_map <- plotly::renderPlotly({
    p <- plotly::plot_ly(
      # Trace 0: Met Office stations (hidden initially)
      type       = "scattermapbox",
      mode       = "markers",
      data       = stations,
      lat        = ~lat,
      lon        = ~lon,
      marker     = list(size = 6, color = cols$cyan, opacity = 0.8),
      text       = ~paste0(station_name, " (", type, ")"),
      hoverinfo  = "text",
      visible    = FALSE,
      showlegend = FALSE
    ) |>
      # Trace 1: NHM sensors (hidden initially)
      add_map_trace(sensors,
                    text    = ~sensor_name,
                    colour  = cols$lime, size = 8, opacity = 0.9,
                    visible = FALSE
      ) |>
      # Trace 2: WMO stations (visible initially — world view)
      add_map_trace(wmo_stations,
                    text    = ~station_name,
                    colour  = cols$pink, size = 3, opacity = 0.6,
                    visible = TRUE
      )

    nhm_plotly_layout(p,
                      palette = palette,
                      mapbox = list(
                        style  = dark_style,
                        zoom   = 1,
                        center = list(lat = 20, lon = 0)
                      ),
                      margin = list(l = 0, r = 0, t = 0, b = 0)
    ) |>
      plotly::layout(autosize = TRUE) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # Helper: toggle trace visibility via plotly proxy
  fly_proxy <- NULL
  shiny::observe({
    fly_proxy <<- plotly::plotlyProxy("fly_map", session)
  })

  make_show_fn <- function(idx) {
    function(visible) {
      plotly::plotlyProxyInvoke(fly_proxy, "restyle",
                                list(visible = visible), list(idx))
    }
  }
  show_stations <- make_show_fn(0L)
  show_sensors  <- make_show_fn(1L)
  show_wmo      <- make_show_fn(2L)

  # Generic fly-to handler
  do_fly_to <- function(view_name, lat, lon, zoom, show_fn, hide_fns) {
    fly_view(view_name)
    plotly::plotlyProxyInvoke(fly_proxy, "relayout", list(autosize = TRUE))
    show_fn(TRUE)
    nhm_map_flyto(session, "fly_map", lat = lat, lon = lon, zoom = zoom,
                  duration = 3500)
    later::later(function() {
      for (f in hide_fns) f(FALSE)
      plotly::plotlyProxyInvoke(fly_proxy, "relayout", list(autosize = TRUE))
    }, delay = 3.5)
  }

  # World view
  shiny::observeEvent(input$fly_world, {
    do_fly_to("World", lat = 20, lon = 0, zoom = 1,
              show_fn   = show_wmo,
              hide_fns  = list(show_stations, show_sensors))
  })

  # UK view
  shiny::observeEvent(input$fly_uk, {
    do_fly_to("Met Office Stations", lat = 54.5, lon = -3, zoom = 4.8,
              show_fn   = show_stations,
              hide_fns  = list(show_sensors, show_wmo))
  })

  # NHM view
  shiny::observeEvent(input$fly_nhm, {
    do_fly_to("Urban Research Station", lat = 51.4965, lon = -0.1764, zoom = 17,
              show_fn   = show_sensors,
              hide_fns  = list(show_stations, show_wmo))
  })

  # London data for info panel
  output$london_info <- shiny::renderUI({
    london <- pathway_heat()
    london <- london[london$city_name == "London", ]
    if (nrow(london) == 0) {
      return(shiny::tags$p("London not found in dataset."))
    }
    years <- pathway_years()
    now  <- london[london$year == min(years), ]
    last <- london[london$year == max(years), ]
    shiny::tagList(
      shiny::tags$p(paste0(
        "Pathway: ", format_pathway_label(selected_pathway())
      )),
      shiny::tags$p(paste0(
        "Peak temp ", min(years), ": ",
        round(now$hottest_3mo_tasmax_c, 1), "\u00b0C"
      )),
      shiny::tags$p(paste0(
        "Peak temp ", max(years), ": ",
        round(last$hottest_3mo_tasmax_c, 1), "\u00b0C"
      )),
      shiny::tags$p(
        style = paste0("color:", cols$pink, ";"),
        paste0("Change: +",
               round(last$hottest_3mo_tasmax_c - now$hottest_3mo_tasmax_c, 1),
               "\u00b0C")
      )
    )
  })
}

shinyApp(ui, server)
