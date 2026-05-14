library(shiny)
library(shinynhm)
library(plotly)

palette <- "default"
cols    <- nhm_colours(palette)

demo_data_dir <- function() {
  file.path("data")
}

# ── Load core city heat data ───────────────────────────────────
heat <- readRDS(file.path("data", "heat_cities.rds"))
pnas_table1 <- read.csv(
  file.path("data", "pnas_2304663120_table1.csv")
)

parse_numeric_field <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  ok <- nzchar(x)
  if (any(ok)) {
    pieces <- strsplit(x[ok], "-", fixed = TRUE)
    out[ok] <- vapply(pieces, function(p) {
      vals <- suppressWarnings(as.numeric(p))
      if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
    }, numeric(1))
  }
  out
}

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
wmo_stations <- readRDS(file.path("data", "wmo_stations.rds"))

# ── Soil sensor locations (DS18B20) ─────────────────────────────
soil_locs <- list(
  "28-00000f9d74ea" = c(51.495769, -0.177637),
  "28-00000f9cf161" = c(51.495584, -0.178054),
  "28-00000f9d3348" = c(),
  "28-00000f9d0f1c" = c(51.495990, -0.178097),
  "28-00000f9c7e6a" = c(51.495980, -0.178801),
  "28-00000f9cf708" = c(51.496000, -0.178425),
  "28-00000f9e7522" = c(51.495615, -0.177593),
  "28-00000f9db933" = c(51.496100, -0.177929),
  "28-00000fa3cd99" = c(51.495755, -0.177402),
  "28-000010491907" = c(51.49621, -0.17819),
  "28-00000fa3f840" = c(51.495774, -0.178360),
  "28-00000fa44683" = c(51.495990, -0.178255),
  "28-00000f9cc2ca" = c(51.495484, -0.178638),
  "28-0000104906cc" = c(51.495606, -0.177782),
  "28-00000ff8e011" = c(51.495755, -0.177037),
  "28-00000fa659d0" = c(51.495719, -0.178035),
  "28-00000fa45dad" = c(51.495665, -0.177136),
  "28-00000ff81fd6" = c(51.495671, -0.178034)
)
valid_soil_locs <- vapply(soil_locs, length, integer(1)) == 2L
sensors <- data.frame(
  sensor_name = names(soil_locs)[valid_soil_locs],
  lat = vapply(soil_locs[valid_soil_locs], `[[`, numeric(1), 1),
  lon = vapply(soil_locs[valid_soil_locs], `[[`, numeric(1), 2)
)

concrete_path <- list(
  "10cm from path" = "28-00000f9cf708",
  "20cm from path" = "28-00000f9dc5e7",
  "30cm from path" = "28-00000f9d2349"
)


# ── Initial map view to cover all soil sensor locations ───────────
soil_map_center <- list(
  lat = mean(range(sensors$lat)),
  lon = mean(range(sensors$lon))
)
soil_map_zoom <- (function() {
  lon_span   <- diff(range(sensors$lon))
  lat_span   <- diff(range(sensors$lat))
  lat_as_lon <- lat_span / cos(soil_map_center$lat * pi / 180)
  max_span   <- max(lon_span, lat_as_lon)
  floor(log2(360 / max_span)) - 1L
})()

# ── Load NHM sensor time series (DS18B20 hourly max, pre-aggregated) ────
nhm_env <- new.env()
load(file.path("data", "nhm_sensors.RData"), envir = nhm_env)
ds18b20_hourly <- nhm_env$ds18b20
ds18b20_hourly <- ds18b20_hourly[!is.na(ds18b20_hourly$value) & ds18b20_hourly$value < 80, ]
sensor_hour_range <- range(ds18b20_hourly$hour_dt)

# Hour with the greatest difference between sensors ...7e6a and ...0f1c (default for slider)
sensor_peak_hour <- local({
  s1 <- ds18b20_hourly[ds18b20_hourly$sensor_id == "28-00000f9c7e6a", c("hour_dt", "value")]
  s2 <- ds18b20_hourly[ds18b20_hourly$sensor_id == "28-00000f9d0f1c", c("hour_dt", "value")]
  h  <- merge(s1, s2, by = "hour_dt", suffixes = c("_a", "_b"))
  h$diff <- abs(h$value_a - h$value_b)
  h$hour_dt[which.max(h$diff)]
})

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

# ── Compact linear-scale positions for page 4 card ───────────
card_x_min <- 15; card_x_max <- 205
card_max_km <- max(dist_world, 1)
card_ppk <- (card_x_max - card_x_min) / card_max_km
cp_nhm <- round(card_x_min + (dist_nhm / 1000) * card_ppk, 1)
cp_uk <- round(card_x_min + dist_uk * card_ppk, 1)
cp_world <- round(card_x_min + dist_world * card_ppk, 1)
# Tick positions and labels
ct_0 <- card_x_min
ct_half <- round(card_x_min + 0.5 * (card_x_max - card_x_min), 1)
ct_end <- card_x_max
ct_half_label <- round(card_max_km / 2)
ct_end_label <- round(card_max_km)

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
  title       = "Investigating Soil Temperature",
  subbrand    = "NHM Living Labs",
  footer  = FALSE,
  palette = palette,

  nhm_map_zoom_js(),

  # Keep soil-time slider labels compact so they do not overflow the left panel.
  shiny::tags$style(shiny::HTML(
    "#soil_time .irs-min, #soil_time .irs-max, #soil_time .irs-from, #soil_time .irs-to, #soil_time .irs-single {font-size: 10px;}"
  )),

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
              value   = TRUE
            ),
            shiny::hr(),
            shiny::uiOutput("year_control"),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::tags$p(class = "nhm-value-label", "SELECTED YEAR"),
                shiny::tags$p(
                  style = paste0(
                    "font-size:2.5rem; font-weight:700; color:",
                    cols$cyan, "; margin:4px 0;"
                  ),
                  shiny::textOutput("year_display", inline = TRUE)
                )
              ),
              shiny::column(
                6,
                shiny::tags$p(
                  class = "nhm-value-label",
                  paste0("CITIES \u2265 35\u00b0C")
                ),
                shiny::tags$p(
                  style = paste0(
                    "font-size:2.5rem; font-weight:700; color:",
                    cols$lime, "; margin:4px 0;"
                  ),
                  shiny::textOutput("year_hot_cities", inline = TRUE)
                )
              )
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
            shiny::uiOutput("fly_scale")
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

    # ── Page 5: Soil temperature map ─────────────────────────────
    nhm_flipbook_page(
      title = "Soil Temperatures",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::uiOutput("soil_time_control"),
            shiny::hr(),
            shiny::tags$p(class = "nhm-value-label", "SELECTED TIME (UTC)"),
            shiny::tags$p(
              style = paste0(
                "font-size:1.1rem;font-weight:700;color:",
                cols$cyan, ";margin:4px 0;"
              ),
              shiny::textOutput("soil_time_display", inline = TRUE)
            )
          ),
          nhm_panel(
            title = "Temperature Summary",
            shiny::uiOutput("soil_temp_summary")
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Soil Temperature by Sensor",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("soil_map", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 6: Concrete Path Distances ───────────────────────────
    nhm_flipbook_page(
      title = "Classic urban heat island",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Classic urban heat island",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("concrete_depth_plot", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 7: 30cm > 10cm example ───────────────────────────────
    nhm_flipbook_page(
      title = "Thermal damping",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Thermal damping",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("cold_effect_plot", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 8: Soil biodiversity shares ─────────────────────────
    nhm_flipbook_page(
      title = "Soil biodiversity shares",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Soil biodiversity by major group",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("soil_biodiv_pies", height = "100%")
            ),
            shiny::tags$p(
              style = paste0("margin-top:10px;font-size:1.05rem;color:", cols$muted, ";"),
              "Source: Anthony MA, Bender SF, van der Heijden MGA (2023). ",
              shiny::tags$em("Enumerating soil biodiversity"),
              ". PNAS 120(33):e2304663120. DOI: ",
              shiny::tags$a(
                href = "https://doi.org/10.1073/pnas.2304663120",
                target = "_blank",
                rel = "noopener noreferrer",
                "10.1073/pnas.2304663120"
              )
            )
          )
        )
      )
    ),

    # ── Page 9: All-species in-soil summary ────────────────────────
    nhm_flipbook_page(
      title = "How much of all life lives in soil?",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Share of all species living in soil",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("soil_total_donut", height = "100%")
            ),
            shiny::tags$p(
              style = paste0("margin-top:10px;font-size:1.05rem;color:", cols$muted, ";"),
              "Excluding phage. Source: Anthony MA, Bender SF, van der Heijden MGA (2023). ",
              shiny::tags$em("Enumerating soil biodiversity"),
              ". PNAS 120(33):e2304663120."
            )
          )
        )
      )
    ),

    # ── Page 10: Tree climate suitability ────────────────────────
    nhm_flipbook_page(
      title = "Future climate suitability of garden trees",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            shiny::div(
              style = "height: clamp(420px, 70vh, 760px);display:flex;align-items:center;justify-content:center;",
              shiny::tags$img(
                src = "images/cards/future-climate-suitability.png",
                alt = "Future climate suitability of NHM garden trees",
                style = "max-height:100%;max-width:100%;width:auto;height:auto;display:block;object-fit:contain;"
              )
            )
          )
        )
      )
    ),

    # ── Page 10: Accumulated degree days in 2025 ─────────────────
    nhm_flipbook_page(
      title = "Concrete heat accumulation",
      shiny::fluidRow(
        shiny::column(
          3,
          nhm_panel(
            title = "Controls",
            shiny::sliderInput(
              inputId = "add_base_temp",
              label = "Base temperature (°C)",
              min = 0,
              max = 20,
              value = 5,
              step = 0.5,
              sep = ""
            ),
            shiny::tags$p(
              style = paste0("margin-top:0.75rem;color:", cols$muted, ";"),
              "Hourly temperatures above this threshold are accumulated into degree days."
            ),
            shiny::checkboxInput(
              inputId = "add_show_aphid_gens",
              label = "Cabbage root fly generation markers",
              value = FALSE
            )
          )
        ),
        shiny::column(
          9,
          nhm_panel(
            title = "Accumulated degree days (2025)",
            shiny::div(
              style = "height: clamp(360px, 62vh, 650px);",
              plotly::plotlyOutput("concrete_add_plot", height = "100%")
            )
          )
        )
      )
    ),

    # ── Page 11: Next steps ─────────────────────────────────────
    nhm_flipbook_page(
      title = "Next steps",
      shiny::fluidRow(
        shiny::column(
          12,
          nhm_panel(
            title = "Next steps",
            shiny::tags$div(
              style = "padding: 1rem 0.25rem 0.25rem 0.25rem;",
              shiny::tags$ol(
                style = paste0(
                  "margin: 0; padding-left: 1.4rem; font-size: 1.8rem; ",
                  "line-height: 1.7; color: ", cols$text, ";"
                ),
                shiny::tags$li("Evolution Garden"),
                shiny::tags$li("Transition to three-site working"),
                shiny::tags$li("Laboratory twins")
              )
            )
          )
        )
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Pre-register plotly click event so event_data() doesn't warn before the
  # heat_map plot is first rendered (which is deferred by bindCache).
  session$userData$plotlyShinyEventIDs <- unique(c(
    session$userData$plotlyShinyEventIDs,
    "plotly_click-A"
  ))

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

  year_data <- reactive({
    shiny::req(input$year)
    df <- pathway_heat()
    df[df$year == input$year, ]
  })

  output$year_display <- renderText({
    input$year
  })

  output$year_hot_cities <- renderText({
    df <- year_data()
    sum(df$hottest_3mo_tasmax_c >= 35, na.rm = TRUE)
  })

  is_change <- reactive({ input$mode == "change" })

  selected_city <- shiny::reactiveVal(NULL)
  selected_city_detail <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$pathway, {
    selected_city(NULL)
  }, ignoreInit = TRUE)

  output$heat_map <- plotly::renderPlotly({
    df <- year_data()
    if (isTRUE(input$filter_hot)) {
      if (is_change()) {
        # In change mode, filter to cities whose absolute temp >= 35 for that year
        abs_df <- pathway_heat()
        abs_df <- abs_df[abs_df$year == input$year, ]
        hot_cities <- abs_df$city_name[abs_df$hottest_3mo_tasmax_c >= 35]
        df <- df[df$city_name %in% hot_cities, ]
      } else {
        df <- df[df$hottest_3mo_tasmax_c >= 35, ]
      }
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

    p <- p |>
      plotly::layout(autosize = TRUE, showlegend = FALSE) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE) |>
      plotly::event_register("plotly_click")
    p
  }) |>
    shiny::bindCache(input$pathway, input$year, input$mode, input$filter_hot)

  get_heat_click <- function() {
    suppressWarnings(
      plotly::event_data("plotly_click", priority = "event")
    )
  }

  shiny::observe({
    click <- get_heat_click()
    if (is.null(click)) {
      return(invisible())
    }

    # Find the city name from the current year's data
    df <- year_data()
    idx <- click$pointNumber + 1L
    if (idx >= 1 && idx <= nrow(df)) {
      selected_city(df$city_name[idx])
      selected_city_detail(click$customdata)
    }
  })

  output$city_detail <- shiny::renderUI({
    detail_html <- selected_city_detail()
    if (is.null(detail_html)) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "Click a city on the map."
      ))
    }
    shiny::HTML(detail_html)
  })

  output$city_timeseries <- plotly::renderPlotly({
    city <- selected_city()
    if (is.null(city)) {
      return(
        plotly::plot_ly(type = "scatter", mode = "markers") |>
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = "transparent",
            plot_bgcolor  = "transparent"
          ) |>
          plotly::config(displayModeBar = FALSE, responsive = TRUE)
      )
    }

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
        plotly::plot_ly(type = "scatter", mode = "markers") |>
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
          ) |>
          plotly::config(displayModeBar = FALSE, responsive = TRUE)
      )
    }

    df <- uk3_filtered()
    if (nrow(df) == 0) {
      return(
        plotly::plot_ly(type = "scatter", mode = "markers") |>
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
          ) |>
          plotly::config(displayModeBar = FALSE, responsive = TRUE)
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

    p <- plotly::plot_ly(type = "scatter", mode = "markers")

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
                          "<br>Temp: ", round(temp, 2), "\u00b0C")
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

  # ── Page 2: Fly-to NHM ──────────────────────────────────────

  fly_view <- shiny::reactiveVal("Globe")

  output$fly_status <- shiny::renderText({ fly_view() })

  output$fly_scale <- shiny::renderUI({
    view <- fly_view()
    show_uk <- view %in% c("Met Office Stations", "Urban Research Station")
    show_nhm <- identical(view, "Urban Research Station")

    svg <- paste0(
      '<svg viewBox="0 0 220 115" width="100%" xmlns="http://www.w3.org/2000/svg" style="display:block;margin:0 auto;">',
      '<line x1="', card_x_min, '" y1="55" x2="', card_x_max, '" y2="55" stroke="white" stroke-width="1" opacity="0.2"/>',
      '<line x1="', ct_0, '" y1="49" x2="', ct_0, '" y2="61" stroke="white" stroke-width="1" opacity="0.35"/>',
      '<text x="', ct_0, '" y="73" fill="white" font-family="sans-serif" font-size="8" text-anchor="middle" opacity="0.55">0 km</text>',
      '<line x1="', ct_half, '" y1="49" x2="', ct_half, '" y2="61" stroke="white" stroke-width="1" opacity="0.3"/>',
      '<text x="', ct_half, '" y="73" fill="white" font-family="sans-serif" font-size="7" text-anchor="middle" opacity="0.45">', ct_half_label, ' km</text>',
      '<line x1="', ct_end, '" y1="49" x2="', ct_end, '" y2="61" stroke="white" stroke-width="1" opacity="0.35"/>',
      '<text x="', ct_end, '" y="73" fill="white" font-family="sans-serif" font-size="8" text-anchor="middle" opacity="0.55">', ct_end_label, ' km</text>',

      if (show_nhm) paste0(
        '<line x1="', cp_nhm, '" y1="34" x2="', cp_nhm, '" y2="50" stroke="', cols$lime, '" stroke-width="1.5" opacity="0.5"/>',
        '<circle cx="', cp_nhm, '" cy="55" r="5" fill="', cols$lime, '" fill-opacity="0.85" stroke="', cols$lime, '" stroke-width="1.5"/>',
        '<text x="', cp_nhm, '" y="26" fill="', cols$lime, '" font-family="sans-serif" font-size="10" font-weight="700" text-anchor="middle">NHM</text>',
        '<text x="', cp_nhm, '" y="16" fill="', cols$lime, '" font-family="sans-serif" font-size="9" font-weight="600" text-anchor="middle">', dist_nhm, ' m</text>'
      ) else "",

      if (show_uk) paste0(
        '<line x1="', cp_uk, '" y1="60" x2="', cp_uk, '" y2="76" stroke="', cols$cyan, '" stroke-width="1.5" opacity="0.5"/>',
        '<circle cx="', cp_uk, '" cy="55" r="5" fill="', cols$cyan, '" fill-opacity="0.85" stroke="', cols$cyan, '" stroke-width="1.5"/>',
        '<text x="', cp_uk, '" y="88" fill="', cols$cyan, '" font-family="sans-serif" font-size="10" font-weight="700" text-anchor="middle">Met Office</text>',
        '<text x="', cp_uk, '" y="98" fill="', cols$cyan, '" font-family="sans-serif" font-size="9" font-weight="600" text-anchor="middle">', dist_uk, ' km</text>'
      ) else "",

      '<line x1="', cp_world, '" y1="34" x2="', cp_world, '" y2="50" stroke="', cols$pink, '" stroke-width="1.5" opacity="0.5"/>',
      '<circle cx="', cp_world, '" cy="55" r="5" fill="', cols$pink, '" fill-opacity="0.85" stroke="', cols$pink, '" stroke-width="1.5"/>',
      '<text x="', cp_world, '" y="26" fill="', cols$pink, '" font-family="sans-serif" font-size="10" font-weight="700" text-anchor="end">World</text>',
      '<text x="', cp_world, '" y="16" fill="', cols$pink, '" font-family="sans-serif" font-size="9" font-weight="600" text-anchor="end">', dist_world, ' km</text>',
      '<text x="110" y="112" fill="white" font-family="sans-serif" font-size="7.5" text-anchor="middle" opacity="0.35">linear scale</text>',
      '</svg>'
    )

    shiny::HTML(svg)
  })

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
      # Trace 1: Urban Research Station soil locations (hidden initially)
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
  fly_visibility_seq <- 0L

  get_fly_proxy <- function() {
    plotly::plotlyProxy("fly_map", session)
  }

  set_trace_visibility <- function(target_idx, hide_delay = 0) {
    fly_proxy <- get_fly_proxy()
    fly_visibility_seq <<- fly_visibility_seq + 1L
    seq_id <- fly_visibility_seq

    # Show the new series first.
    plotly::plotlyProxyInvoke(
      fly_proxy,
      "restyle",
      list(visible = TRUE),
      list(as.integer(target_idx))
    )

    # Then hide old series after the requested delay.
    later::later(function() {
      if (fly_visibility_seq != seq_id) {
        return(invisible())
      }

      fly_proxy <- get_fly_proxy()
      for (idx in setdiff(0:2, target_idx)) {
        plotly::plotlyProxyInvoke(
          fly_proxy,
          "restyle",
          list(visible = FALSE),
          list(as.integer(idx))
        )
      }
    }, delay = hide_delay)
  }

  # Generic map-view handler (interrupt-safe)
  do_fly_to <- function(view_name, lat, lon, zoom, target_idx) {
    fly_proxy <- get_fly_proxy()
    fly_duration_s <- 1.8

    fly_view(view_name)
    # Keep previous traces visible during the fly animation, then hide them.
    set_trace_visibility(target_idx, hide_delay = fly_duration_s)

    # Keep plot responsive, then run animated fly/zoom transition.
    plotly::plotlyProxyInvoke(fly_proxy, "relayout", list(autosize = TRUE))
    nhm_map_flyto(
      session,
      "fly_map",
      lat = lat,
      lon = lon,
      zoom = zoom,
      duration = as.integer(fly_duration_s * 1000)
    )
  }

  # World view
  shiny::observeEvent(input$fly_world, {
    do_fly_to("World", lat = 20, lon = 0, zoom = 1,
              target_idx = 2L)
  })

  # UK view
  shiny::observeEvent(input$fly_uk, {
    do_fly_to("Met Office Stations", lat = 54.5, lon = -3, zoom = 4.8,
              target_idx = 0L)
  })

  # NHM view
  shiny::observeEvent(input$fly_nhm, {
    do_fly_to("Urban Research Station", lat = 51.4965, lon = -0.1764, zoom = 17,
              target_idx = 1L)
  })

  # ── Page 5: Soil temperature map ──────────────────────────────

  output$soil_time_control <- shiny::renderUI({
    shiny::sliderInput(
      inputId  = "soil_time",
      label    = "Time",
      min      = sensor_hour_range[1],
      max      = sensor_hour_range[2],
      value    = sensor_peak_hour,
      step     = 3600,
      timeFormat = "%d %b %H:%M",
      animate  = shiny::animationOptions(interval = 200, loop = FALSE),
      timezone = "+0000",
      width    = "100%"
    )
  })

  output$soil_time_display <- shiny::renderText({
    shiny::req(input$soil_time)
    format(input$soil_time, "%d %b %Y %H:00 UTC")
  })

  soil_hour_data <- shiny::reactive({
    shiny::req(input$soil_time)
    sel <- as.POSIXct(
      round(as.numeric(input$soil_time) / 3600) * 3600,
      origin = "1970-01-01", tz = "UTC"
    )
    df <- ds18b20_hourly[ds18b20_hourly$hour_dt == sel, ]
    merge(df, sensors, by.x = "sensor_id", by.y = "sensor_name", all.x = TRUE)
  })

  output$soil_map <- plotly::renderPlotly({
    df        <- soil_hour_data()
    df        <- df[!is.na(df$lat) & !is.na(df$lon) & !is.na(df$value), ]
    temp_range <- quantile(ds18b20_hourly$value, probs = c(0.02, 0.98), na.rm = TRUE)

    base_map <- function(p) {
      nhm_plotly_layout(p,
        palette = palette,
        mapbox  = list(
          style  = dark_style,
          zoom   = soil_map_zoom,
          center = soil_map_center
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      ) |>
        plotly::layout(autosize = TRUE) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }

    if (nrow(df) == 0) {
      return(base_map(plotly::plot_ly(type = "scattermapbox", mode = "markers")))
    }

    base_map(
      plotly::plot_ly() |>
        # Trace 1: colour-coded markers
        plotly::add_trace(
          inherit    = FALSE,
          data       = df,
          type       = "scattermapbox",
          mode       = "markers",
          lat        = ~lat,
          lon        = ~lon,
          marker     = list(
            size       = 26,
            color      = ~value,
            colorscale = list(
              c(0, "#2166AC"), c(0.25, "#67A9CF"), c(0.5, "#FDDBC7"),
              c(0.75, "#EF8A62"), c(1, "#B2182B")
            ),
            cmin     = temp_range[1],
            cmax     = temp_range[2],
            colorbar = list(
              title     = "\u00b0C",
              titlefont = list(color = cols$text),
              tickfont  = list(color = cols$text)
            )
          ),
          hovertext  = ~paste0(sensor_id, "<br>", round(value, 1), "\u00b0C"),
          hoverinfo  = "text",
          showlegend = FALSE
        ) |>
        # Trace 2: temperature labels (text-only, rendered above markers)
        plotly::add_trace(
          inherit      = FALSE,
          data         = df,
          type         = "scattermapbox",
          mode         = "text",
          lat          = ~lat,
          lon          = ~lon,
          text         = ~paste0(round(value, 1), "\u00b0C"),
          textposition = "middle center",
          textfont     = list(color = "#000000", size = 11,
                              family = "sans-serif"),
          hoverinfo    = "none",
          showlegend   = FALSE
        )
    )
  })

  output$soil_temp_summary <- shiny::renderUI({
    df <- soil_hour_data()
    df <- df[!is.na(df$value), ]

    if (nrow(df) == 0) {
      return(shiny::tags$p(
        style = paste0("color:", cols$muted, ";"),
        "No sensor data for selected time."
      ))
    }

    avg <- round(mean(df$value, na.rm = TRUE), 1)
    mn  <- round(min(df$value,  na.rm = TRUE), 1)
    mx  <- round(max(df$value,  na.rm = TRUE), 1)
    n   <- nrow(df)

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "MEAN TEMP"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:", cols$cyan, ";margin:2px 0;"),
          paste0(avg, "\u00b0C")
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "MIN TEMP"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:", cols$blue, ";margin:2px 0;"),
          paste0(mn, "\u00b0C")
        )
      ),
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "MAX TEMP"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:", cols$pink, ";margin:2px 0;"),
          paste0(mx, "\u00b0C")
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "ACTIVE SENSORS"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:", cols$lime, ";margin:2px 0;"),
          n
        )
      )
    )
  })
  # ── Page 6: Concrete Path Distances ─────────────────────────────────────────

  # Pre-compute: find the day with the greatest 10cm vs 30cm temperature range
  concrete_peak <- local({
    id_10 <- concrete_path[["10cm from path"]]
    id_20 <- concrete_path[["20cm from path"]]
    id_30 <- concrete_path[["30cm from path"]]

    df_10 <- ds18b20_hourly[ds18b20_hourly$sensor_id == id_10, c("hour_dt", "value")]
    df_20 <- ds18b20_hourly[ds18b20_hourly$sensor_id == id_20, c("hour_dt", "value")]
    df_30 <- ds18b20_hourly[ds18b20_hourly$sensor_id == id_30, c("hour_dt", "value")]

    # Inner-join so we only keep hours where all three sensors have readings
    hourly <- merge(df_10, df_20, by = "hour_dt", suffixes = c("_10", "_20"))
    hourly <- merge(hourly, df_30, by = "hour_dt")
    names(hourly)[names(hourly) == "value"] <- "value_30"
    hourly <- hourly[order(hourly$hour_dt), ]

    hourly$valid <- hourly$value_10 > hourly$value_20 & hourly$value_20 > hourly$value_30
    hourly$diff  <- hourly$value_10 - hourly$value_30

    # For each candidate hour, check that every point in its ±24 h window is valid
    ts <- as.numeric(hourly$hour_dt)
    window_ok <- vapply(seq_len(nrow(hourly)), function(i) {
      in_win <- abs(ts - ts[i]) <= 24 * 3600
      all(hourly$valid[in_win])
    }, logical(1))

    candidates <- hourly[window_ok, ]
    peak_hour <- candidates$hour_dt[which.max(candidates$diff)]
    max_diff  <- round(max(candidates$diff, na.rm = TRUE), 1)

    # 48 hours centred on the peak hour
    window_start <- peak_hour - 24 * 3600
    window_end   <- peak_hour + 24 * 3600

    all_ids <- unlist(concrete_path, use.names = FALSE)
    df_win  <- ds18b20_hourly[
      ds18b20_hourly$sensor_id %in% all_ids &
        ds18b20_hourly$hour_dt >= window_start &
        ds18b20_hourly$hour_dt <= window_end,
    ]

    dist_map <- data.frame(
      sensor_id = unlist(concrete_path, use.names = FALSE),
      depth     = names(concrete_path),
      stringsAsFactors = FALSE
    )
    df_win <- merge(df_win, dist_map, by = "sensor_id")
    df_win$depth <- factor(df_win$depth, levels = names(concrete_path))

    list(data = df_win, peak_hour = peak_hour, max_diff = max_diff,
         window_start = window_start, window_end = window_end)
  })

  output$concrete_depth_plot <- plotly::renderPlotly({
    df         <- concrete_peak$data
    peak_hour  <- concrete_peak$peak_hour
    win_start  <- concrete_peak$window_start
    win_end    <- concrete_peak$window_end

    depth_colours <- c(
      "10cm from path" = cols$pink,
      "20cm from path" = cols$cyan,
      "30cm from path" = cols$lime
    )

    p <- plotly::plot_ly()
    for (dep in levels(df$depth)) {
      sub <- df[df$depth == dep, ]
      sub <- sub[order(sub$hour_dt), ]
      p <- plotly::add_trace(p,
        data       = sub,
        x          = ~hour_dt,
        y          = ~value,
        name       = dep,
        type       = "scatter",
        mode       = "lines+markers",
        line       = list(color = depth_colours[[dep]], width = 2),
        marker     = list(color = depth_colours[[dep]], size = 6),
        hovertemplate = paste0("%{x|%d %b %H:00}<br>%{y:.1f}\u00b0C<extra>", dep, "</extra>")
      )
    }

    x_range <- paste0(
      format(win_start, "%d %b"), "\u2013", format(win_end, "%d %b %Y"), " (UTC)"
    )

    nhm_plotly_layout(p,
      palette = palette,
      xaxis = list(
        title = list(text = paste0("Date and hour (UTC): ", x_range), standoff = 10),
        automargin = TRUE,
        tickformat = "%d %b\n%H:00"
      ),
      yaxis  = list(
        title = list(text = "Sensor temperature (\u00b0C)", standoff = 10),
        automargin = TRUE
      ),
      legend = list(orientation = "h", y = -0.2),
      shapes = list(list(
        type    = "line",
        x0      = peak_hour, x1 = peak_hour,
        y0      = 0, y1 = 1, yref = "paper",
        line    = list(color = cols$muted, width = 1, dash = "dot")
      ))
    ) |>
      plotly::layout(
        xaxis = list(title = "Date and hour (UTC)"),
        yaxis = list(title = "Sensor temperature (\u00b0C)")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ── Page 7: 30cm > 10cm cold effect ──────────────────────────────────────

  cold_peak <- local({
    id_10 <- concrete_path[["10cm from path"]]
    id_30 <- concrete_path[["30cm from path"]]

    df_10 <- ds18b20_hourly[ds18b20_hourly$sensor_id == id_10, c("hour_dt", "value")]
    df_30 <- ds18b20_hourly[ds18b20_hourly$sensor_id == id_30, c("hour_dt", "value")]

    h <- merge(df_10, df_30, by = "hour_dt", suffixes = c("_10", "_30"))
    h <- h[h$value_30 > h$value_10, ]
    h$diff <- h$value_30 - h$value_10

    peak_hour  <- h$hour_dt[which.max(h$diff)]
    max_diff   <- round(max(h$diff, na.rm = TRUE), 1)

    window_start <- peak_hour - 24 * 3600
    window_end   <- peak_hour + 24 * 3600

    all_ids <- unlist(concrete_path, use.names = FALSE)
    df_win  <- ds18b20_hourly[
      ds18b20_hourly$sensor_id %in% all_ids &
        ds18b20_hourly$hour_dt >= window_start &
        ds18b20_hourly$hour_dt <= window_end,
    ]

    dist_map <- data.frame(
      sensor_id = unlist(concrete_path, use.names = FALSE),
      depth     = names(concrete_path),
      stringsAsFactors = FALSE
    )
    df_win <- merge(df_win, dist_map, by = "sensor_id")
    df_win$depth <- factor(df_win$depth, levels = names(concrete_path))

    list(data = df_win, peak_hour = peak_hour, max_diff = max_diff,
         window_start = window_start, window_end = window_end)
  })

  output$cold_peak_info <- shiny::renderUI({
    ph <- cold_peak$peak_hour
    md <- cold_peak$max_diff

    shiny::tagList(
      shiny::tags$div(
        style = "margin-bottom:10px;",
        shiny::tags$p(class = "nhm-value-label", "PEAK HOUR (UTC)"),
        shiny::tags$p(
          style = paste0("font-size:1.2rem;font-weight:700;color:", cols$cyan, ";margin:2px 0;"),
          format(ph, "%d %b %Y %H:00")
        )
      ),
      shiny::tags$div(
        shiny::tags$p(class = "nhm-value-label", "MAX DIFF (30\u00a0cm \u2212 10\u00a0cm)"),
        shiny::tags$p(
          style = paste0("font-size:1.5rem;font-weight:700;color:", cols$lime, ";margin:2px 0;"),
          paste0(md, "\u00b0C")
        )
      )
    )
  })

  output$cold_effect_plot <- plotly::renderPlotly({
    df        <- cold_peak$data
    peak_hour <- cold_peak$peak_hour
    win_start <- cold_peak$window_start
    win_end   <- cold_peak$window_end

    depth_colours <- c(
      "10cm from path" = cols$pink,
      "20cm from path" = cols$cyan,
      "30cm from path" = cols$lime
    )

    p <- plotly::plot_ly()
    for (dep in levels(df$depth)) {
      sub <- df[df$depth == dep, ]
      sub <- sub[order(sub$hour_dt), ]
      p <- plotly::add_trace(p,
        data       = sub,
        x          = ~hour_dt,
        y          = ~value,
        name       = dep,
        type       = "scatter",
        mode       = "lines+markers",
        line       = list(color = depth_colours[[dep]], width = 2),
        marker     = list(color = depth_colours[[dep]], size = 6),
        hovertemplate = paste0("%{x|%d %b %H:00}<br>%{y:.1f}\u00b0C<extra>", dep, "</extra>")
      )
    }

    x_range <- paste0(
      format(win_start, "%d %b"), "\u2013", format(win_end, "%d %b %Y"), " (UTC)"
    )

    nhm_plotly_layout(p,
      palette = palette,
      xaxis = list(
        title = list(text = paste0("Date and hour (UTC): ", x_range), standoff = 10),
        automargin = TRUE,
        tickformat = "%d %b\n%H:00"
      ),
      yaxis  = list(
        title = list(text = "Sensor temperature (\u00b0C)", standoff = 10),
        automargin = TRUE
      ),
      legend = list(orientation = "h", y = -0.2),
      shapes = list(list(
        type = "line",
        x0   = peak_hour, x1 = peak_hour,
        y0   = 0, y1 = 1, yref = "paper",
        line = list(color = cols$muted, width = 1, dash = "dot")
      ))
    ) |>
      # Apply explicit axis titles after theme helper so defaults cannot override.
      plotly::layout(
        xaxis = list(title = "Date and hour (UTC)"),
        yaxis = list(title = "Sensor temperature (\u00b0C)")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ── Page 8: Accumulated degree days in 2025 ─────────────────────────────

  shiny::observeEvent(input$add_show_aphid_gens, {
    if (isTRUE(input$add_show_aphid_gens) && !isTRUE(all.equal(input$add_base_temp, 5))) {
      shiny::updateSliderInput(session, "add_base_temp", value = 5)
    }
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$add_base_temp, {
    if (isTRUE(input$add_show_aphid_gens) && !isTRUE(all.equal(input$add_base_temp, 5))) {
      shiny::updateSliderInput(session, "add_base_temp", value = 5)
    }
  }, ignoreInit = TRUE)

  output$concrete_add_plot <- plotly::renderPlotly({
    base_temp <- if (isTRUE(input$add_show_aphid_gens)) {
      5
    } else if (is.null(input$add_base_temp)) {
      0
    } else {
      input$add_base_temp
    }

    page8_sensors <- c(
      "DC Courtyard" = "28-00000f9d0f1c",
      "Chalk grassland hill" = "28-00000f9d74ea",
      "Woodland" = "28-00000f9c7e6a"
    )
    all_ids <- unlist(page8_sensors, use.names = FALSE)
    df_2025 <- ds18b20_hourly[
      ds18b20_hourly$sensor_id %in% all_ids &
        format(ds18b20_hourly$hour_dt, "%Y") == "2025",
      c("sensor_id", "hour_dt", "value")
    ]

    shiny::validate(
      shiny::need(nrow(df_2025) > 0, "No concrete sensor data available for 2025.")
    )

    dist_map <- data.frame(
      sensor_id = unlist(page8_sensors, use.names = FALSE),
      series    = names(page8_sensors),
      stringsAsFactors = FALSE
    )
    df_2025 <- merge(df_2025, dist_map, by = "sensor_id")
    df_2025$series <- factor(df_2025$series, levels = names(page8_sensors))

    # Fill missing hours by linear interpolation so cumulative curves are continuous.
    hourly_interp <- do.call(
      rbind,
      lapply(split(df_2025, df_2025$series), function(sub) {
        sub <- sub[order(sub$hour_dt), c("sensor_id", "series", "hour_dt", "value")]
        sub <- sub[!duplicated(sub$hour_dt), ]

        full_hours <- seq(min(sub$hour_dt), max(sub$hour_dt), by = "hour")

        if (nrow(sub) < 2) {
          interp_vals <- rep(sub$value[1], length(full_hours))
        } else {
          interp_vals <- stats::approx(
            x = as.numeric(sub$hour_dt),
            y = sub$value,
            xout = as.numeric(full_hours),
            method = "linear",
            rule = 2
          )$y
        }

        data.frame(
          sensor_id = sub$sensor_id[1],
          series = sub$series[1],
          hour_dt = full_hours,
          value = interp_vals,
          stringsAsFactors = FALSE
        )
      })
    )

    hourly_interp$date <- as.Date(hourly_interp$hour_dt, tz = "UTC")

    # Degree-day contribution from hourly values above the selected base temperature.
    hourly_interp$deg_day <- pmax(hourly_interp$value - base_temp, 0) / 24

    daily <- aggregate(deg_day ~ date + series, data = hourly_interp, FUN = sum, na.rm = TRUE)
    daily <- daily[order(daily$series, daily$date), ]

    daily$add_2025 <- ave(daily$deg_day, daily$series, FUN = cumsum)

    daily_lookup <- split(daily, daily$date)
    daily$hover_text <- vapply(seq_len(nrow(daily)), function(i) {
      row <- daily[i, ]
      others <- daily_lookup[[as.character(row$date)]]
      others <- others[others$series != row$series, , drop = FALSE]

      parts <- paste0(
        "<b>", row$series, "</b>",
        "<br>Date: ", format(row$date, "%d %b %Y"),
        "<br>Accumulated degree days above ", sprintf("%.1f", base_temp), "°C: ",
        sprintf("%.1f", row$add_2025), " °C·days"
      )

      if (nrow(others) > 0) {
        for (j in seq_len(nrow(others))) {
          delta <- round(row$add_2025 - others$add_2025[[j]], 1)
          delta_label <- paste0(ifelse(delta >= 0, "+", ""), delta, " °C·days")
          parts <- paste0(parts, "<br>vs ", others$series[[j]], ": ", delta_label)
        }
      }

      parts
    }, character(1))

    depth_colours <- c(
      "Chalk grassland hill" = cols$blue,
      "DC Courtyard" = "#FF8C42",
      "Woodland" = cols$lime
    )

    p <- plotly::plot_ly()
    for (dep in levels(daily$series)) {
      sub <- daily[daily$series == dep, ]
      sub <- sub[order(sub$date), ]
      p <- plotly::add_trace(p,
        data = sub,
        x = ~date,
        y = ~add_2025,
        name = dep,
        type = "scatter",
        mode = "lines",
        line = list(color = depth_colours[[dep]], width = 3),
        hoverinfo = "text",
        text = ~hover_text
      )
    }

    nhm_plotly_layout(p,
      palette = palette,
      margin = list(r = 100),
      xaxis = list(
        title = NULL,
        automargin = TRUE,
        tickformat = "%b %d",
        showticklabels = FALSE
      ),
      yaxis = list(
        title = NULL,
        automargin = TRUE,
        showticklabels = FALSE
      ),
      legend = list(orientation = "v", x = 0.02, y = 0.98, xanchor = "left", yanchor = "top"),
      shapes = if (isTRUE(input$add_show_aphid_gens)) {
        max_add <- max(daily$add_2025, na.rm = TRUE)
        gen_thresholds <- c()
        current <- 210
        while (current <= max_add) {
          gen_thresholds <- c(gen_thresholds, current)
          current <- current + 290
        }
        lapply(gen_thresholds, function(y_val) {
          list(
            type = "line",
            x0 = 0, x1 = 1, xref = "paper",
            y0 = y_val, y1 = y_val, yref = "y",
            line = list(color = cols$pink, width = 1, dash = "dash"),
            opacity = 0.6
          )
        })
      } else {
        list()
      },
      annotations = if (isTRUE(input$add_show_aphid_gens)) {
        max_add <- max(daily$add_2025, na.rm = TRUE)
        gen_thresholds <- c()
        current <- 210
        while (current <= max_add) {
          gen_thresholds <- c(gen_thresholds, current)
          current <- current + 290
        }
        lapply(seq_along(gen_thresholds), function(i) {
          list(
            x = 1.02,
            y = gen_thresholds[[i]],
            xref = "paper",
            yref = "y",
            text = paste0("Gen ", i),
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "middle",
            font = list(color = cols$pink, size = 8)
          )
        })
      } else {
        list()
      }
    ) |>
      plotly::layout(
        xaxis = list(title = list(text = "")),
        yaxis = list(title = list(text = "")),
        legend = list(x = 1.05, y = 0.98, xanchor = "left", yanchor = "top")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ── Page 9: All-species in-soil summary donut ─────────────────────────────

  output$soil_total_donut <- plotly::renderPlotly({
    in_soil <- 59
    other   <- 100 - in_soil
    plotly::plot_ly(
      labels  = c("In soil", "Other habitats"),
      values  = c(in_soil, other),
      type    = "pie",
      hole    = 0.55,
      marker  = list(
        colors = c(cols$lime, cols$muted),
        line   = list(color = "#ffffff", width = 2)
      ),
      textinfo      = "none",
      hovertemplate = "%{label}: %{value}%<extra></extra>"
    ) |>
    plotly::layout(
      annotations = list(list(
        text     = "<b>59%</b><br>in soil",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 22, color = "#ffffff")
      )),
      showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin = list(t = 40, b = 40, l = 60, r = 60)
    ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ── Page 10: Soil biodiversity pie charts ────────────────────────────────

  output$soil_biodiv_pies <- plotly::renderPlotly({
    major_groups <- c(
      "Mammalia", "Nematoda", "Arthropoda", "Plantae",
      "Bacteria", "Fungi", "Archaea", "Protists", "Phage"
    )

    total_df <- pnas_table1[
      pnas_table1$section == "Total species" & pnas_table1$group %in% major_groups,
      c("group", "central")
    ]
    soil_df <- pnas_table1[
      pnas_table1$section == "Species in soil" & pnas_table1$group %in% major_groups,
      c("group", "central")
    ]
    names(total_df)[2] <- "total_central"
    names(soil_df)[2] <- "soil_central"

    pie_df <- merge(total_df, soil_df, by = "group", all = FALSE)
    pie_df$display_group <- ifelse(pie_df$group == "Phage", "Bacteriophage", pie_df$group)
    pie_df$total_val <- parse_numeric_field(pie_df$total_central)
    pie_df$soil_val <- parse_numeric_field(pie_df$soil_central)
    pie_df$pct_soil <- 100 * pie_df$soil_val / pie_df$total_val
    pie_df <- pie_df[is.finite(pie_df$pct_soil) & !is.na(pie_df$pct_soil), ]
    pie_df <- pie_df[order(-pie_df$pct_soil, pie_df$display_group), ]

    shiny::validate(
      shiny::need(nrow(pie_df) > 0, "No biodiversity percentage data available.")
    )

    n <- nrow(pie_df)
    grid_cols <- 3L
    grid_rows <- ceiling(n / grid_cols)

    p <- plotly::plot_ly()
    annotations <- list()

    for (i in seq_len(n)) {
      r <- (i - 1L) %/% grid_cols
      c <- (i - 1L) %% grid_cols

      x0 <- c / grid_cols
      x1 <- (c + 1L) / grid_cols
      y1 <- 1 - (r / grid_rows)
      y0 <- 1 - ((r + 1L) / grid_rows)

      pct <- pie_df$pct_soil[i]
      grp <- pie_df$display_group[i]

      p <- plotly::add_trace(
        p,
        type = "pie",
        name = grp,
        labels = c("In soil", "Other habitats"),
        values = c(pct, 100 - pct),
        hole = 0.45,
        sort = FALSE,
        direction = "clockwise",
        textinfo = "none",
        marker = list(colors = c(cols$lime, cols$muted)),
        showlegend = i == 1L,
        domain = list(
          x = c(x0 + 0.02, x1 - 0.02),
          y = c(y0 + 0.08, y1 - 0.02)
        ),
        hovertemplate = paste0("%{label}: %{value:.1f}%<extra>", grp, "</extra>")
      )

      annotations[[length(annotations) + 1L]] <- list(
        x = (x0 + x1) / 2,
        y = y0 + 0.02,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "bottom",
        text = paste0(grp, "<br>", sprintf("%.1f", pct), "% in soil"),
        font = list(color = "#F1F5F9", size = 11)
      )
    }

    nhm_plotly_layout(
      p,
      palette = palette,
      margin = list(t = 20, b = 40, l = 10, r = 10),
      legend = list(orientation = "h", y = -0.1),
      annotations = annotations
    ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

}

shinyApp(ui, server)
