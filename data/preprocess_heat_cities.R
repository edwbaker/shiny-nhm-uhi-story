source_path <- "D:/hot-cities/outputs_raw/c40_city_source_annual_heat_metrics_ensemble.csv"
local_source_path <- "data/c40_city_source_annual_heat_metrics_ensemble.csv"
output_path <- "data/heat_cities.csv"

if (!file.exists(source_path)) {
  stop("Source file not found: ", source_path, call. = FALSE)
}

dir.create(dirname(local_source_path), recursive = TRUE, showWarnings = FALSE)
file.copy(source_path, local_source_path, overwrite = TRUE)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required to preprocess heat cities data.", call. = FALSE)
}

heat <- data.table::fread(local_source_path)

required_cols <- c(
  "climate_source_id",
  "scenario",
  "year",
  "city_name",
  "country",
  "pop_max",
  "lon",
  "lat",
  "ensemble_mean_hottest90d_tasmax_c"
)

missing_cols <- setdiff(required_cols, names(heat))
if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

if ("variable" %in% names(heat)) {
  heat <- heat[variable == "tasmax"]
}

if ("rolling_days" %in% names(heat)) {
  heat <- heat[rolling_days == 90]
}

heat <- heat[
  !is.na(city_name) &
    !is.na(climate_source_id) &
    !is.na(year) &
    !is.na(lat) &
    !is.na(lon) &
    !is.na(ensemble_mean_hottest90d_tasmax_c)
]

heat[, pathway := data.table::fifelse(
  grepl("cmip5", climate_source_id, ignore.case = TRUE),
  "cmip5_rcp85",
  data.table::fifelse(
    grepl("cmip6", climate_source_id, ignore.case = TRUE),
    "cmip6_ssp585",
    paste0("source_", scenario)
  )
)]

heat <- heat[, .(
  pathway = as.character(pathway),
  scenario = as.character(scenario),
  city_name = as.character(city_name),
  lat = as.numeric(lat),
  lon = as.numeric(lon),
  year = as.integer(year),
  hottest_3mo_tasmax_c = as.numeric(ensemble_mean_hottest90d_tasmax_c),
  country = as.character(country),
  pop_max = as.numeric(pop_max)
)]

heat <- heat[, .(
  hottest_3mo_tasmax_c = mean(hottest_3mo_tasmax_c, na.rm = TRUE),
  pop_max = max(pop_max, na.rm = TRUE)
), by = .(pathway, scenario, city_name, country, lon, lat, year)]

data.table::setorder(heat, pathway, year, city_name)
data.table::fwrite(heat, output_path)

cat("Copied source to", normalizePath(local_source_path, winslash = "/", mustWork = FALSE), "\n")
cat("Saved", nrow(heat), "rows to", normalizePath(output_path, winslash = "/", mustWork = FALSE), "\n")
cat("Pathways:", paste(sort(unique(heat$pathway)), collapse = ", "), "\n")