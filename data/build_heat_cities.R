# Build heat_cities dataset in RDS format for faster loading.
# Usage: Rscript data/build_heat_cities.R

source_path <- file.path("data", "heat_cities.csv")

if (!file.exists(source_path)) {
  stop("Could not find source CSV: ", source_path, call. = FALSE)
}

message("Reading source: ", normalizePath(source_path, winslash = "/", mustWork = TRUE))
heat <- read.csv(source_path, stringsAsFactors = FALSE)

if (!"scenario" %in% names(heat)) {
  heat$scenario <- "rcp85"
}

if (!"pathway" %in% names(heat)) {
  heat$pathway <- ifelse(grepl("^ssp", heat$scenario), "cmip6_ssp585", "cmip5_rcp85")
}

# Compute baseline (first year) change
baseline_years <- stats::aggregate(
  year ~ pathway + city_name,
  data = heat,
  FUN = min
)
baseline <- merge(
  baseline_years,
  heat[, c("pathway", "city_name", "year", "hottest_3mo_tasmax_c")],
  by = c("pathway", "city_name", "year"),
  all.x = TRUE
)
baseline <- baseline[, c("pathway", "city_name", "hottest_3mo_tasmax_c")]
names(baseline)[3] <- "baseline_temp"
heat <- merge(heat, baseline, by = c("pathway", "city_name"), all.x = TRUE)
heat$temp_change <- heat$hottest_3mo_tasmax_c - heat$baseline_temp

out_path_rds <- file.path("data", "heat_cities.rds")
saveRDS(heat, out_path_rds)

message("Wrote dataset: ", normalizePath(out_path_rds, winslash = "/", mustWork = TRUE))
message("Rows: ", nrow(heat), " | Cities: ", length(unique(heat$city_name)))
