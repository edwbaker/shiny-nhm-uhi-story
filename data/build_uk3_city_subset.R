# Build a local UK3 subset used by page 3 of the Shiny app.
# Usage: Rscript data/build_uk3_city_subset.R

source_candidates <- c(
  "D:/hot-cities/outputs_raw_uk3/c40_uk3_city_model_annual_heat_metrics_raw.csv",
  file.path("data", "c40_uk3_city_model_annual_heat_metrics_raw.csv")
)

source_path <- source_candidates[file.exists(source_candidates)]
if (length(source_path) == 0) {
  stop("Could not find source CSV in expected locations.", call. = FALSE)
}
source_path <- source_path[[1]]

message("Reading source: ", normalizePath(source_path, winslash = "/", mustWork = TRUE))
raw <- read.csv(source_path, stringsAsFactors = FALSE)

required_cols <- c(
  "city_name", "scenario", "model", "year",
  "annual_hottest90d_tasmax_c", "variable", "rolling_days", "value_available"
)
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

cities_needed <- c("London", "Reading", "Tring")
subset_rows <- raw[
  raw$city_name %in% cities_needed &
    raw$variable == "tasmax" &
    raw$rolling_days == 90 &
    as.logical(raw$value_available),
  c("city_name", "scenario", "model", "year", "annual_hottest90d_tasmax_c")
]

subset_rows <- subset_rows[order(subset_rows$city_name, subset_rows$scenario, subset_rows$model, subset_rows$year), ]

out_path_rds <- file.path("data", "c40_uk3_city_model_annual_heat_metrics_raw.rds")
saveRDS(subset_rows, out_path_rds)

message("Wrote subset: ", normalizePath(out_path_rds, winslash = "/", mustWork = TRUE))
message("Rows: ", nrow(subset_rows), " | Cities: ", paste(sort(unique(subset_rows$city_name)), collapse = ", "))
