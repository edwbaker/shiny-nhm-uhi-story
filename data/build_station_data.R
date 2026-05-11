# Build station datasets (met_office, nhm_sensors, wmo_stations) in RDS format.
# Usage: Rscript data/build_station_data.R

datasets <- list(
  met_office_stations = "met_office_stations.csv",
  nhm_sensors = "nhm_sensors.csv",
  wmo_stations = "wmo_stations.csv"
)

for (name in names(datasets)) {
  csv_file <- datasets[[name]]
  csv_path <- file.path("data", csv_file)
  
  if (!file.exists(csv_path)) {
    warning("Skipping ", csv_file, " - file not found")
    next
  }
  
  message("Reading: ", csv_file)
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  rds_file <- sub("\\.csv$", ".rds", csv_file)
  rds_path <- file.path("data", rds_file)
  saveRDS(df, rds_path)
  
  message("  -> Wrote: ", rds_file, " (", nrow(df), " rows)")
}

message("Done.")
