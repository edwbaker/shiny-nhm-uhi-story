cat("Timing data loads...\n")

t1 <- system.time({ heat <- readRDS("data/heat_cities.rds") })
cat("heat_cities.rds:", nrow(heat), "rows in", round(t1[["elapsed"]], 2), "s\n")

t2 <- system.time({ uk3 <- readRDS("data/c40_uk3_city_model_annual_heat_metrics_raw.rds") })
cat("uk3 rds:", nrow(uk3), "rows in", round(t2[["elapsed"]], 2), "s\n")

t3 <- system.time({
  readRDS("data/met_office_stations.rds")
  readRDS("data/nhm_sensors.rds")
  readRDS("data/wmo_stations.rds")
})
cat("stations in", round(t3[["elapsed"]], 2), "s\n")

cat("unique(heat$pathway):", paste(unique(heat$pathway), collapse=", "), "\n")
cat("nrow heat:", nrow(heat), "\n")
cat("columns:", paste(names(heat), collapse=", "), "\n")
