d <- read.csv("C:/Users/edwab/Downloads/StationSearchResults.csv",
              stringsAsFactors = FALSE, check.names = FALSE)

cat("Columns:\n")
print(names(d))
cat("\nRows:", nrow(d), "\n")

# Keep only name, lat, lon
out <- data.frame(
  station_name = trimws(d$Station),
  lat          = as.numeric(d$Latitude),
  lon          = as.numeric(d$Longitude),
  stringsAsFactors = FALSE
)

# Remove rows with missing coordinates
out <- out[!is.na(out$lat) & !is.na(out$lon), ]
# Remove (0,0)
out <- out[out$lat != 0 | out$lon != 0, ]

cat("After cleaning:", nrow(out), "\n")
cat("Lat range:", range(out$lat), "\n")
cat("Lon range:", range(out$lon), "\n")

write.csv(out, "data/wmo_stations.csv", row.names = FALSE)
cat("Saved", nrow(out), "rows\n")
