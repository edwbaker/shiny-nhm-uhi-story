d <- read.csv("C:/Users/edwab/Downloads/Data Ecosystem _ NHM.csv",
              stringsAsFactors = FALSE)

# Clean longitude - strip leading quote marks
d$Longitude <- as.numeric(gsub("^'+", "", as.character(d$Longitude)))
d$Latitude  <- as.numeric(d$Latitude)

# Fix known sign error: positive lon near NHM should be negative
d$Longitude[d$Latitude > 51 & d$Latitude < 52 & d$Longitude > 0 & d$Longitude < 1] <-
  -d$Longitude[d$Latitude > 51 & d$Latitude < 52 & d$Longitude > 0 & d$Longitude < 1]

# Remove (0,0) entries
d <- d[d$Latitude != 0 & d$Longitude != 0, ]

# Filter to NHM vicinity only (bounding box around NHM grounds + gardens)
d <- d[d$Latitude > 51.493 & d$Latitude < 51.500 &
       d$Longitude > -0.182 & d$Longitude < -0.174, ]

cat("After NHM filter:", nrow(d), "\n")

# De-duplicate by unique lat/lon (keep first named entry per location)
# Prefer named entries (not UUIDs) - sort so named entries come first
is_uuid <- grepl("^[0-9a-f]{8}-", d$Location.Name)
d <- d[order(is_uuid), ]  # named first, then UUIDs

d$loc_key <- paste(round(d$Latitude, 5), round(d$Longitude, 5))
d <- d[!duplicated(d$loc_key), ]

cat("After dedup:", nrow(d), "\n")
cat("\nLocations:\n")
for (i in seq_len(nrow(d))) {
  cat(sprintf("  %s (%.6f, %.6f)\n", d$Location.Name[i], d$Latitude[i], d$Longitude[i]))
}

out <- data.frame(
  sensor_name = d$Location.Name,
  type        = d$Location.Type,
  lat         = d$Latitude,
  lon         = d$Longitude,
  stringsAsFactors = FALSE
)
write.csv(out, "data/nhm_sensors.csv", row.names = FALSE)
cat("\nSaved", nrow(out), "rows\n")
