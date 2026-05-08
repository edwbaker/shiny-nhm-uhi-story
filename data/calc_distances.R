stations <- read.csv("data/met_office_stations.csv", stringsAsFactors = FALSE)
sensors  <- read.csv("data/nhm_sensors.csv", stringsAsFactors = FALSE)
wmo      <- read.csv("data/wmo_stations.csv", stringsAsFactors = FALSE)

haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}

mean_nn_dist <- function(lat, lon, chunk_size = 500) {
  n <- length(lat)
  if (n < 2) return(NA_real_)
  lat_r <- lat * pi / 180
  lon_r <- lon * pi / 180
  clat  <- cos(lat_r)
  nn <- numeric(n)
  # Process in chunks to avoid giant matrices
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  for (ch in chunks) {
    # Distance from chunk rows to ALL columns
    dlat <- outer(lat_r[ch], lat_r, "-")
    dlon <- outer(lon_r[ch], lon_r, "-")
    a <- sin(dlat / 2)^2 + outer(clat[ch], clat) * sin(dlon / 2)^2
    d <- 6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
    # Set self-distance to Inf
    for (k in seq_along(ch)) d[k, ch[k]] <- Inf
    nn[ch] <- apply(d, 1, min)
  }
  mean(nn)
}

cat("Computing NHM sensors (n=", length(sensors$lat), ")...\n")
d_nhm <- mean_nn_dist(sensors$lat, sensors$lon)
cat("  NHM:", round(d_nhm * 1000), "m\n")

cat("Computing Met Office (n=", length(stations$lat), ")...\n")
d_uk <- mean_nn_dist(stations$lat, stations$lon)
cat("  Met Office:", round(d_uk, 1), "km\n")

cat("Computing WMO (n=", length(wmo$lat), ")...\n")
d_wmo <- mean_nn_dist(wmo$lat, wmo$lon)
cat("  WMO:", round(d_wmo, 1), "km\n")
