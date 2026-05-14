load("//valentine/mbl/share/workspaces/groups/urban-nature-project/sensors/sensors.Rdata")

ds18b20 <- sensor_data[sensor_data$sensor == "ds18b20",]
ds18b20$timestamp <- as.numeric(ds18b20$timestamp)

soil_locs <- c(
  "28-00000f9d74ea",
  "28-00000f9cf161",
  "28-00000f9d3348",
  "28-00000f9d0f1c",
  "28-00000f9c7e6a",
  "28-00000f9cf708",
  "28-00000f9e7522",
  "28-00000f9db933",
  "28-00000fa3cd99",
  "28-000010491907",
  "28-00000fa3f840",
  "28-00000fa44683",
  "28-00000f9cc2ca",
  "28-0000104906cc",
  "28-00000ff8e011",
  "28-00000fa659d0",
  "28-00000fa45dad",
  "28-00000ff81fd6",
  "28-00000f9dc5e7",
  "28-00000f9d2349"
)
ds18b20 <- ds18b20[ds18b20$sensor_id %in% soil_locs,]
ds18b20 <- ds18b20[as.POSIXct(ds18b20$timestamp, origin = "1970-01-01") >= as.POSIXct("2025-05-09"),]

# Aggregate to hourly max per sensor to reduce file size
ds18b20$value <- as.numeric(ds18b20$value)
ds18b20 <- ds18b20[!is.na(ds18b20$value) & ds18b20$value < 80, ]
ds18b20$hour_dt <- as.POSIXct(
  format(as.POSIXct(ds18b20$timestamp, origin = "1970-01-01"), "%Y-%m-%d %H:00:00"),
  tz = "UTC"
)
ds18b20 <- aggregate(value ~ sensor_id + hour_dt, data = ds18b20, FUN = max, na.rm = TRUE)

save(ds18b20, file="data/nhm_sensors.RData")
