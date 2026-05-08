# Build a compact summary for the Bluetooth heatmap page.
# Run from repo root:
# Rscript data/build_bluetooth_heatmap_summary.R

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) > 0) args[[1]] else "data"

raw_file <- file.path(base_dir, "bluetooth.RData")
out_file <- file.path(base_dir, "bluetooth_heatmap_summary.rds")

if (!file.exists(raw_file)) {
  stop("Missing input file: ", raw_file, call. = FALSE)
}

env <- new.env(parent = emptyenv())
loaded_names <- load(raw_file, envir = env)

if ("data" %in% loaded_names) {
  bluetooth_data <- as.data.frame(env$data)
} else if (length(loaded_names) > 0) {
  bluetooth_data <- as.data.frame(env[[loaded_names[[1]]]])
} else {
  stop("No objects found in bluetooth.RData", call. = FALSE)
}

if (!all(c("device", "datetime", "count") %in% names(bluetooth_data))) {
  stop("Expected columns not found: device, datetime, count", call. = FALSE)
}

bluetooth_data <- bluetooth_data[
  !is.na(bluetooth_data$datetime) & !is.na(bluetooth_data$count),
  c("device", "datetime", "count")
]

if (nrow(bluetooth_data) == 0) {
  stop("No valid rows found after filtering.", call. = FALSE)
}

build_time_bin <- function(datetime, time_unit) {
  switch(time_unit,
    "hour" = format(datetime, "%Y-%m-%d %H:00"),
    "day" = format(datetime, "%Y-%m-%d"),
    "week" = format(as.Date(cut(datetime, "week")), "%Y-%m-%d"),
    "month" = format(datetime, "%Y-%m"),
    stop("Unsupported time_unit")
  )
}

time_bin_to_date <- function(time_bin, time_unit) {
  switch(time_unit,
    "hour" = as.Date(substr(time_bin, 1, 10)),
    "day" = as.Date(time_bin),
    "week" = as.Date(time_bin),
    "month" = as.Date(paste0(time_bin, "-01")),
    stop("Unsupported time_unit")
  )
}

aggregate_bt_data <- function(data, time_unit) {
  data$time_bin <- build_time_bin(data$datetime, time_unit)
  agg <- aggregate(count ~ device + time_bin, data = data, FUN = sum, na.rm = TRUE)
  agg$date_bin <- time_bin_to_date(agg$time_bin, time_unit)
  agg
}

device_totals <- sort(tapply(
  bluetooth_data$count,
  bluetooth_data$device,
  sum,
  na.rm = TRUE
), decreasing = TRUE)

agg_hour <- aggregate_bt_data(bluetooth_data, "hour")
agg_day <- aggregate_bt_data(bluetooth_data, "day")
agg_week <- aggregate_bt_data(bluetooth_data, "week")
agg_month <- aggregate_bt_data(bluetooth_data, "month")

summary_obj <- list(
  agg_hour = agg_hour,
  agg_day = agg_day,
  agg_week = agg_week,
  agg_month = agg_month,
  device_totals = device_totals,
  date_min = min(as.Date(bluetooth_data$datetime), na.rm = TRUE),
  date_max = max(as.Date(bluetooth_data$datetime), na.rm = TRUE)
)

saveRDS(summary_obj, out_file)

cat("Saved summary to:", out_file, "\n")
cat("Rows (hour):", nrow(agg_hour), "\n")
cat("Rows (day):", nrow(agg_day), "\n")
cat("Rows (week):", nrow(agg_week), "\n")
cat("Rows (month):", nrow(agg_month), "\n")
cat("Devices:", length(device_totals), "\n")
