# Script to create the jhansi_weather sample dataset
# Run this script from the package root directory:
#   source("data-raw/create_dataset.R")

# Read the CSV created during package development
jhansi_weather <- read.csv("data-raw/jhansi_weather.csv",
                           stringsAsFactors = FALSE)
jhansi_weather$Date <- as.Date(jhansi_weather$Date)
jhansi_weather$RH_morning <- as.integer(jhansi_weather$RH_morning)
jhansi_weather$RH_evening <- as.integer(jhansi_weather$RH_evening)

# Save as .rda
usethis::use_data(jhansi_weather, overwrite = TRUE, compress = "xz")

cat("Dataset saved to data/jhansi_weather.rda\n")
cat("Dimensions:", nrow(jhansi_weather), "x", ncol(jhansi_weather), "\n")
