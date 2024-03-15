# Load necessary library
library(dplyr)

# Step 1: Read the CSV file
used_cars <- read.csv("used_cars.csv", stringsAsFactors = FALSE)

# Step 2: Convert the price from a character string to an integer
# This removes the dollar sign and commas, then converts to integer
used_cars$price_int <- as.integer(gsub("\\$|,", "", used_cars$price))

# Step 3: Now, you've got a new column 'price_int' with integer prices

# Step 4: Write the updated data frame back to a new CSV file, or overwrite the old one
write.csv(used_cars, "used_cars_updated.csv", row.names = FALSE)
