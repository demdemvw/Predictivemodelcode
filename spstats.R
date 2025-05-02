# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(lubridate)  # For handling timestamps
library(zoo)        # For running average (rollmean)

# Load the data
data <- read.csv("irradiancedata.csv", header = TRUE)

# Check column names to confirm they match expectations
print(colnames(data))

# Combine DATE and TIME into a timestamp (format: MM/DD/YYYY HH:MM:SS)
data <- data %>%
  mutate(Timestamp = as.POSIXct(paste(DATE, TIME), format = "%m/%d/%Y %H:%M:%S"))

# Sort by timestamp to ensure chronological order
data <- data %>% arrange(Timestamp)

# Convert columns to numeric, using correct column names with double dots
data <- data %>%
  mutate(
    `E..HUMID` = as.numeric(`E..HUMID`),
    `E..TEMP` = as.numeric(`E..TEMP`),
    `I..HUMID` = as.numeric(`I..HUMID`),
    `I..TEMP` = as.numeric(`I..TEMP`),
    `IRRAD.` = as.numeric(`IRRAD.`),
    `FAN.SPD.` = as.numeric(`FAN.SPD.`),
    `VENT..` = as.numeric(`VENT..`)
  )

# Remove rows with any NA values
data <- data %>% filter(complete.cases(.))

# Apply a 120-second running average to IRRAD.
data <- data %>%
  mutate(Smoothed_IRRAD = rollmean(`IRRAD.`, k = 120, fill = NA, align = "right"))

# Remove rows with NA in Smoothed_IRRAD (first 119 rows due to running average)
data <- data %>% filter(!is.na(Smoothed_IRRAD))

# Create the target variable: internal temperature 10 minutes ahead (600 seconds)
data <- data %>%
  mutate(Future_I_temperature = lead(`I..TEMP`, n = 600))

# Remove rows where Future_I_temperature is NA (last 600 rows)
data <- data %>% filter(!is.na(Future_I_temperature))

# Split into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the linear regression model using Smoothed_IRRAD and correct column names
temp_model <- lm(Future_I_temperature ~ Smoothed_IRRAD + `FAN.SPD.` + `VENT..` + 
                   `E..TEMP` + `E..HUMID` + `I..HUMID` + `I..TEMP`, 
                 data = train_data)

# Predict on the test set
test_data$Predicted_Future_I_temperature <- predict(temp_model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$Future_I_temperature - test_data$Predicted_Future_I_temperature)^2))
cat("RMSE on Test Data:", rmse, "\n")

# Plot predicted vs. actual temperatures
ggplot(test_data, aes(x = Future_I_temperature, y = Predicted_Future_I_temperature)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Future Internal Temp (10 min ahead)",
       x = "Actual Future Internal Temperature (°C)",
       y = "Predicted Future Internal Temperature (°C)") +
  theme_minimal()

# Save the plot
ggsave("predicted_vs_actual_temp_10min_smoothed.png", width = 8, height = 6)

# Display model summary
summary(temp_model)

# Extract coefficients for Arduino implementation
coefficients <- coef(temp_model)
cat("\nModel Coefficients for Arduino Implementation:\n")
print(coefficients)
