# cop2073c-pe9Solve.R
# D. Singletary
# 10/20/24
# Simple linear regression

# Load the required package
# install.packages("MASS") # uncomment if necessary
library(MASS)

# Extract relevant data from the survey dataframe
# Note: Removing rows with NA values for Height or Wr.Hnd
survey_data <- na.omit(survey[, c("Height", "Wr.Hnd", "Pulse")])

# Fit a linear model for height based on writing hand span
lm_height_hand <- lm(Height ~ Wr.Hnd, data = survey_data)

# Predict mean student heights with 99% confidence intervals 
# for given hand spans
hand_spans <- data.frame(Wr.Hnd = c(12, 15.2, 17, 19.9))
predictions <- predict(lm_height_hand, newdata = hand_spans, 
                       interval = "confidence", level = 0.99)
print(predictions)

# Fit a linear model to predict mean student height based on pulse rate
lm_height_pulse <- lm(Height ~ Pulse, data = survey_data)

# Create a scatterplot of the data with the fitted line superimposed
plot(survey_data$Pulse, survey_data$Height, main = "Height vs Pulse Rate",
     xlab = "Pulse Rate", ylab = "Height (cm)", pch = 19, col = "blue")
abline(lm_height_pulse, col = "red")

# Extract and interpret the slope and hypothesis test results
slope <- coef(lm_height_pulse)["Pulse"]
summary_model <- summary(lm_height_pulse)
t_stat <- summary_model$coefficients["Pulse", "t value"]
p_value <- summary_model$coefficients["Pulse", "Pr(>|t|)"]

cat("\nSlope Estimate: ", slope, "\n")
cat("t-statistic: ", t_stat, "\n")
cat("p-value: ", p_value, "\n")

# Interpretation of the point estimate of the slope
cat("\nThe point estimate of the slope is ", slope, ", which indicates that\n")
cat("for each unit increase in pulse rate, the expected change in student\n")
cat("height is ", slope, " cm.\n")

# Interpretation of the hypothesis test result
if (p_value < 0.05) {
  cat("The p-value is ", p_value, ", which is less than the significance\n")
  cat("level of 0.05. This suggests that we reject the null hypothesis\n")
  cat("(H0: B1 = 0) and conclude that there is a statistically significant\n")
  cat("relationship between pulse rate and height.\n")
} else {
  cat("The p-value is ", p_value, ", which is greater than the significance\n")
  cat("level of 0.05. This suggests that we fail to reject the null\n")
  cat("hypothesis (H0: B1 = 0) and conclude that there is no statistically\n")
  cat("significant relationship between pulse rate and height.\n")
}

# Calculate 90% confidence interval for the slope parameter
conf_interval <- confint(lm_height_pulse, "Pulse", level = 0.90)
cat("90% Confidence Interval for Slope: ", conf_interval, "\n")

# Identify incomplete observations for the height and pulse data
incomplete_obs <- which(is.na(survey$Height) | is.na(survey$Pulse))

# Calculate the sample mean of height excluding incomplete observations
# using the unary operator
mean_height <- mean(survey$Height[-incomplete_obs], na.rm = TRUE)
cat("\nMean Student Height: ", mean_height, "\n")

# Add a horizontal line to the plot representing the mean height
abline(h = mean_height, col = "green", lty = 2)
