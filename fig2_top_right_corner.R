library(ggplot2)

# Parameters
slope1 <- 1.67
SE1 <- 2.41
slope_diff <- -2.52
SE2 <- 2.24
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

# Determine common y-axis limits
y_min <- min(df$CI_lower)  # Use min from both datasets
y_max <- max(df$CI_upper)  # Use max from both datasets

# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#414487FF", size = 1) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
    plot.margin = unit(c(13.5,9,13.5,9), "cm"))


# Parameters
slope1 <- -1.16
SE1 <- 0.68
slope_diff <- -2.22
SE2 <- 1.60
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

# Determine common y-axis limits
y_min <- min(df$CI_lower)  # Use min from both datasets
y_max <- max(df$CI_upper)  # Use max from both datasets


# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#22A884FF", size = 1) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))




# Parameters
slope1 <- -0.89
SE1 <- 0.50
slope_diff <- -1.05
SE2 <- 1.10
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

y_min <- min(df$CI_lower)  # Use min from both datasets
y_max <- max(df$CI_upper)  # Use max from both datasets


# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#7AD151FF", size = 1) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


##############################################################################

#snowmelt
# Parameters
slope1 <- 0.01
SE1 <- 0.12
slope_diff <- 0.79
SE2 <- 0.22
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create snowmelt range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)


# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#414487FF", size = 1) +
  ylim(145,164) +
  xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


# Parameters
slope1 <- -0.05
SE1 <- 0.15
slope_diff <- 0.54
SE2 <- 0.17
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create snowmelt range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#22A884FF", size = 1) +
  ylim(138,152) +
  xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


# Parameters
slope1 <- 0.00
SE1 <- 0.20
slope_diff <- 0.17
SE2 <- 0.21
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#7AD151FF", size = 1) +
  ylim(146,155) +
  xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))

################################################################################
########################## single predictor ####################################
################################################################################

# Parameters
slope1 <- -1.96
SE1 <- 1.90
slope_diff <- 1.47
SE2 <- 2.38
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

y_min <- min(df$CI_lower)  # Use min from both datasets
y_max <- max(df$CI_upper)  # Use max from both datasets


# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#414487FF", size = 1) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


# Parameters
slope1 <- -4.95
SE1 <- 0.92
slope_diff <- 4.20
SE2 <- 2.09
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#22A884FF", size = 1) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))




# Parameters
slope1 <- -2.56
SE1 <- 0.57
slope_diff <- 1.93
SE2 <- 1.17
slope2 <- slope1 + slope_diff
breakpoint <- 0  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
temperature <- seq(-10, 10, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(temperature <= breakpoint,
                    intercept + slope1 * temperature,
                    intercept + slope1 * breakpoint + slope2 * (temperature - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(temperature <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(temperature <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(temperature, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = temperature, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#7AD151FF", size = 1) +
  xlim(-10,10) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Temperature (°C)",
       y = "Phenology  \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


##############################################################################

#snowmelt
# Parameters
slope1 <- 0.12
SE1 <- 0.09
slope_diff <- 0.83
SE2 <- 0.24
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)

y_min <- min(df$CI_lower)  # Use min from both datasets
y_max <- max(df$CI_upper)  # Use max from both datasets

# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#414487FF", size = 1) +
  ylim(y_min, y_max) +  # Ensure identical y-axis
  xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))



# Parameters
slope1 <- 0.02
SE1 <- 0.15
slope_diff <- 0.69
SE2 <- 0.28
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create snowmelt range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#22A884FF", size = 1) +
  #ylim(y_min, y_max) +  # Ensure identical y-axis
  #xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))


# Parameters
slope1 <- 0.02
SE1 <- 0.15
slope_diff <- 0.33
SE2 <- 0.21
slope2 <- slope1 + slope_diff
breakpoint <- 140  # Temperature at which the slope changes
intercept <- 150  # Adjust to suit your dataset

# Create temperature range
snowmelt <- seq(130, 150, by = 0.1)

# Calculate phenology (day of year) and confidence intervals
phenology <- ifelse(snowmelt <= breakpoint,
                    intercept + slope1 * snowmelt,
                    intercept + slope1 * breakpoint + slope2 * (snowmelt - breakpoint))

t_value <- qt(0.975, df = 30)  # Adjust df as needed
CI_upper <- ifelse(snowmelt <= breakpoint,
                   phenology + t_value * SE1,
                   phenology + t_value * SE2)
CI_lower <- ifelse(snowmelt <= breakpoint,
                   phenology - t_value * SE1,
                   phenology - t_value * SE2)

# Create a data frame
df <- data.frame(snowmelt, phenology, CI_upper, CI_lower)

# Plot with ggplot
ggplot(df, aes(x = snowmelt, y = phenology)) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red", size = 0.3) +
  geom_line(color = "#7AD151FF", size = 1) +
  #ylim(150,158) +
  #xlim(129,151) +
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.2, fill = "blue") +
  labs(x = "Snowmelt \n(Day of Year)",
       y = "Phenology \n(Day of Year)") +
  theme_test() +
  theme(axis.text = element_blank(),
        axis.title.x = element_text(vjust = -2, size = 8),
        axis.title.y = element_text(vjust = 4, size = 8),
        axis.ticks = element_blank(),
        plot.margin = unit(c(13.5,9,13.5,9), "cm"))












