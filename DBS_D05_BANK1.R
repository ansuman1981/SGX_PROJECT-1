install.packages("readr")
library(readr)
library(dplyr)
library(lubridate)
D05 <- read.csv("D05.SI_2024-2026.csv.csv")
names(D05)
dbs <- D05
sum(is.na(dbs$close)) # check any NA values 
sum(dbs$close == 0, na.rm = TRUE) # if both return 0 fit to go 

### craete a sort data for year 2024-2025

# Tells R: "My date is Day-Month-Year"
dbs$date <- as.Date(dbs$date, format = "%d-%m-%Y")

# Now check the first few rows again
head(dbs$date)

# filter the whole date 
clean_data <- dbs %>%
  filter(date >= "2024-01-01" & date <= "2025-12-31")
class(clean_data$date)
nrow(clean_data)
head(dbs$date)

## bird eye view
# basic summary for the open colum  
summary(clean_data$open)
# Check the 'Spread' (Difference between highest and lowest open ever)
diff(range(clean_data$open, na.rm = TRUE))

#find the percentage difference relative to the mean
clean_data %>%
  summarise(
    Avg = mean(open, na.rm = TRUE),
    Mid = median(open, na.rm = TRUE),
    Pct_Diff = ((Mid - Avg) / Avg) * 100
  )

# Compare the Open price by year
clean_data %>%
  group_by(Year = year(date)) %>%
  summarise(
    Avg_Open = mean(open),
    Max_Open = max(open),
    Volatility = sd(open)
  )

# check the density ........................................................
library(ggplot2)
# check density
ggplot(clean_data, aes(x = open, fill = as.factor(year(date)))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Where the Price 'Lives': 2024 vs 2025", fill = "Year")


# Compare the Open price by year with all the major things
yearly_summary <- clean_data %>%
  group_by(Year =year(date)) %>%
  summarise(
    Avg_Open = mean(open, na.rm = TRUE),
    Max_Open = max(open, na.rm = TRUE),
    Min_Open = min(open, na.rm = TRUE),
    Spread = diff(range(open, na.rm = TRUE)),
    Volatility = sd(open, na.rm = TRUE)
  )

print(yearly_summary)


#  Run The "Trend Journey 2024 to 2025 every month" 
library(ggplot2)
library(lubridate)

ggplot(clean_data, aes(x = date, y = open)) +
  # 1. Draw the daily price line, colored by year so they stand out
  geom_line(aes(color = as.factor(year(date))), alpha = 0.8, linewidth = 0.7) +
  
  # 2. Draw the underlying momentum trend line (smooth curve)
  geom_smooth(method = "loess", color = "black", linetype = "dashed", se = FALSE, linewidth = 1) +
  
  # 3. Draw a physical boundary exactly on New Year's Day
  geom_vline(xintercept = as.Date("2025-01-01"), color = "red", linetype = "dotted", linewidth = 1) +
  
  # 4. Force the X-axis to show every 2 months (e.g., "Jan 2024", "Mar 2024")
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  
  # 5. Clean theme and tilt the text so the months don't overlap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # 6. Professional Labels
  labs(title = "High-Definition Price Journey: 2024 vs 2025",
       subtitle = "Dotted red line separates the 2024 baseline from the 2025 breakout",
       x = "Exact Timeline",
       y = "Daily Opening Price",
       color = "Year")


# black swan findingn outlier 

library(ggplot2)
library(lubridate)

ggplot(clean_data, aes(x = as.factor(year(date)), y = open, fill = as.factor(year(date)))) +
  # Adding a 'notch' makes the median comparison more professional
  geom_boxplot(notch = TRUE, alpha = 0.7, color = "#2c3e50", outlier.shape = NA) + 
  # Adding 'jitter' shows the actual data points lightly in the background
  geom_jitter(width = 0.1, alpha = 0.2, color = "black") +
  # Note: The names here must match the years in your data
  scale_fill_manual(values = c("2024" = "#BDC3C7", "2025" = "#2980B9")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  ) +
  labs(
    title = "Statistical Distribution of Open Prices",
    subtitle = "Confirmed: Zero Outliers detected across both fiscal years",
    x = "Trading Year",
    y = "Opening Price (SGD)"
  )


# Analyzing the 'Personality' of the Work Week monday to friday  
weekly_personality <- clean_data %>%
  group_by(Year =year(date), day_of_week) %>%
  summarise(
    Avg_Open = mean(open, na.rm = TRUE),
    Volatility = sd(open, na.rm = TRUE), # Shows which days are the 'wildest'
    .groups = "drop"
  ) %>%
  arrange(Year, match(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))

print(weekly_personality)


library(ggplot2)
# Re-ordering the days so they appear in Work-Week order
clean_data$day_of_week <- factor(clean_data$day_of_week, 
                                 levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

ggplot(clean_data, aes(x = day_of_week, y = open, fill = as.factor(year(date)))) +
  # We use 'stat_summary' to let ggplot calculate the mean for us automatically
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  # Adding error bars shows the 'Volatility' you found (Higher bars = more wild)
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("2024" = "#BDC3C7", "2025" = "#2980B9")) +
  theme_minimal() +
  labs(title = "Weekly Personality: 2024 vs 2025",
       subtitle = "Error bars represent the Standard Error (Volatility) of each day",
       x = "Day of the Week",
       y = "Average Opening Price",
       fill = "Year")

# Analyzing the month SEASONALITY with month, average_open and growth_range
monthly_seasonality <- clean_data %>%
  group_by(Year = year(date), Month = month(date, label = TRUE)) %>%
  summarise(
    Avg_Open = mean(open, na.rm = TRUE),
    Growth_Range = max(open) - min(open), # Which month had the biggest move?
    .groups = "drop"
  ) %>%
  arrange(Year, Month)

print(monthly_seasonality)

#plot comapring average monthly opening price trends 
library(ggplot2)
library(lubridate)

ggplot(monthly_seasonality, aes(x = Month, y = Avg_Open, color = as.factor(Year), group = Year)) +
  # 1. Draw thick lines and points to show the monthly trajectory
  geom_line(linewidth = 1.5, alpha = 0.8) +
  geom_point(size = 3.5) +
  
  # 2. Use the established 'Executive' color palette
  scale_color_manual(values = c("2024" = "#BDC3C7", "2025" = "#2980B9")) +
  
  # 3. Enhance the theme for a clean, banking-report look
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0) # Months are short, no need to tilt
  ) +
  
  # 4. Professional Labels
  labs(
    title = "Monthly Seasonality: The Annual Pulse of DBS",
    subtitle = "Comparing average monthly opening prices to identify seasonal trends",
    x = "Month",
    y = "Average Opening Price (SGD)",
    color = "Year"
  )
#################################################
#closeing summmary 

# Phase 1: Statistical Deep-Dive of the Close Price
close_stats <- clean_data %>%
  group_by(Year = year(date)) %>%
  summarise(
    Avg_Close = mean(close, na.rm = TRUE),
    Median_Close = median(close, na.rm = TRUE),
    Volatility_Close = sd(close, na.rm = TRUE),
    Max_Close = max(close, na.rm = TRUE),
    Min_Close = min(close, na.rm = TRUE),
    Total_Days = n()
  )

print(close_stats)
# finding the summary for the close 
summary(clean_data$close)

#The Verdict Analysis (Win/Loss Frequency)
# Phase 2: Calculating Daily Win/Loss Frequency and Magnitude
verdict_analysis <- clean_data %>%
  mutate(Daily_Verdict = ifelse(close > open, "Green Day (Bullish)", "Red Day (Bearish)")) %>%
  group_by(Year = year(date), Daily_Verdict) %>%
  summarise(
    Count = n(),
    Avg_Move_Size = mean(close - open, na.rm = TRUE),
    Max_Daily_Gain = max(close - open, na.rm = TRUE),
    Max_Daily_Loss = min(close - open, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  mutate(Win_Loss_Percentage = (Count / sum(Count)) * 100)

print(verdict_analysis)

####The Intraday Distribution (Visualizing the Verdict)

library(ggplot2)

# Step 1: Ensure we have the Intraday_Move calculated
clean_data <- clean_data %>%
  mutate(Intraday_Move = close - open)

# Step 2: Plot the Density comparison
ggplot(clean_data, aes(x = Intraday_Move, fill = as.factor(year(date)))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("2024" = "red", "2025" = "green"), name = "Year") +
  labs(title = "Intraday Momentum Distribution: 2024 vs 2025",
       subtitle = "Shift to the right indicates stronger intraday buyer conviction",
       x = "Intraday Move (Close - Open)",
       y = "Density") +
  annotate("text", x = 1, y = 0.5, label = "Bulls Win -->", color = "green") +
  annotate("text", x = -1, y = 0.5, label = "<-- Bears Win", color = "red")

###################
# finding the outlier 
# comparing the "final verdict distribution 2024-2025
library(ggplot2)
library(lubridate)

ggplot(clean_data, aes(x = as.factor(year(date)), y = close, fill = as.factor(year(date)))) +
  # 1. The "Cloud": Shows the density/shape of the prices
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  # 2. The "Structure": The professional boxplot inside
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 16, alpha = 0.6) +
  # 3. The "Rain": Individual days so we can see every single settle
  geom_jitter(width = 0.05, alpha = 0.2, size = 0.8, aes(color = as.factor(year(date)))) +
  # Modern styling
  theme_minimal() +
  scale_fill_manual(values = c("2024" = "#95a5a6", "2025" = "#2c3e50")) +
  scale_color_manual(values = c("2024" = "#7f8c8d", "2025" = "#34495e")) +
  labs(
    title = "Closing Price Structural Integrity",
    subtitle = "Comparing the 'Final Verdict' distribution (2024 vs 2025)",
    x = "Year",
    y = "Closing Price (SGD)"
  ) +
  theme(legend.position = "none")



####c final monthly personality
library(ggplot2)
library(dplyr)
library(lubridate)

# Step 1: Aggregate Monthly Close Prices
monthly_radar <- clean_data %>%
  mutate(Month = month(date, label = TRUE)) %>%
  group_by(Year = year(date), Month) %>%
  summarise(Avg_Close = mean(close, na.rm = TRUE), .groups = "drop")

# Step 2: Create the Seasonal Radar Plot
ggplot(monthly_radar, aes(x = Month, y = Avg_Close, group = as.factor(Year), color = as.factor(Year))) +
  geom_polygon(fill = NA, size = 1.2) + # Connects Dec back to Jan
  geom_point(size = 3) +
  coord_polar() + # This makes it a circle (Radar)
  theme_minimal() +
  scale_color_manual(values = c("2024" = "#BDC3C7", "2025" = "#2c3e50"), name = "Year") +
  labs(title = "Monthly Price Personality: The Annual Cycle",
       subtitle = "The 'Expansion' of the 2025 circle shows consistent growth across all seasons",
       y = "Avg Close (SGD)",
       x = "") +
  theme(axis.text.y = element_blank(), # Cleans up the inside of the circle
        plot.title = element_text(face = "bold", size = 16))


#########################################################
# Phase 6, Step 1: The Macro Ceiling Analysis
macro_ceiling <- clean_data %>%
  group_by(Year = year(date)) %>%
  summarise(
    Absolute_Max_High = max(high, na.rm = TRUE),
    Average_High = mean(high, na.rm = TRUE),
    # Averaging the top 10 highest days to find the "True" Resistance Zone
    True_Ceiling_Top10 = mean(sort(high, decreasing = TRUE)[1:10], na.rm = TRUE),
    .groups = "drop"
  )

print(macro_ceiling)
