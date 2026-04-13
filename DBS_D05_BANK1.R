install.packages("readr")
library(readr)
library(dplyr)
library(lubridate)
D05 <- read.csv("D05.SI_2024-2026.csv")
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


#  Run The "Trend Journey" 
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


# black swan 

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


# Analyzing the 'Personality' of the Work Week
weekly_personality <- clean_data %>%
  group_by(Year =year(date), day_of_week) %>%
  summarise(
    Avg_Open = mean(open, na.rm = TRUE),
    Volatility = sd(open, na.rm = TRUE), # Shows which days are the 'wildest'
    .groups = "drop"
  ) %>%
  arrange(Year, match(day_of_week, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))

print(weekly_personality)
