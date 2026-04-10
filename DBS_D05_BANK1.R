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
summary(clean_date$open)
# Check the 'Spread' (Difference between highest and lowest open ever)
diff(range(clean_data$open, na.rm = TRUE))

