install.packages("readr")
library(readr)
library(dplyr)
D05 <- read.csv("D05.SI_2024-2026.csv")
names(D05)
summary(D05)
dbs <- D05 |>  # arrange check
  arrange(date)

sum(is.na(dbs$close))
sum(dbs$close == 0, na.rm = TRUE)
