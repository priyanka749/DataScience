library(tidyverse)

# Load the datasets
crime <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/crime_cleaned.csv")
school <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/schoolcleaned.csv")
housingPrice <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/combined_house_data.csv")
broadband <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/broadband_cleaned.csv")

# Inspect initial dataset dimensions
dim(crime)
dim(school)
dim(housingPrice)
dim(broadband)

# Selecting Relevent Rows from Broadband
selected_broadband = broadband %>%
  group_by(Town_City) %>%
  summarise(
    avg_upl_speed = mean(AvgUpSpeed, na.rm = TRUE),
    avg_down_speed = mean(AvgDownSpeed, na.rm = TRUE)
  ) %>%
  mutate(TOWN = str_trim(toupper(Town_City))) %>%
  select(TOWN, avg_upl_speed, avg_down_speed)

# Check the number of rows
nrow(selected_broadband)

# Selecting Attainment 8 from school data
selected_attainment8 = school %>%
  group_by(TOWN) %>%
  summarise(avgAtt8 = mean(ATT8SCR, na.rm = TRUE)) %>%
  select(TOWN, avgAtt8) %>%
  distinct() %>%
  mutate(TOWN = str_trim(toupper(TOWN)))

# Check the number of rows
nrow(selected_attainment8)

# Selecting relevant house price data for 2023
selected_house = housingPrice %>%
  filter(Year == 2023) %>%
  mutate(TOWN = str_trim(toupper(Town_City))) %>%
  group_by(TOWN) %>%
  summarise(avgPrice = mean(Price, na.rm = TRUE)) %>%
  select(avgPrice, TOWN) %>%
  na.omit() %>%
  distinct()

# Check the number of rows
nrow(selected_house)

# Preparing town and postcode data
town = housingPrice %>%
  mutate(postcode = str_trim(substring(Postcode, 1, 6))) %>%
  mutate(TOWN = str_trim(toupper(Town_City))) %>%
  select(postcode, TOWN) %>%
  distinct()

# Check the number of rows
nrow(town)

# Selecting relevant crime data for 2023
selected_crime = crime %>%
  filter(Year == 2023) %>%
  group_by(postcode) %>%
  summarise(crimeno = n()) %>%
  arrange(desc(crimeno)) %>%
  select(postcode, crimeno)

# Check the number of rows
nrow(selected_crime)

# Joining crime data with town data
final_crime = selected_crime %>%
  left_join(town, by = "postcode") %>%
  na.omit() %>%
  distinct()

# Check the number of rows after joining
nrow(final_crime)

# Summarizing crime data by town
final_crime = final_crime %>%
  group_by(TOWN) %>%
  summarise(crimerate = sum(crimeno)) %>%
  select(TOWN, crimerate)

# Check the number of rows
nrow(final_crime)

# Joining all datasets
ranking = selected_house %>%
  left_join(selected_attainment8, by = "TOWN") %>%  
  left_join(selected_broadband, by = "TOWN") %>%  
  left_join(final_crime, by = "TOWN") %>%
  na.omit()

# Check the number of rows after joining
nrow(ranking)
View(ranking)

# Check the number of rows
nrow(ranking_Points)
View(ranking_Points)

# Calculating the minimum and maximum for each column
Extremes <- ranking_Points %>%
  summarise(
    minDownSpeed = min(avg_down_speed),
    maxDownSpeed = max(avg_down_speed),
    minUpSpeed = min(avg_upl_speed),
    maxUpSpeed = max(avg_upl_speed),
    minAtt8 = min(avgAtt8),
    maxAtt8 = max(avgAtt8),
    minHousingPrice = min(avgPrice),
    maxHousingPrice = max(avgPrice),
    minCrimeRate = min(crimerate),
    maxCrimeRate = max(crimerate)
  )

# Normalizing and calculating the final points
finalRanking <- ranking_Points %>%
  mutate(
    normDownSpeed = (avg_down_speed - Extremes$minDownSpeed) / (Extremes$maxDownSpeed - Extremes$minDownSpeed),
    normUpSpeed = (avg_upl_speed - Extremes$minUpSpeed) / (Extremes$maxUpSpeed - Extremes$minUpSpeed),
    normAtt8 = (avgAtt8 - Extremes$minAtt8) / (Extremes$maxAtt8 - Extremes$minAtt8),
    normHousingPrice = 1 - (avgPrice - Extremes$minHousingPrice) / (Extremes$maxHousingPrice - Extremes$minHousingPrice),
    normCrimeRate = 1 - (crimerate - Extremes$minCrimeRate) / (Extremes$maxCrimeRate - Extremes$minCrimeRate),
    finalPoints = normDownSpeed + normUpSpeed + normAtt8 + normHousingPrice + normCrimeRate
  )

# View the final ranking data with the final score
finalRanking = finalRanking %>%
  arrange(desc(finalPoints))

# Check the number of rows
nrow(finalRanking)
View(finalRanking)
write_csv("C:\Users\Lenovo\Documents\priyanka datascience\ranking.csv")
