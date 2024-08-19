#graph

library(tidyverse)
library(dplyr)

library(ggplot2)
#install.packages("fmsb")
library(fmsb)

library(readr)

#install.packages("scales")
library(scales)
#_______________________________Crime_____________________

crime <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/crime_cleaned.csv")

vehicle_crime = crime %>%
  filter(`Crime type` == "Vehicle crime")

View(vehicle_crime)


vehicle_sum <- vehicle_crime %>%
  group_by(Year) %>%
  summarise(total_crime = sum(population, na.rm = TRUE))



radardata <- as.data.frame(t(vehicle_sum$total_crime))
colnames(radardata) <- vehicle_sum$Year

# Add max and min rows for radar chart scaling
radardata <- rbind(rep(max(vehicle_sum$total_crime), length(years)), 
                   rep(0, length(years)), 
                   radardata)

par(mar = c(2, 2, 2, 2))  # Adjust plot margins

radarchart(radardata,
           axistype = 1,
           pcol = "#BEF381",
           pfcol = "#89CFF0",
           plwd = 4,
           cglcol = "black",
           axislabcol = "blue",  # Make axis labels clearer
           caxislabels = seq(0, max(vehicle_sum$total_crime), length.out = 5),
           title = "Vehicle Crime Rate from 2021 to 2024"
)


#___________________________Crime Piechat__________________________________


view(crime)

rober = crime %>%
  filter(`Crime type` == "Robbery" & Year == "2023") %>%
  group_by(Month) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create the pie chart with percentage labels
ggplot(rober, aes(x = "", y = percentage, fill = as.factor(Month))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Robberies by Month (2023)", fill = "Month") +
  theme_minimal()

#___________________________________________Drug________


drug_offences <- subset(crime, `Crime type` == "Drugs" & Year == 2023 & counties %in% c("Bristol, City of", "Cornwall"))

# Create the boxplot



ggplot(drug_offences, aes(x = counties, y = population / 10000, fill = counties)) + 
  geom_boxplot() +
  labs(title = "Drug Offence Rate per 10,000 People in 2023",
       x = "County",
       y = "Drug Offence Rate (per 10,000 people)")+
  theme_minimal()



# Filter data for drug offences
drug_offences = crime %>%
  filter(`Crime type` == "Drugs" & Year == "2023")
view(drug_offences)


# Summarize data for Cornwall
cornwall_data <- drug_offences %>%
  filter(counties == "Cornwall") %>%
  distinct(`LSOA code`, .keep_all = TRUE) %>%
  summarise(total_population = sum(population),
            total_drug_offences = n())
cornwall_data


# Summarize data for Bristol
bristol_dataset <- drug_offences %>%
  filter(counties == "Bristol, City of") %>%
  distinct(`LSOA code`, .keep_all = TRUE) %>%
  summarise(total_population = sum(population),
            total_drug_offences = n())
bristol_dataset

cornwall_data <- cornwall_data %>%
  mutate(offence_rate = (total_drug_offences / total_population) * 10000)

bristol_dataset <- bristol_dataset %>%
  mutate(offence_rate = (total_drug_offences / total_population) * 10000)


combined_data <- bind_rows(
  cornwall_data %>% mutate(county = "Cornwall"),
  bristol_dataset %>% mutate(county = "Bristol, City of")
)
combined_data

ggplot(combined_data, aes(x = county, y = offence_rate, fill = county)) +
  geom_boxplot() +
  labs(title = "Distribution of Drug Offence Rates (2023)",
       x = "Location",
       y = "Offence Rate (per 10,000)") +
  theme_minimal()



#_____________________________Broadband Speed_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

speed <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/broadband_cleaned.csv")
colnames(speed)

#average download speed for both counties
ggplot(speed, aes(x = County, y = AvgDownSpeed, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Download Speed by County",
       x = "County",
       y = "AvgDownSpeed") +
  theme_minimal()


#average and maximum speed for cornwall
cornwall_final = speed%>%
  filter(County == "CORNWALL")%>%
  group_by(Town_City) %>% 
  summarize(
    AvgDownSpeed=mean(AvgDownSpeed),
    MaxDownSpeed=max(MaxDownSpeed)
  ) %>% 
  pivot_longer(cols=c(AvgDownSpeed,MaxDownSpeed),names_to="SpeedType",values_to = "Speed")


ggplot(cornwall_final, aes(x = `Town_City`, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity" , position = "dodge") +
  labs(title = "Average and Maximum Download Speeds by Town/City in Cornwall",
       x = "Town_City",
       y = "Speed )") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top")


#average and maximum speed for bristol
bristol_final = speed%>%
  filter(County == "CITY OF BRISTOL")%>%
  group_by(`Town_City`) %>% 
  summarize(
    AvgDownSpeed=mean(AvgDownSpeed),
    MaxDownSpeed=max(MaxDownSpeed)
  ) %>% 
  pivot_longer(cols=c(AvgDownSpeed,MaxDownSpeed),names_to="SpeedType",values_to = "Speed")


ggplot(bristol_final, aes(x = `Town_City`, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity" , position = "dodge") +
  labs(title = "AvgDownSpeed in Bristol",
       x = "Speed Type",
       y = "Speed ") +
  theme_minimal() 





#_____________________house_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________


house <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/combined_house_data.csv")
colnames(data)
head(house)


house_years = house %>%
  filter(Year >= 2020) %>%
  group_by(Year, `Town_City`) %>%
  summarise(avg_price = mean(Price))

# Filter data for 2023
house_2023 = house_years %>%
  filter(Year == 2023)

view(house_2023)
colnames(house_2023)

# Bar chart for 2023 average house prices by Town
ggplot(house_2023, aes(x = `Town_City`, y = avg_price, fill = `Town_City`)) +
  geom_bar(stat = "identity") +
  ggtitle("Average House Price in 2023 by Town in Cornwall") +
  ylab("Average Price") +
  xlab("Town") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Boxplot for 2023 house prices
ggplot(house %>% filter(Year == 2023), aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  ggtitle("House Prices in 2023") +
  ylab("Price") +
  xlab("County") +
  theme_minimal()

house_years_c = house %>%
  filter(Year >= 2020) %>%
  group_by(Year, County) %>%
  summarise(avg_price = mean(Price))

# Line graph for average house prices from 2020 to 2023
ggplot(house_years_c, aes(x = Year, y = avg_price, color = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ggtitle("Average House Price from 2020 to 2023") +
  ylab("Average Price") +
  xlab("Year") +
  theme_minimal()

housing_2023<- house%>% 
  filter(Year==2023) %>% 
  group_by(County) %>% 
  summarise(average_house_price_2023=mean(Price))

ggplot(housing_2023,aes(x=County,y=average_house_price_2023,fill=County))+
  geom_bar(stat="identity")+
  ggtitle("Average Housing Price in 2023")+
  ylab("Average Price")+
  xlab("County")+
  theme_minimal()



#_____________________________School_______________________________________________



#boxplot
school <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/schoolcleaned.csv")

View(school)

data = school %>%
  filter(YEAR == 2022)


ggplot(data, aes(x = COUNTY, y = ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Scores for 2023 by County",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()


#bristol line graph

bristol_2021_2022 = school %>%
  filter(YEAR == 2021)%>%
  filter(COUNTY=="Bristol")

ggplot(bristol_2021_2022, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Bristol Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#theme_minimal()


#cornwall

cornwall_2021_2022 = school %>%
  filter(YEAR == 2021)%>%
  filter(COUNTY=="Cornwall")

ggplot(cornwall_2021_2022, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "green") +
  geom_point() +
  labs(title = "Cornwall Schools: Average Attainment 8 Score (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




