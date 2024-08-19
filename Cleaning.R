
#libraries 
library(dplyr)
library(tidyverse)
library(readr)

#______________________House Price  Cleaning____________________________________________________________________________________________________________________________________________________________________________ 

#defining column cames on dataset

column_names <- c("Transaction_ID", "Price", "Transaction_Date", "Postcode", 
                  "Property_Type", "Old_New", "Duration", "PAON", "SAON", 
                  "Street", "Locality", "Town_City", "District", "County", 
                  "PPD_Category_Type", "Record_Status")

#data sets loading with column names 

house_2020 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/house price/pp-2020.csv", col_names = column_names)
house_2021 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/house price/pp-2021.csv", col_names = column_names)
house_2022 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/house price/pp-2022.csv", col_names = column_names)
house_2023 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/house price/pp-2023.csv", col_names = column_names)

#combine data sets
combined_house_data = bind_rows(house_2020, house_2021, house_2022, house_2023)

# Data cleaning 
cleaned_housing_data <- combined_house_data %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(Year = year(Transaction_Date)) %>% 
  select(Price, Postcode,Year,Town_City,County) %>% 
  na.omit() %>%
  distinct()

dim(cleaned_housing_data )
View(cleaned_housing_data )
colnames(cleaned_housing_data )

# Save the cleaned data to a new CSV file
write_csv(cleaned_housing_data, "C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/combined_house_data.csv")





#______________________Cleaning Broadband Speed______________________________________________________________________________________________________________________________________________________________________________________

#dataset

broadband_performance <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/broadbandspeed/201805_fixed_pc_performance_r03.csv")

#Selecting Relevant Columns
broadband_selected <- broadband_performance %>%
  select("postcode_space",
         "Median download speed (Mbit/s)",
         "Median upload speed (Mbit/s)",
         "Average upload speed (Mbit/s)",
         "Maximum upload speed (Mbit/s)",
         "Average download speed (Mbit/s)",
         "Maximum download speed (Mbit/s)",
  )

#Checking Null Values by summary
na_summary <- sapply(broadband_selected, function(x) sum(is.na(x)))
print(na_summary)

#Cleaning the data set
broadband_clean <-broadband_selected %>% 
  rename(
    Postcode = postcode_space,
    MedianDownSpeed = `Median download speed (Mbit/s)`,
    MedianUpSpeed = `Median upload speed (Mbit/s)`,
    AvgUpSpeed = `Average upload speed (Mbit/s)`,
    MaxUpSpeed = `Maximum upload speed (Mbit/s)`,
    AvgDownSpeed = `Average download speed (Mbit/s)`,
    MaxDownSpeed = `Maximum download speed (Mbit/s)`
  ) %>% 
  na.omit() %>% 
  distinct()

#Checking for null values after Cleaning
na_summary_cleandata <- sapply(broadband_clean, function(x) sum(is.na(x)))
print(na_summary_cleandata)

View(broadband_clean)

#Joining with Housing Data with common PostCode Column

#Selecting only 3 column  from housing
housing_selected <- cleaned_housing_data %>% 
  select(Postcode,Town_City,County)

#Using Inner Join for broadband and housing merge  
broadband_M_housing = inner_join(housing_selected,broadband_clean,by="Postcode");
View(broadband_M_housing)

na_merged <- sapply(broadband_M_housing, function(x) sum(is.na(x)))
print(na_merged)
dim(broadband_M_housing)


# Create the 'broadband_final_data' 
broadband_final_data <- broadband_M_housing %>%
  distinct()

# Check the dimensions of 'broadband_final_data'
dim(broadband_final_data)

# View the 'broadband_final_data' data frame
View(broadband_final_data)

#save the dataset
write_csv(broadband_final_data, "C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/broadband_cleaned.csv")


#________________CRIME Cleaning____________________________________________________________________________________________________________________________________________________________________________________________________________________

#cornWall

#2021

cornWall1 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-07/2021-07-devon-and-cornwall-street.csv")
cornWall2 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-08/2021-08-devon-and-cornwall-street.csv")
cornWall3 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-09/2021-09-devon-and-cornwall-street.csv")
cornWall4 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-10/2021-10-devon-and-cornwall-street.csv")
cornWall5 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-11/2021-11-devon-and-cornwall-street.csv")
cornWall6 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-12/2021-12-devon-and-cornwall-street.csv")

#2022

cornWall7 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-01/2022-01-devon-and-cornwall-street.csv")
cornWall8 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-02/2022-02-devon-and-cornwall-street.csv")
cornWall9 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-03/2022-03-devon-and-cornwall-street.csv")
cornWall10 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-04/2022-04-devon-and-cornwall-street.csv")
cornWall11 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-05/2022-05-devon-and-cornwall-street.csv")
cornWall12 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-06/2022-06-devon-and-cornwall-street.csv")
cornWall13 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-07/2022-07-devon-and-cornwall-street.csv")
cornWall14 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-08/2022-08-devon-and-cornwall-street.csv")
cornWall15 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-09/2022-09-devon-and-cornwall-street.csv")
cornWall16 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-10/2022-10-devon-and-cornwall-street.csv")
cornWall17 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-11/2022-11-devon-and-cornwall-street.csv")
cornWall18 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-12/2022-12-devon-and-cornwall-street.csv")



#2023

cornWall19 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-01/2023-01-devon-and-cornwall-street.csv")
cornWall20 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-02/2023-02-devon-and-cornwall-street.csv")
cornWall21 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-03/2023-03-devon-and-cornwall-street.csv")
cornWall22 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-04/2023-04-devon-and-cornwall-street.csv")
cornWall23 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-05/2023-05-devon-and-cornwall-street.csv")
cornWall24 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-06/2023-06-devon-and-cornwall-street.csv")
cornWall25 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-07/2023-07-devon-and-cornwall-street.csv")
cornWall26 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-08/2023-08-devon-and-cornwall-street.csv")
cornWall27 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-09/2023-09-devon-and-cornwall-street.csv")
cornWall28 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-10/2023-10-devon-and-cornwall-street.csv")
cornWall29 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-11/2023-11-devon-and-cornwall-street.csv")
cornWall30 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-12/2023-12-devon-and-cornwall-street.csv")


#2024

cornWall31 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-01/2024-01-devon-and-cornwall-street.csv")
cornWall32 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-02/2024-02-devon-and-cornwall-street.csv")
cornWall33 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-03/2024-03-devon-and-cornwall-street.csv")
cornWall34 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-04/2024-04-devon-and-cornwall-street.csv")
cornWall35 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-05/2024-05-devon-and-cornwall-street.csv")
cornWall36 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-06/2024-06-devon-and-cornwall-street.csv")

#Bistrol
#2021
bistrol1 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-07/2021-07-avon-and-somerset-street.csv")
bistrol2 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-08/2021-08-avon-and-somerset-street.csv")
bistrol3 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-09/2021-09-avon-and-somerset-street.csv")
bistrol4 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-10/2021-10-avon-and-somerset-street.csv")
bistrol5 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-11/2021-11-avon-and-somerset-street.csv")
bistrol6 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2021-12/2021-12-avon-and-somerset-street.csv")

#2022
bistrol7 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-01/2022-01-avon-and-somerset-street.csv")
bistrol8 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-02/2022-02-avon-and-somerset-street.csv")
bistrol9 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-03/2022-03-avon-and-somerset-street.csv")
bistrol10 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-04/2022-04-avon-and-somerset-street.csv")
bistrol11 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-05/2022-05-avon-and-somerset-street.csv")
bistrol12 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-06/2022-06-avon-and-somerset-street.csv")
bistrol13 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-07/2022-07-avon-and-somerset-street.csv")
bistrol14 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-08/2022-08-avon-and-somerset-street.csv")
bistrol15 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-09/2022-09-avon-and-somerset-street.csv")
bistrol16 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-10/2022-10-avon-and-somerset-street.csv")
bistrol17 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-11/2022-11-avon-and-somerset-street.csv")
bistrol18 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2022-12/2022-12-avon-and-somerset-street.csv")


#2023
bistrol19 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-01/2023-01-avon-and-somerset-street.csv")
bistrol20 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-02/2023-02-avon-and-somerset-street.csv")
bistrol21 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-03/2023-03-avon-and-somerset-street.csv")
bistrol22 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-04/2023-04-avon-and-somerset-street.csv")
bistrol23 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-05/2023-05-avon-and-somerset-street.csv")
bistrol24 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-06/2023-06-avon-and-somerset-street.csv")
bistrol25 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-07/2023-07-avon-and-somerset-street.csv")
bistrol26 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-08/2023-08-avon-and-somerset-street.csv")
bistrol27 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-09/2023-09-avon-and-somerset-street.csv")
bistrol28 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-10/2023-10-avon-and-somerset-street.csv")
bistrol29 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-11/2023-11-avon-and-somerset-street.csv")
bistrol30 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2023-12/2023-12-avon-and-somerset-street.csv")


#2024
bistrol31 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-01/2024-01-avon-and-somerset-street.csv")
bistrol32 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-02/2024-02-avon-and-somerset-street.csv")
bistrol33 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-03/2024-03-avon-and-somerset-street.csv")
bistrol34 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-04/2024-04-avon-and-somerset-street.csv")
bistrol35 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-05/2024-05-avon-and-somerset-street.csv")
bistrol36 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Crime/2024-06/2024-06-avon-and-somerset-street.csv")


# Combining the Datasets
crime_combined = rbind(
  bistrol1, bistrol2, bistrol3, bistrol4, bistrol5, bistrol6, bistrol7, bistrol8, bistrol9, bistrol10, bistrol11, bistrol12,
  bistrol13, bistrol14, bistrol15, bistrol16, bistrol17, bistrol18, bistrol19, bistrol20, bistrol21, bistrol22, bistrol23,
  bistrol24, bistrol25, bistrol26, bistrol27, bistrol28, bistrol29, bistrol30, bistrol31, bistrol32, bistrol33, bistrol34,
  bistrol35, bistrol36,
  
  cornWall1, cornWall2, cornWall3, cornWall4, cornWall5, cornWall6, cornWall7, cornWall8, cornWall9, cornWall10, cornWall11,
  cornWall12, cornWall13, cornWall14, cornWall15, cornWall16, cornWall17, cornWall18, cornWall19, cornWall20, cornWall21,
  cornWall22, cornWall23, cornWall24, cornWall25, cornWall26, cornWall27, cornWall28, cornWall29, cornWall30, cornWall31,
  cornWall32, cornWall33, cornWall34, cornWall35, cornWall36
)

head(crime_combined)
View(crime_combined)
dim(crime_combined)

#Converting it to a tibble
crime_combined <- crime_combined %>% 
  as_tibble()


#_________________POST CODE TO LSOA_
postcode_to_lsoa <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Postcode to LSOA.csv")
population <- read_csv ("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/Population2011_1656567141570.csv")

#Selecting necessary column
selected_lsoa <- postcode_to_lsoa %>%
  select(`lsoa11cd`, `lsoa11nm`, `ladnm`, `pcds`)

selected_crime <- crime_combined %>% 
  select(Month, `LSOA code`, `Crime type`, `Falls within`)

#Renaming the columns
colnames(selected_lsoa) = c('LSOA code', 'street', 'counties', "postcode")
colnames(population) = c("postcode", "population")


#Cleaning LSOA
clean_lsoa <- selected_lsoa %>% 
  filter(counties %in% c("Bristol, City of","Cornwall")) %>% 
  mutate(postcode=str_trim((substring(postcode,1,6))))

#Checking for duplicates 
any(duplicated(selected_crime$`LSOA code`))
any(duplicated(clean_lsoa$`LSOA code`))

#removing duplicates them
clean_lsoa=unique(clean_lsoa,by="LSOA code")
selected_crime=unique(selected_crime,by="LSOA code")

#Final Cleaning and merging
finalCrime_data= selected_crime %>% 
  left_join(clean_lsoa, by=("LSOA code"),relationship = "many-to-many") %>% 
  mutate(Year=str_trim(substring(Month,1,4))) %>% 
  mutate(Month=str_trim(substring(Month,6,7))) %>% 
  left_join(population,by="postcode") %>% 
  distinct() %>% 
  na.omit()

dim(finalCrime_data)
View(finalCrime_data)


#save the dataset
write_csv(finalCrime_data, "C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/crime_cleaned.csv")



#______________School___________________________________________________

#school dataset
bristol_school21 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/school info/Bristol/2021-2022/801_ks4final.csv")
bristol_school22 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/school info/Bristol/2022-2023/801_ks4final.csv")
cornwall_school21 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/school info/Cornwall/2021-2022/908_ks4final.csv")
cornwall_school22 <- read_csv("C:/Users/Lenovo/Documents/priyanka datascience/Obtained Datasets/school info/Cornwall/2022-2023/908_ks4final.csv")

#Selecting and Transforming Data 

bristol_school21 <- bristol_school21 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2021,COUNTY="Bristol")

bristol_school22 <- bristol_school21 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2022,COUNTY="Bristol")

cornwall_school21<-bristol_school21%>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2021,COUNTY="Cornwall")

cornwall_school22 <- bristol_school21 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2022,COUNTY="Cornwall")


#Combining
bristolCombined=rbind(bristol_school21,bristol_school22)
cornwallCombined=rbind(cornwall_school21,cornwall_school22 )

# Viewing combined data 
View(bristolCombined)
View(cornwallCombined)

schoolCombined=rbind(bristolCombined,cornwallCombined)
View(schoolCombined)


#Cleaning and Filtering Data

schoolCombined <- schoolCombined %>% 
  filter(ATT8SCR!="NE"&ATT8SCR!="SUPP") %>%
  na.omit() %>% 
  distinct()

dim(schoolCombined)
View(schoolCombined)

#save the dataset
write_csv(schoolCombined, "C:/Users/Lenovo/Documents/priyanka datascience/Cleaned Datasets/schoolcleaned.csv")


























