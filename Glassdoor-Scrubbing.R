library(tidyverse)
library(splitstackshape)
options(scipen = 999)
# Pull in City Level Income Data from Census Data
data.income <- read.csv("Income-Data.csv") %>% 
  select(State_ab,City,Median_Income = Median, Mean_Income = Mean, Std_Income = Stdev) %>% 
  mutate(City_State = paste(City,State_ab, sep = ", ")) %>% 
  group_by(City_State) %>% 
  summarize(City_Salary_Median = median(Median_Income))

# Pull in City Level Rent Data from Zillow
# https://www.zillow.com/research/data/ (Rent 2 BR)
data.rent <- read_csv("Rent-Data.csv") %>% 
  mutate(City_State = paste(RegionName,State,sep =", ")) %>% 
  select(City_State, Rent_CY = `2019-07`, Rent_PY = `2018-07`) %>% 
  mutate(Rent_PY = ifelse(is.na(Rent_PY),Rent_CY,Rent_PY))

# Public Transportation Data
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
data.public.trans <- read_csv("Public-Trans-Data.csv", skip = 1) %>% 
  select(Area = `Geographic Area_1`,Pct_Commute = Percent) %>% 
  mutate(City_State = sub("(,\\s+[A-Z]+).*", "\\1", Area)) %>%
  separate(col = City_State, into = c("City","State"), sep = "," , remove = F) %>% 
  cSplit("City", sep = "-", direction = "long") %>% 
  mutate(State = trimws(State),
         City_State = paste(City,State,sep = ", ")) %>%
  select(City_State, Pct_Commute) %>% 
  group_by(City_State) %>% 
  summarize(Pct_Commute = mean(Pct_Commute))

data.income2 <- read_csv("Income-Data2.csv") %>% 
  select(Area = `GEO.display-label`,Median_Income = HD01_VD01) %>% 
  mutate(City_State = sub("(,\\s+[A-Z]+).*", "\\1", Area)) %>%
  separate(col = City_State, into = c("City","State"), sep = "," , remove = F) %>% 
  cSplit("City", sep = "--", direction = "long") %>% 
  mutate(State = trimws(State),
         City_State = paste(City,State,sep = ", ")) %>%
  select(City_State, Median_Income) %>% 
  group_by(City_State) %>% 
  summarize(Median_Income = mean(Median_Income))

# Commute Time Data
data.commute <- read_csv("Commute-Data.csv", skip = 1) %>% 
  select(Area = `Geographic Area_1`,Commute_Time = Minute) %>% 
  mutate(City_State = sub("(,\\s+[A-Z]+).*", "\\1", Area)) %>%
  separate(col = City_State, into = c("City","State"), sep = "," , remove = F) %>% 
  cSplit("City", sep = "-", direction = "long") %>% 
  mutate(State = trimws(State),
    City_State = paste(City,State,sep = ", ")) %>%
  select(City_State, Commute_Time) %>% 
  group_by(City_State) %>% 
  summarize(Commute_Time = mean(Commute_Time))
  

# Pull in City level Population data from simple maps
# https://simplemaps.com/data/us-cities
data.city <- read.csv("City-Data.csv") %>% 
  select(City = city,
         State = state_id,
         County_Fips = county_fips,
         County = county_name,
         Lat=lat, 
         Lon = lng,
         Pop_Total = population,
         Pop_Proper = population_proper,
         Pop_Density = density) %>% 
  mutate(City_State = paste(City,State,sep = ", ")) %>% select(-City,-State)


data.econ <- data.city %>% 
  left_join(data.rent, by = "City_State") %>% 
  left_join(data.public.trans, by = "City_State") %>% 
  left_join(data.commute, by = "City_State") %>% 
  left_join(data.income, by = "City_State") %>% 
  left_join(data.income2, by = "City_State")


# Pull in data from Glassdoor scraping tool and join other tables
data.raw <- readRDS("Data-Raw-Total.Rds") %>% 
  separate(col = City_State, into = c("City","State"), sep = "," , remove = F) %>% 
  separate(col = salary_Range, into = c("Salary_Low","Salary_High"), sep = "-") %>%
  left_join(data.econ, by = "City_State") %>% 
  mutate(GD_Salary_Low = parse_number(Salary_Low)*1000,
         GD_Salary_High = parse_number(Salary_High)*1000,
         GD_Salary_Avg = (GD_Salary_Low + GD_Salary_High)/2,
         State = trimws(State),
         Company_Rating = as.numeric(Company_Rating),
         Post_Date = ifelse(grepl("hr",Post_Date),1,parse_number(Post_Date)),
         City_Salary_Median = ifelse(City == "United States",
                                     mean(City_Salary_Median,na.rm = TRUE),
                                     City_Salary_Median),
         Salary_Ratio_Low = GD_Salary_Low / City_Salary_Median,
         Salary_Ratio_Avg = GD_Salary_Avg / City_Salary_Median,
         Salary_Ratio_High = GD_Salary_High / City_Salary_Median,
         Salary_Ratio_Current = 100000 / City_Salary_Median)
