
# header ------------------------------------------------------------------

# Project:
# Names:
# Date:
# Overview:


# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)



# functions ---------------------------------------------------------------

source(file = "functions.R")

    # # view the data -----------------------------------------------------
    # 
    # names(tbl_temp) # return the column names in tbl_temp
    # 
    # glimpse(tbl_temp) # transposed glimpse of tbl_temp
    # 
    # head(tbl_temp) # look at the first few rows of tbl_temp
    # 
    # tail(tbl_temp) #look at the last few rows of tbl_temp



# data - temperature ------------------------------------------------------

# extract worksheet names
temp_sheets <- excel_sheets(path = "temperature_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_temp_1 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[1], 120)
tbl_temp_2 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[2], 108)
tbl_temp_3 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[3], 228)

# reformat sheet 1, 2 and 3 data
tbl_temp_1 <- reformat_temp_data(tbl_temp_1) 
tbl_temp_2 <- reformat_temp_data(tbl_temp_2) 
tbl_temp_3 <- reformat_temp_data(tbl_temp_3) 

# combine temperature data sets
tbl_temp <- bind_rows(tbl_temp_1, tbl_temp_2, tbl_temp_3)



# data - humidity ---------------------------------------------------------

# extract worksheet names
humidity_sheets <- excel_sheets(path = "relative_humidity_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_humidity_1 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[1], 120)
tbl_humidity_2 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[2], 120)
tbl_humidity_3 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[3], 225)

# reformat sheet 1, 2 and 3 data
tbl_humidity_1 <- reformat_humidity_data(tbl_humidity_1) 
tbl_humidity_2 <- reformat_humidity_data(tbl_humidity_2) 
tbl_humidity_3 <- reformat_humidity_data(tbl_humidity_3) 

# combine temperature data sets
tbl_humidity <- bind_rows(tbl_humidity_1, tbl_humidity_2, tbl_humidity_3)



# data - rainfall ---------------------------------------------------------

# extract worksheet names
rainfall_sheets <- excel_sheets(path = "precipitation_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_rainfall_1 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[1], 120)
tbl_rainfall_2 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[2], 120)
tbl_rainfall_3 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[3], 228)

# reformat sheet 1, 2 and 3 data
tbl_rainfall_1 <- reformat_precip_data(tbl_rainfall_1) 
tbl_rainfall_2 <- reformat_precip_data(tbl_rainfall_2) 
tbl_rainfall_3 <- reformat_precip_data(tbl_rainfall_3) 

# combine temperature data sets
tbl_rainfall <- bind_rows(tbl_rainfall_1, tbl_rainfall_2, tbl_rainfall_3)



# data - yield ------------------------------------------------------------

# extract worksheet names
yield_sheets <- excel_sheets(path = "rice_yield_1981_2016.xlsx")

# import yield data 
tbl_yield <- read_excel(path = "rice_yield_1981_2016.xlsx",
                        sheet = yield_sheets[1], 
                        na = c("", "-"),
                        skip = 1, 
                        n_max = 36)

# reformat sheet 1, 2 and 3 data
tbl_yield <- tbl_yield %>%
    rename(Farming_Area = `Farming area`) %>%
    rename(Harvested_Area = `Harvested area`) %>%
    rename(Yield_per_Rai = `Yield per Rai (kg)`) %>%
    select(Year, Farming_Area, Harvested_Area, Yield_per_Rai)



# data - full  ------------------------------------------------------------

# combine temperature, humidity, precipitation and yield
tbl_data <- tbl_temp %>%
    left_join(tbl_humidity, by = "Date") %>%
    left_join(tbl_rainfall, by = "Date") #%>%
    #left_join(tbl_yield, by = Date)



# exploratory analysis ----------------------------------------------------

plot(tbl_temp)
plot(tbl_humidity)
plot(tbl_rainfall)
plot(tbl_yield)


tbl_yearly <- tbl_data %>%
    filter(Month > 4, Month < 11) %>%
    group_by(Year) %>%
    summarize(avg_temp = mean(Temperature), avg_humi = mean(Humidity), avg_rain = mean(Precipitation)) %>%
    left_join(tbl_yield, by = "Year") %>%
    select(-Farming_Area, -Harvested_Area)




tbl_monthly <- tbl_data %>%
    filter(Month > 4, Month < 11) %>%
    group_by(Year, Month) %>%
    summarize(avg_temp = mean(Temperature), avg_humi = mean(Humidity), avg_rain = mean(Precipitation)) %>%
    mutate(month_temp = paste("temp", Month, sep = "_")) %>%
    mutate(month_humi = paste("humi", Month, sep = "_")) %>%
    mutate(month_rain = paste("rain", Month, sep = "_")) %>%
    select(-Month) %>%
    ungroup() %>%
    spread(month_temp, avg_temp) %>%
    spread(month_rain, avg_rain) %>%
    spread(month_humi, avg_humi) %>%
    group_by(Year) %>%
    summarize_each(funs(sum( ., na.rm = TRUE))) %>%
    left_join(tbl_yield, by = "Year")



# statistical inference ---------------------------------------------------

monthly_fit <- lm(Yield_per_Rai ~ ., data = tbl_monthly)

yearly_fit <- lm(Yield_per_Rai ~ Year, data = tbl_yearly)


# predictive modeling -----------------------------------------------------





