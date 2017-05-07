
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



# exploratory analysis ----------------------------------------------------

plot(tbl_temp)
plot(tbl_humidity)
plot(tbl_rainfall)



# statistical inference ---------------------------------------------------






# predictive modeling -----------------------------------------------------





