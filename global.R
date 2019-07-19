# global.R

# shiny app to show how temperatures for lower American and Yuba Rivers fluctuate through time

library(CDECRetrieve)
library(plyr)
library(dplyr)
library(lubridate)
library(leaflet)
library(shiny)
library(ggplot2)
library(baydeltautils)

setwd("C:/Users/JSpector/Documents/YubaAmericantempComparison")

# 25 is sensor code for temperature

yrs <- cdec_query("YRS", 25, "H", "2001-01-10", "2017-05-16") # data only available hourly and until 5/16/2017
lar <- cdec_query("AWP", 25, "E", "2001-01-10", "2017-05-16") # 15 min data is available from 2001-01-10 through the present

# remove duplicate columns
yrs <- yrs %>% select(-c(agency_cd, location_id, parameter_cd))

# rename duplicate columns

yrs <- rename(yrs, yrs_parameter_value = parameter_value)
lar <- rename(lar, lar_parameter_value = parameter_value)

# filter data to remove sensor errors from raw data

yrs <- yrs[yrs$yrs_parameter_value > 40,]
lar <- lar[lar$lar_parameter_value < 100 & lar$lar_parameter_value > 45,]

# average data by day

yrs <- yrs %>% group_by(month=floor_date(datetime, "day")) %>%
  summarize(average_yrs=mean(yrs_parameter_value))

lar <- lar %>% group_by(month=floor_date(datetime, "day")) %>%
  summarize(average_lar=mean(lar_parameter_value))

# join the datasets so only have single time column

joined_data <- join(yrs, lar, by="month", type="inner")

# add column for WY month
joined_data$WYMonth <- wyMonth(joined_data$month)

# remove NA line
joined_data <- na.omit(joined_data)