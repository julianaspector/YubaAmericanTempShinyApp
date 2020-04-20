# global.R

# shiny app to show how temperatures for lower American and Yuba Rivers fluctuate through time

library(CDECRetrieve)
library(plyr)
library(dplyr)
library(lubridate)
library(leaflet)
library(shiny)
library(ggplot2)

# function for wyMonth
wyMonth <- function(date, wyStart=10) {
  mo <- lubridate::month(date)
  return(factor(month.abb[mo], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}

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

#ui.R


ui <- fluidPage(
  titlePanel("Daily Averaged Water Temperatures for Lower American River (at William B Pond Park) and Lower Yuba River (at Smartsville)"),
  sidebarPanel(fluidRow(column=12, 
                        dateRangeInput("dateRange", 
                                       label="Date Range", 
                                       start="2001-01-10", 
                                       end="2017-05-16", 
                                       min="2001-01-10", 
                                       max="2017-05-16", 
                                       startview="decade")
  ),
  br(),
  br(),
  fluidRow(column=12,
           leafletOutput("siteMap"))
  ),
  mainPanel(fluidRow(column=12,
                     plotOutput("linePlot")),
            # monthly plots
            fluidRow(column=12,
                     plotOutput("monthOutput"))
  )
)

#server.R
# create map and add basemap
siteMap <- leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap")
shinyServer(
  function(input,output){
    # name of y axis label
    my.ylab = expression(atop(paste("Temperature (", degree,"F)")))
    output$linePlot <- renderPlot({
      # data range defined by input$dateRange
      desiredData <- joined_data[joined_data$month < input$dateRange[2] & joined_data$month > input$dateRange[1],]
      gg<-ggplot(data=desiredData)+
        geom_line(aes(x=month, y=average_lar, color="Lower American River"))+
        geom_line(aes(x=month, y=average_yrs, color="Lower Yuba River"))+
        scale_color_manual(name="River", values=c("Lower American River"= "#ef8a62", "Lower Yuba River"= "#67a9cf"))+
        ylab(my.ylab)+
        xlab("Time")
      gg
    })
    # monthly plots
    output$monthOutput <- renderPlot({
      dataWanted <- joined_data[joined_data$month < input$dateRange[2] & joined_data$month > input$dateRange[1],]
      ggMonthly <- ggplot(data=dataWanted)+
        geom_point(aes(x=month, y=average_lar, color="Lower American River"))+
        geom_point(aes(x=month, y=average_yrs, color="Lower Yuba River"))+
        facet_wrap(~WYMonth, scales="free_y")+
        scale_color_manual(name="River", values=c("Lower American River"= "#ef8a62", "Lower Yuba River"= "#67a9cf"))+
        ylab(my.ylab)+
        xlab("Year")
      ggMonthly
    })
    output$siteMap <- renderLeaflet({
      siteMap <- addMarkers(siteMap, lat=39.2351722717285, lng=-121.274124145508, popup="Yuba River at Smartsville")%>%
        addMarkers(siteMap, lat=38.591, lng=	-121.332, popup="American River at William B Pond Park")
      siteMap
    })
  }
)