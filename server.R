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