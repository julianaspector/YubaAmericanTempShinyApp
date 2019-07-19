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