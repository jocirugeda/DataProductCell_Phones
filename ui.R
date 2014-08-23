
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(navbarPage("Cell Phones & GDP",
                   
                   tabPanel("Explore Country Zones",
                            sidebarLayout(
                                    
                                sidebarPanel(
                                        titlePanel("Cell Phone Subscriptions and GPD PerCapita by region"),
                                        selectInput("ZONES_region","Region:",c("World","Australia and Oceania",
                                                        "Caribbean","Central America",
                                                        "East Asia","Eastern Europe",
                                                        "Former Soviet Republics",
                                                        "Middle East and North Africa",
                                                        "North America",
                                                        "South America",
                                                        "South Asia",
                                                        "Sub-Saharan Africa","Wester Europe")
                                        ),
                                        
                                        radioButtons("ZONES_plotType", "Plot type",
                                                     c("Time Series"="T", "Country Networks"="N")
                                        ),
                                        hr(),
                                        sliderInput("ZONES_year",
                                                "Networks in Year:",
                                                min = 1980,
                                                max = 2012,
                                                value = 2000),
                                        hr(),
                                        helpText("Data from quandl (http://www.quandl.com/)"),
                                        helpText("Years 1980 to 2012 "),
                                        hr(),
                                        helpText("For Info/Help go to Help Tab ")
                                ),                    
                                mainPanel(
                                       plotOutput("ZONES_plot"),
                                       verbatimTextOutput("ZONES_summary"),                                
                                       dataTableOutput(outputId="ZONES_table")    
                                )
                            ),value="ZONES"        
                   ),
                   tabPanel("Regresion Models",
                            sidebarLayout(
                                    
                                    sidebarPanel(
                                            titlePanel(" Regresion model Cell Phone Subscriptions by region"),
                                            selectInput("REGRESION_region","Region:",c("World","Australia and Oceania",
                                                                                   "Caribbean","Central America",
                                                                                   "East Asia","Eastern Europe",
                                                                                   "Former Soviet Republics",
                                                                                   "Middle East and North Africa",
                                                                                   "North America",
                                                                                   "South America",
                                                                                   "South Asia",
                                                                                   "Sub-Saharan Africa","Wester Europe")
                                            ),                                            
                                            sliderInput("REGRESION_year",
                                                        "Networks from Year:",
                                                        min = 1980,
                                                        max = 2012,
                                                        value = 2000),
                                            hr(),
                                            helpText("Data from quandl (http://www.quandl.com/)"),
                                            helpText("Years 1980 to 2012 "),
                                            hr(),
                                            
                                            helpText("For Info/Help go to Help Tab ")
                                    ),                    
                                    mainPanel(
                                            plotOutput("REGRESION_plot"),
                                            verbatimTextOutput("REGRESION_summary")
                                              
                                    )
                            ),value="REGRESION"        
                   ),
                   tabPanel("Help",
                            includeHTML("base_doc.html")                            
                            ,value="HELP"),id="ACTIVE_PAGE"
                   
))