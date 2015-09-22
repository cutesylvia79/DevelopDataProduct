library(shiny)
require(markdown)
shinyUI(
  navbarPage("Steps Analysis from Smart Wearable", 
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Explore the Data",
                      sidebarPanel(
                        dateRangeInput(inputId = "dateRange",
                                       label = "Date range",
                                       start = "2012-10-01",
                                       end = "2012-11-01"
                        ),
                        radioButtons(inputId = "NAType",
                                     label = "NA Treatment",
                                     choices = list("Filter out NA" = "NoNA",
                                                    "Impute NA with Mean" = "ImputeNAMean"))
                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(p(icon("bar-chart"), "Histogram"), 
                                   h4('Histogram of Daily Activity', align = "center"),
                                   ##h5('Show histogram here later'),
                                   htmlOutput("popHist")
                                  ##  plotOutput("popHist")
                                   ##plotOutput(outputid="TestHist")
                                   ##showOutput("themesByYear", "nvd3")
                                  
                          ), # end of "Visualize the Data" tab panel
                          
                          tabPanel(p(icon("line-chart"),"Weekday vs. Weekend", align="center"),
                                   h4('Projected Daily Activity over weekday vs Weekend', align="center"),
                                   ##h5('Show chart later')
                                   plotOutput("testWeek")
                                   ##dataTableOutput(outputId="dWeekTable")
                            
                          ),
                          
                            # Data 
                            tabPanel(p(icon("table"), "Dataset"),
                                     ##h4('Display Dataset here later')
                                     dataTableOutput(outputId="dTable")
                            ) # end of "Dataset" tab panel
                        )
                        
                      )     
             ), # end of "Explore Dataset" tab panel
             
             
             
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
             )
  
  )