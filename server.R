library(shiny)
library(plyr)
library(ggplot2)
require(googleVis)
source("dataprocess.R")

shinyServer(function(input, output) { # server is defined within
  # these parentheses
  output$textDisplay <- renderText({ # mark function as reactive
    # and assign to
    # output$textDisplay for
    # passing to ui.R
    paste0("You have input '", input$dateRange[1], # from the text
           "'Date To", input$dateRange[2], # input control as
           " NA Process Type ", input$NAType
    )
  })
  
  FilterDataset <-reactive({PrepData(data, input$dateRange[1], 
                                   input$dateRange[2], input$NAType)
    })
  
  # Prepare dataset
  dataTable <- reactive({
    PrepDataset(FilterDataset())
  })
  
  week.data <- reactive({
    PrepWeekData(FilterDataset())
  })
  
  # Render data table
  output$dTable <- renderDataTable({
    dataTable()
  } #, options = list(bFilter = FALSE, iDisplayLength = 50)
  )
  
  ##histogram "Average Daily Pattern"
  ##dlyActivity <- ddply(FilterDataset(),"date", summarise, steps=sum(steps))
  output$popHist <- renderGvis({
    
    dlyActivity <- ddply(FilterDataset(),"date", summarise, steps=sum(steps))
    popHist <- gvisHistogram(data.frame(dlyActivity$steps), options=list(
      height="500px",
      title= "Average Daily Activity Pattern",
      histogram="{ hideBucketItems:true, bucketSize:10}",
      hAxis="{ title:'Steps per day'}",
      yAxis="{ title:'Frequency'}"
    ))
    
    return(popHist)
    ##print(ggplot(dlyActivity, aes(dlyActivity$steps)) + geom_histogram())
    ##print(graph1)
  })
  
  output$TestHist <- renderPlot({
    graphtest<- PrepHistogram(FilterDataset())
    return(graphtest)
  })
  

  
  output$testWeek <- renderPlot({
    week.data1 <- data.frame(PrepWeekData(FilterDataset()))
    g <- ggplot(week.data1, aes(interval, steps))
    g <- g + geom_line(colour="blue")
    g <- g + facet_wrap(~DayType, nrow=2)
    g <- g + geom_smooth(method="lm", se= FALSE, col="red", aes(group=1))
    g <- g + labs(x="5-minute interval") + labs(y="Average steps over day")
    g <- g + labs(title= "Projected Daily activity over weekday vs Weekend")
    print (g)
  })
})