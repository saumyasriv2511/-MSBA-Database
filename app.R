library(dplyr)
library(plotly)
library(shiny)

df <- read.csv("indicator_modified_subset.csv")
df$Year<-as.factor(df$Year)

  
server<-function(input, output) {
  
  # Fill in the spot we created for a plot
  output$WDIPlot <- renderPlotly({
    # print(input$IndicatorName)
    df2<-df%>%filter(IndicatorName==input$indicator)
    years <- df2$Year[order(df2$Value, decreasing = TRUE)]
    
    ggplot(data = subset (df2, Year %in% years [1 : 20]),aes(x=Year,y=Value,fill=CountryName))+geom_bar(stat="identity")+guides(fill=guide_legend(title="BRICS Countries")) + ylab(input$indicator) + xlab("Year")

  })
}
ui<-fluidPage( 
  headerPanel("World Bank Indicators: 1960-2013"),
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("indicator", "Select Indicators",selected = TRUE, 
                  choices=df$IndicatorName),
      hr(),
      helpText("Data from World Bank Indicators.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotlyOutput("WDIPlot"),width=7
    )
    
  )
)
shinyApp(ui = ui, server = server)
