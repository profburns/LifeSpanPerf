
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Playing with ARIMA Sim"),
    sidebarLayout(

      sidebarPanel(
         textInput("growthSlope", "Slope of Initial Rise",
                   value = round(rnorm(1,1,.3),2)),
         textInput("lengthPlatau", "How Long Till Decline", value = round(rnorm(1,35,5))),
         textInput("declineSlope", "Slope of Exit", value = round(rnorm(1,-1.5,.3),2)),
         actionButton("refresh", "Refresh")),
      mainPanel(
         plotOutput("distPlot"),
         textOutput("totPerf"))
      )
   )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     #Change Mean to Input for Shiny App
     gp_df1<-arima.sim(model = list(order=c(0,1,0)), n = 24, mean = as.numeric(input$growthSlope))
     #plot(gp_df1)
     
     #Change n to Input from component
     gp_df2<-arima.sim(model = list(ma=1), sd=.3, n = as.numeric(input$lengthPlatau))
     #plot(gp_df2)
     
     #Change mean to Input from component
     gp_df3<-arima.sim(model = list(order=c(0,1,0)), n = 50, mean = as.numeric(input$declineSlope))
     #plot(gp_df3)
     #ts.plot(gp_df1,gp_df2,gp_df3)
     
     gp_df4<-(c(gp_df1,gp_df2+gp_df1[24],gp_df3+gp_df2[length(gp_df2)]+gp_df1[24]))
     FirstTwo<- length(gp_df1)+length(gp_df2)
     FirstNegative <- which(gp_df4[FirstTwo:length(gp_df4)] < 0)[1]
     FirstNegative <- FirstNegative + FirstTwo - 1
     gp_df4[length(gp_df4)]<-ifelse(is.na(FirstNegative),0,gp_df4[length(gp_df4)])
     FirstNegative <- ifelse(is.na(FirstNegative),length(gp_df4),FirstNegative)
     gp_df5<-ts(gp_df4[1:FirstNegative])
     
     plot(gp_df5)
     
     output$totPerf <- renderText(c("Total Performance for Employee is ",{round(sum(sum(gp_df5)),2)}))
  
   })
   
   observe({
     input$refresh
     updateNumericInput(session, "growthSlope", value = round(rnorm(1,1,.3),2))
     updateNumericInput(session, "lengthPlatau", value = round(rnorm(1,35,5)))
     updateNumericInput(session, "declineSlope", value = round(rnorm(1,-1.5,.3),2))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

