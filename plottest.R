library(shiny)

runApp(list(
  ui = pageWithSidebar(
    headerPanel("Test"),
    sidebarPanel(
      sliderInput("width", "Plot Width (%)", min = 0, max = 100, value = 100),
      sliderInput("height", "Plot Height (px)", min = 0, max = 400, value = 400)
    ),
    mainPanel(
      uiOutput("plot.ui"),
      verbatimTextOutput('text1')
    
    )
  ),
  server = function(input, output, session) {
    
    
    
    output$text1=renderPrint({
      mysize=ifelse(input$width==100,100,0)
      mysize   
      cat("\nmysize=",mysize)
      for(i in 1:20) cat("\n",i,"\n")
    })
    output$plot.ui <- renderUI({
      mywidth=input$width
      myheight=input$height
      #plotOutput("plot", width = paste0(mysize,"%"), height = paste0(mysize,"%"))
      plotOutput("plot", width = '100%', height = myheight)
    })
    
    output$plot <- renderPlot({
      plot(1:10)
    })
  }
))