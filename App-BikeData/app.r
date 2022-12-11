
ui <- fluidPage(
  
  setBackgroundImage(src = "https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEiF9h1DWI7ANnK6ve9gUKkbFjR4ybAi-oTqNHtKUWfewPIH1i-cbS83gdcav5tWiXbxgzUJYJGJRmxzWM2gq1_AXVKBOBsAHooSN5CxpYAGm5y1BUNO7JrK3Eny9J4WxaENamZcdwH2dbJzlg79eInX6LtnR3J6vScKJaBnVMLp8q5ZvLJXt197W79Zuw/s16000/wp1867536.jpg"),
  titlePanel("Seoul Bike Data"),
  
  
  sidebarLayout(

    sidebarPanel(
      style = "background-color: #d0dde9;",      
      sliderInput(inputId="Temperature", 
                  label="Temperature:", 
                  value = 0,
                  min=-20,
                  max=40, step=0.1, animate = animationOptions(interval = 400)
      ),
      
      
      sliderInput(inputId="Humidity...", 
                  label="Humidity:", 
                  value = 50,
                  min=-0,
                  max=100, step=1, animate = animationOptions(interval = 400)
      ),
      
      sliderInput(inputId="Visibility..10m.", 
                  label="Visibility (10m):", 
                  value = 50,
                  min=0,
                  max=2000, step=1, animate = animationOptions(interval = 400)
      ),
      
      sliderInput(inputId="Solar.Radiation..MJ.m2.", 
                  label="Solar.Radiation (MJ/m^2):", 
                  value = 0,
                  min=0,
                  max=4, step=0.01, animate = animationOptions(interval = 400)
      ),
      
      sliderInput(inputId="Rainfall.mm.", 
                  label="Rainfall(mm):", 
                  value = 0,
                  min=0,
                  max=100, step=1, animate = animationOptions(interval = 400)
      ),
      
      sliderInput(inputId="Wind.speed..m.s.", 
                  label="Wind speed (m/s):", 
                  value = 0,
                  min=0,
                  max=8, step=0.1, animate = animationOptions(interval = 400)
      ),
      
      sliderInput(inputId="Snowfall..cm.", 
                  label="Snowfall (cm):", 
                  value = 0,
                  min=0,
                  max=10, step=0.1, animate = animationOptions(interval = 400)
      ),
      
      selectInput("Seasons",label="Seasons:", 
                  choices = list("Autumn", "Spring",
                                 "Summer", "Winter"), selected = "Autumn"
      ), 
      
      h5(strong("Ausstattung:"),align="left"),
      
      checkboxInput(inputId="Holiday", label="Holiday", value = FALSE),
    ),
    
    mainPanel(
      
      div(
        align = "center",
        plotOutput(outputId = "BarChart")
      ),
      
      plotOutput(outputId = "BoxPlot"),
    )
  )
)

server <- function(input, output) {
  
  prognose_list <- list()
  
  
  
  output$BarChart <- renderPlot({
    for (hour in 0:23){
      
      prognose <- reactive({
        
        X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday","Snowfall..cm.","Rainfall.mm.","Solar.Radiation..MJ.m2.","Visibility..10m.","Humidity...", "Wind.speed..m.s.")]
        
        X[1,"Hour"] <- hour
        X[1,"Temperature..C."] <- input$Temperature 
        X[1,"Snowfall..cm."] <- input$Snowfall..cm.
        X[1,"Rainfall.mm."] <- input$Rainfall.mm.
        X[1,"Visibility..10m."] <- input$Visibility..10m.
        X[1,"Seasons"] <- as.factor(input$Seasons)
        X[1,"Humidity..."] <- input$Humidity...
        X[1,"Holiday"] <- as.factor(ifelse(input$Holiday == FALSE, "No Holiday", "Holiday"))
        X[1,"Solar.Radiation..MJ.m2."] <- input$Solar.Radiation..MJ.m2.
        X[1,"Wind.speed..m.s."] <- input$Wind.speed..m.s.
        
        prognosevektor <- predict(model,X)
        prog <- prognosevektor[1]
        prog <- round(prog,digits=2)
        prog
      })
      prog <- prognose()
      if (prog < 0){
        prog <- 0
      }
      prognose_list <- append(prognose_list, prog)
    }
    
    values <- c(unlist(prognose_list))
    labels <- c(0:23)
    par(bg = "#d0dde9")
    bar_chart <- barplot(values, names.arg=labels, xpd=TRUE, las=2, main = "Forecast of rented bikes per hour", xlab="Hour", ylab="Rented Bike Count", font.lab=2, col.lab="#5787af", col="#5787af", ylim=c(0,2700), border = "white")
    for (i in 1:length(values)) {
      text(bar_chart[i], values[i], labels = round(values[i], digits=0), las=2, pos = 3, cex = 0.8)
    }
    
  })
  
  output$BoxPlot <- renderPlot({
    values <- unlist(Daten[,"Rented.Bike.Count"])
    
    par(bg = "#d0dde9")
    
    boxplot(main = "Rented Bike Count over the whole year (01.12.2017 - 30.11.2018) in Seoul", values, horizontal = TRUE, col="#5787af", xlab="Rented Bike Count", font.lab=2, col.lab="#5787af")
  })
}


shinyApp(ui = ui, server = server)
