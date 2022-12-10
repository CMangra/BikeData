
ui <- fluidPage(
  
  titlePanel("Seoul Bike Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Wohnung:",align="left"),
      hr(style="height: 1px; background: black"),
      
      sliderInput(inputId="Temperature", 
                  label="Temperature:", 
                  value = 0,
                  min=-20,
                  max=40, step=0.1
      ),
      
      sliderInput(inputId="Humidity...", 
                  label="Humidity:", 
                  value = 50,
                  min=-0,
                  max=100, step=1
      ),
      
      sliderInput(inputId="Visibility..10m.", 
                  label="Visibility (10m):", 
                  value = 50,
                  min=0,
                  max=2000, step=1
      ),
      
      sliderInput(inputId="Solar.Radiation..MJ.m2.", 
                  label="Solar.Radiation (MJ/m^2):", 
                  value = 0,
                  min=0,
                  max=4, step=0.01
      ),
      
      sliderInput(inputId="Rainfall.mm.", 
                  label="Rainfall(mm):", 
                  value = 0,
                  min=0,
                  max=100, step=1
      ),
      
      sliderInput(inputId="Snowfall..cm.", 
                  label="Snowfall (cm):", 
                  value = 0,
                  min=0,
                  max=10, step=0.1
      ),
      
      selectInput("Seasons",label="Seasons:", 
                  choices = list("Autumn", "Spring",
                                 "Summer", "Winter"), selected = "Autumn"
      ), 
      
      h5(strong("Ausstattung:"),align="left"),
      
      checkboxInput(inputId="Holiday", label="Holiday", value = FALSE),
    ),
    
    mainPanel(
      
      plotOutput(outputId = "Verteilung"),
      
      textOutput("Prognose"),
      
    )
  )
)

server <- function(input, output) {
  
  prognose_list <- list()
  
  
  
  output$Verteilung <- renderPlot({
    for (hour in 0:23){
      
      prognose <- reactive({
        
        X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday","Snowfall..cm.","Rainfall.mm.","Solar.Radiation..MJ.m2.","Visibility..10m.","Humidity...")]
        
        X[1,"Hour"] <- hour
        X[1,"Temperature..C."] <- input$Temperature 
        X[1,"Snowfall..cm."] <- input$Snowfall..cm.
        X[1,"Rainfall.mm."] <- input$Rainfall.mm.
        X[1,"Visibility..10m."] <- input$Visibility..10m.
        X[1,"Seasons"] <- as.factor(input$Seasons)
        X[1,"Humidity..."] <- input$Humidity...
        X[1,"Holiday"] <- as.factor(ifelse(input$Holiday == FALSE, "No Holiday", "Holiday"))
        X[1,"Solar.Radiation..MJ.m2."] <- input$Solar.Radiation..MJ.m2.
        
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
    
    print(unlist(prognose_list))
    
    values <- c(unlist(prognose_list))
    labels <- c(0:23)
    bar_chart <- barplot(values, names.arg=labels, xpd=TRUE, las=2, xlab="Hour", font.lab=2, col.lab="#69b3a2", col="#69b3a2", ylim=c(0,2700))
    
    for (i in 1:length(values)) {
      text(bar_chart[i], values[i], labels = round(values[i], digits=0), las=2, pos = 3, cex = 0.8)
    }
  })
}


shinyApp(ui = ui, server = server)
