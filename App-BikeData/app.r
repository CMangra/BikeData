
ui <- fluidPage(
  
  titlePanel("Seoul Bike Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Wohnung:",align="left"),
      hr(style="height: 1px; background: black"),
      
      sliderInput(inputId = "Hour",
                  label = "Hour:",
                  min = 0,
                  max = 23,
                  value = 12
      ),
      
      sliderInput(inputId="Temperature", 
                  label="Temperature:", 
                  value = 0,
                  min=-17.80,
                  max=39.40, step=0.1
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
                  max=50, step=1
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
  
  prognose <- reactive({
    
    X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday","Snowfall..cm.","Rainfall.mm.","Solar.Radiation..MJ.m2.","Visibility..10m.","Humidity...")]
    
    X[1,"Hour"] <- input$Hour  ## Obacht, muss raus
    X[1,"Temperature..C."] <- input$Temperature 
    X[1,"Snowfall..cm."] <- input$Snowfall..cm.
    X[1,"Rainfall.mm."] <- input$Rainfall.mm.
    X[1,"Solar.Radiation..MJ.m2."] <- input$Solar.Radiation..MJ.m2.
    X[1,"Visibility..10m."] <- input$Visibility..10m.
    X[1,"Seasons"] <- as.factor(input$Seasons)
    X[1,"Humidity..."] <- input$Humidity...
    X[1,"Holiday"] <- as.factor(ifelse(input$Holiday == FALSE, "No Holiday", "Holiday"))
    prognosevektor <- predict(model,X)
    prog <- prognosevektor[1]
    prog <- round(prog,digits=2)
    prog
  })         


  output$Verteilung <- renderPlot({
    
    prog <- prognose()
    X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday","Snowfall..cm.","Rainfall.mm.","Solar.Radiation..MJ.m2.","Visibility..10m.","Humidity...")]
    y <- Daten[,"Rented.Bike.Count"]
    abweichungen <- y-predict(model,X)
  
    hist(prog+abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten",xlim=c(0,15), breaks = 2)
    
  })

  output$Prognose <- renderText({
    
    prog <- prognose()
    
    Ausgabe <- paste("Durchschnittliche Miete: ", prog," Euro")
  })
  
}


shinyApp(ui = ui, server = server)
