


################
# Im folgenden Abschnitt wird das User Interface (UI) definiert
ui <- fluidPage(
  
  # Titel der App
  titlePanel("Seoul Bike Data"),
  
  # Layout für die Eingaben in die App und die Ausgaben
  sidebarLayout(
    
    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(
      
      # Eine Überschrift mit Linie darunter
      h3("Wohnung:",align="left"),
      hr(style="height: 1px; background: black"),
      
      # Ein Slider für die Zeit
      sliderInput(inputId = "Hour",
                  label = "Hour:",
                  min = 0,
                  max = 23,
                  value = 12
      ),
      
      # Ein Slider für die Temperatur
      sliderInput(inputId="Temperature", 
                  label="Temperature:", 
                  value = 0,
                  min=-17.80,
                  max=39.40, step=0.1
      ),
      
      # Die Lage als Auswahlliste
      # die Auswahlmöglichkeiten sind "normale Lage", "gute Lage" und "beste Lage",
      # die entsprechende Zuordnung mit Zahlen 1, 2 und 3 sind wie im Datensatz,
      # die Voreinstellung ist 1 (selected) - also eine "normale Lage" 
      selectInput("Seasons",label="Seasons:", 
                  choices = list("Autumn", "Spring",
                                 "Summer", "Winter"), selected = "Autumn"
      ), 
      
      # eine Überschrift für die weiteren Ausstattungsmerkmale
      h5(strong("Ausstattung:"),align="left"),
      
      # die weiteren drei Ausstattungsmerkmale (kueche, bad, zh) mit
      # Boxen zum Anklicken
      # die Voreinstellung ist jeweils FALSE (value), das heißt, es ist als
      # Voreinstellung keine Box angeklickt
      checkboxInput(inputId="Holiday", label="Holiday", value = FALSE),
    ),
    
    # der Hauptbereich der Nutzeroberfläche für die Ausgabe der Ergebnisse
    mainPanel(
      
      # Ausgabe des Histogramms
      plotOutput(outputId = "Verteilung"),
      
      # Ausgabe der Prognose
      textOutput("Prognose"),
      
    )
  )
)

############


server <- function(input, output) {
  
  # Innerhalb dieser Funktion werden die Bilder für die Ausgabe
  # erzeugt und die Ergebnisse berechnet
  
  # Folgende Funktion berechnet die Prognose für die eingegeben Werte  
  prognose <- reactive({
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday")]
    
    # Ersetze die erste Zeile in X nun mit den neuen, eingegebenen Werten
    
    # zunächst die Werte für flaeche und bjahr 
    X[1,"Hour"] <- input$Hour 
    X[1,"Temperature..C."] <- input$Temperature
    # der angegebene Wert für lage muss zusätzlich noch in factor umgewandelt werden
    
    X[1,"Seasons"] <- as.factor(input$Seasons)
    
    
    # die Eingaben TRUE/FALSE für die Ausstattungsmerkmale kueche, bad und zh
    # werden jeweils in 0/1-Variablen umgewandelt (mit ifelse) und in
    # den Datentyp factor umgewandelt (mit as.factor);
    # die Werte werden in die erste Zeile von X eingetragen
    X[1,"Holiday"] <- as.factor(ifelse(input$Holiday == FALSE, "No Holiday", "Holiday"))

    # Berechne die Prognosen für X
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    prognosevektor <- predict(model,X)
    prog <- prognosevektor[1]
    
    # der Prognosewert wird noch auf 2 Stellen hinter dem Komma (digits=2) gerundet.
    prog <- round(prog,digits=2)
    
    # der errechnete Wert soll als Ergebnis der Funktion zurückgegeben werden
    prog
  })         
  
  # diese Funktion erzeugt das Histogramm und speichert es als Ausgabebild 
  # mit dem Namen output$Verteilung
  output$Verteilung <- renderPlot({
    
    # die errechnete Prognose aus der oben geschriebenen Funktion prognose()
    prog <- prognose()
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    # und die Daten der Zielvariable in y.
    # Berechne dann die Abweichungen zwischen den Prognosen und den realen Werten
    X <- Daten[,c("Hour","Temperature..C.","Seasons","Holiday")]
    y <- Daten[,"Rented.Bike.Count"]
    abweichungen <- y-predict(model,X)
    
    # Zeichne jetzt im Histogram die Prognose mit den Abweichungen;
    # dies visualisiert die bandbreite der Mieten für diese Wohnung 
    hist(prog+abweichungen, col = "blue", main = "Verteilung der Quadratmetermieten",xlim=c(0,15), breaks = 2)
    
  })
  
  # Definition einer Textausgabe mit dem namen output$Prognose 
  # In dieser Textausgabe soll der in der Funktion prognose() 
  # errechnete Prognosewert ausgegeben werden
  output$Prognose <- renderText({
    
    # der Wert der Prognose aus der Funktion prognose()
    prog <- prognose()
    
    # die Ausgabe ist eine Kombination (mit dem Befehl 'paste') von Text
    # und des errechneten Prognosewerts prog 
    Ausgabe <- paste("Durchschnittliche Miete: ", prog," Euro")
  })
  
}



# Aufruf der App-Funktionen
###############

shinyApp(ui = ui, server = server)

###############






