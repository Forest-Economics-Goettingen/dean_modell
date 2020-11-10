


library(shiny)
#install.packages("shinyMatrix")
library(shinyMatrix)
library(dplyr)



ui <- fluidPage(
  
  titlePanel("Dean Model / Dean-Modell"),
  
  sidebarLayout(
    sidebarPanel(
      
      h2("Change values for visualization of different sitiuations of the Dean Model"),
      tags$i(h2("Veraendere die Werte, um verschiedene Situationen zu visualisieren in denen das Dean Modell zur Anwendung kommt.")),
      
      h3("Hint: After changing one or more values click with the cursor outside the light grey area to update the model."),
      
      tags$i(h3("Tipp: Nachdem einer oder mehrere Werte veraendert wurden, mit dem Mauszeiger ausserhalb des hellgarauen Bereiches klicken, um das Modell gemaess der geaenderten Eingangsgroessen zu aktualisieren.")),
      
      h2("Financing / Finanzierung"),
      
      matrixInput("bank", value = matrix(c(6,5,7.3,8.6,10.3,10500,2500,25500,35500,41500), nrow = 5, byrow = F,
                                            dimnames = list(c("Bank A","Bank B","Bank C","Bank D","Bank E"),
                                                            c("base rate (Kreditzins)","finance lease (Finanzierungssumme)"))),
                  rows = list(names = T),
                  cols = list(names = T)),
      
      h2("Investment"),
      
      matrixInput("fond", value = matrix(c(13.5,4,8.2,6,8.5,6300,27000,16900,20000,12200), nrow = 5, byrow = F,
                                            dimnames = list(c("Fond A","Fond B","Fond C","Fond D","Fond E"),
                                                            c("interest rate (Verzinsung)","amount invested (Investitionssumme)"))),
                  rows = list(names = T),
                  cols = list(names = T),),
      
      h2("In case you want to zoom in or out to make e. g. the the intersection point better readable, you can adjust the axes."),
      
      tags$i(h2("Falls gewuenscht, kann durch eine Veraenderung der Achsengrenzen z. B. der Schnittpunkt besser ablesbar gemacht werden.")),
      
      numericInput("xmin", "Xmin", 0),
      numericInput("xmax", "Xmax", 45000),
      numericInput("ymin", "Ymin", 3),
      numericInput("ymax", "Ymax", 15),
      
    ),
    mainPanel(
      
      plotOutput("stairs"),

      
      h3("The intersection point of the two graphs marks the endogenous interest rate. Endogenous means that the value depends on the model inputs which are in this case the financing and the investment. All investment objects to the left side of the the intersection point have a positive capital value und all investment objects to the right side have a negative capital value. Thus, the intersection point additionally marks the highest finance lease to utilise without loosing capital."),
      
      tags$i(h3("Der Schnittpunkt markiert den sogenannten endogenen Grenzzinssatz . Alle im optimalen Programm enthaltenen Investitionsobjekte weisen bei diesem Kalkulationszinssatz einen positiven Kapitalwert, alle nicht im Programm enthaltenen einen negativen Kapitalwert auf. Weiterhin kann durch den Schnittpunkt beider Funktionen das Investitions- und Finanzierungsvolumen bestimmt werden."))
    )
  )
  
)

server <- function(input, output){
  
  
  output$stairs <- renderPlot({
    
    #matrix input
    
    data_bank <- data.frame(Zins = as.numeric(input$bank[,1]), Summe = as.numeric(input$bank[,2]))
    data_bank <- arrange(data_bank, Summe) 
    data_bank <- data.frame(Zins = c(rep(data_bank[1,1],2),rep(data_bank[2,1],2),rep(data_bank[3,1],2),
                                     rep(data_bank[4,1],2),rep(data_bank[5,1],2)),
                            Summe = c(0, rep(data_bank[1,2],2),rep(data_bank[2,2],2),rep(data_bank[3,2],2),
                                      rep(data_bank[4,2],2), data_bank[5,2]))
  
    
    data_fond <- data.frame(Zins = as.numeric(input$fond[,1]), Summe = as.numeric(input$fond[,2]))
    data_fond <- arrange(data_fond, Summe) 
    data_fond <- data.frame(Zins = c(rep(data_fond[1,1],2),rep(data_fond[2,1],2),rep(data_fond[3,1],2),
                                     rep(data_fond[4,1],2),rep(data_fond[5,1],2)),
                            Summe = c(0, rep(data_fond[1,2],2),rep(data_fond[2,2],2),rep(data_fond[3,2],2),
                                      rep(data_fond[4,2],2), data_fond[5,2]))

   
    par(mar = c(6,6,6,6))
    
    plot(data_bank$Zins~data_bank$Summe, type = "o",
         ylim = c(input$ymin,input$ymax), 
         xlim = c(input$xmin, input$xmax),
         las = 1, col = "blue",
         ylab = "Zins / Interest rate [%]", xlab = "Geldsumme / Sum of money [€]",
         pch = 16, 
         cex = 2, cex.lab = 2, cex.axis =2)
    
    lines(data_fond$Zins~data_fond$Summe, type = "o", col = "red",
          pch = 16, cex = 2)
    
    legend("topright", c("Finanzierungsquelle / Funding source","Investitionen / Investment"),
           lty = 1, pch = 16, col = c("blue","red"), cex = 2)
  } )
  
}

shinyApp(ui = ui, server = server)