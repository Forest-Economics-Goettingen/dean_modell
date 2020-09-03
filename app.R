
#Datensatz 3 Fälle

#Faelle

#Fuer die Shiny-App sollen drei verschiedene Faelle betrachtet werden. In allen Faellen gibt es Finanzierungsquellen (z.b. verschiedene Banken), die fuer eine bestimmte Finanzierungssumme einen Kredit-Zins haben (Spalte 1 und 2 der Matrizen) und Investitionen (z.B. verschiedene Fonds), die fuer eine bestimmte Investitionssumme jeweilig eine interne Verzinsung aufweisen. Es koennen beliebig die drei dargestellten Faele manipuliert werden, sodass unterschiedliche Treppendiagramme als Grundlage des Dean-Modells entstehen.

#Wenn etwas am Input verändert wird muss bei output$Ergebnis etwas verändert werden, da sich der Text nicht automatisch anpasst (Zeile 91 - 95)

A <- data.frame(Kreditzins= c(5,6,7.3,8.6,10.3),
           Finanzierungssumme = c(2500,10500,25500,35500,41500),
           InterneVerzinsung = c(11.5,9.6,8.5,7.9,3.6),
           Investitionssumme = c(6300,12200,16900,27900,30900))

B <- data.frame(Kreditzins= c(6,7,8,9,10.7, 9.5),
                Finanzierungssumme = c(2500,16000,25500,35500,41500, 45000),
                InterneVerzinsung = c(14.5,13,11.9,11,9.3, 11.7),
                Investitionssumme = c(6300,12200,16900,27900,30900, 37000))

C <- data.frame(Kreditzins= c(5,6,7.3,8.6,10.3),
                Finanzierungssumme = c(2500,10500,25500,35500,41500),
                InterneVerzinsung = c(13.5,8.5,8.2,6,4),
                Investitionssumme = c(6300,12200,16900,20000,27000))

#Abaenderung für Treppendiagramm


A_Treppe <- data.frame(Kreditzins = sort(rep(A[,1],2)),
                       Finanzierungssumme= c(0,sort(rep(A[,2],2))[-length(rep(A[,2],2))]),
                      InterneVerzinsung = sort(rep(A[,3],2), decreasing = TRUE),
                      Investitionssumme =c(0,sort(rep(A[,4],2))[-length(rep(A[,2],2))]))

B_Treppe <- data.frame(Kreditzins = sort(rep(B[,1],2)),
                       Finanzierungssumme= c(0,sort(rep(B[,2],2))[-length(rep(B[,2],2))]),
                       InterneVerzinsung = sort(rep(B[,3],2), decreasing = TRUE),
                       Investitionssumme =c(0,sort(rep(B[,4],2))[-length(rep(B[,2],2))]))

C_Treppe <- data.frame(Kreditzins = sort(rep(C[,1],2)),
                       Finanzierungssumme= c(0,sort(rep(C[,2],2))[-length(rep(C[,2],2))]),
                       InterneVerzinsung = sort(rep(C[,3],2), decreasing = TRUE),
                       Investitionssumme =c(0,sort(rep(C[,4],2))[-length(rep(C[,2],2))]))

library(shiny)

ui <- fluidPage(
  
  titlePanel("Dean Modell - 3 Fallbeispiele"),
  
  sidebarLayout(
    sidebarPanel(
     
      
      selectInput(inputId = "case", label = "Fall:",
                  choices = c("A",
                              "B",
                              "C")),
      
      helpText("Waehle einen der Bespielfaelle zur Anwendung des Dean Modells aus")
      
    ),
    mainPanel(
      h2("Treppendiagramm"),
      plotOutput("stairs"),
      
      h3(textOutput("Ergebnis"))
    )
  )
  
)

server <- function(input, output){
  
  
  
  output$stairs <- renderPlot({
    
    data <- switch(input$case, 
                   "A" = A_Treppe,
                   "B" = B_Treppe,
                   "C" = C_Treppe)
    
    plot(data[,1]~data[,2], type = "o",
         ylim = c(3,15), las = 1, col = "blue",
         ylab = "Zins", xlab = "Geldsumme [€]",
         pch = 16)
    lines(data[,3]~data[,4], type = "o", col = "red", pch = 16)
    legend("topright", c("Finanzierungsquelle","Investitionen"),
           lty = 1, pch = 16, col = c("blue","red"), cex = 1.3)
  } )
  
  output$Ergebnis <- renderText({
    switch(input$case,
           "A" = "Endogener Grenzzinssatz ist 7.9 % und Investitions-/Finanzierungsvolumen ist 25500 €",
           "B" = "Endogener Grenzzinssatz ist 9.3 % und Investitions-/Finanzierungsvolumen ist 35500 €",
           "C" = "Endogener Grenzzinssatz ist 7.3 % und Investitions-/Finanzierungsvolumen ist 16900 €")
    
  })
  
}

shinyApp(ui = ui, server = server)