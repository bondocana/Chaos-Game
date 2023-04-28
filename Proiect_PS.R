library(shiny)
library(shinythemes)

# Functie pentru generarea jocului
generate_chaos_game <- function(n, x, y) 
{
  puncte <- matrix(ncol = 2, nrow = n)
  puncte[1, ] <- c(x, y)
  for (i in 2:n) 
  {
    index <- sample(1:3, 1)
    puncte[i, ] <- (puncte[i - 1, ] + varfuri[index, ]) / 2
  }
  return(puncte)
}

# Definim varfurile triunghiului echilateral
varfuri <- matrix(c(0, 0, 1, 0, 0.5, sqrt(3) / 2), ncol = 2, byrow = TRUE)

# UI pentru aplicatia Shiny 
ui <- fluidPage(
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: pink}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: pink}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: pink}")),
  
  titlePanel("Chaos Game"),
  
  helpText("Aplicatia dezvoltata in raportul nostru, in care se disting urmatoarele functionalitati : "),
  helpText(" - Se pot alege coordonatele punctului de start "),
  helpText(" - Se poate selecta numarul de puncte dorite "),
  helpText(" Obs : Odata cu cresterea numarului de puncte, se ilustreaza mai bine aspectul fractalului "),
  
  sidebarLayout(
    
    # sidebarPanel-ul va contine sliderele pentru selectarea optiunilor, reprezentand partea interactiva a aplicatiei
    sidebarPanel(
      
      sliderInput("n", "Numarul de puncte : ", min = 10, max = 7500, value = 1000),
      sliderInput("x", "Abscisa punctului de start : ", min = -1, max = 1, value = 0),
      sliderInput("y", "Ordonata punctului de start : ", min = -1, max = 1, value = 0),
      
      helpText("Sursa date:"),
      
      tags$a(href="https://en.wikipedia.org/wiki/Chaos_game", "Despre Joc"),
      helpText(""),
      tags$a(href="https://mathworld.wolfram.com/ChaosGame.html", "Mai multe despre Joc")
      
      
    ),
    # mainPanel-ul va contine reprezentarea grafica a triunghiului lui Sierpinski in functie de preferintele utilizatorului
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server pentru aplicatia Shiny
server <- function(input, output) 
{
  output$plot <- renderPlot(
    {
      #incarcam punctele si afisam interfata grafica
      puncte <- generate_chaos_game(input$n, input$x, input$y)
      plot(puncte, type = "p", col = "pink", pch = 20, xlim = c(-0.1, 1.1), ylim = c(-0.1, sqrt(3) / 2 + 0.1))
      
      # cream laturile triunghiului
      segments(varfuri[1, 1], varfuri[1, 2], varfuri[2, 1], varfuri[2, 2], col = "purple")
      segments(varfuri[2, 1], varfuri[2, 2], varfuri[3, 1], varfuri[3, 2], col = "purple")
      segments(varfuri[3, 1], varfuri[3, 2], varfuri[1, 1], varfuri[1, 2], col = "purple")
    }
  )
}

# rulam aplicatia
shinyApp(ui, server)