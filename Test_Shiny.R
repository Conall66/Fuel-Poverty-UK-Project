# Test Shiny

library(shiny)

ui <- fluidPage(
  skin = "black",
  dashboardHeader("Susceptibility to Winter Health Conditions"),
  dashboardSidebar(
    sliderInput("year", "Year:", 2012, 2030, 1),
    width = "10%",
    div(
      style = "font-family: 'Open Sans', sans-serif; font-style: italic; font-weight: bold; display: flex; justify-content: center; align-items: center; height: 100%; padding-top: 50px; gap: 10px;"
    )
  ),
  dashboardBody()
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)