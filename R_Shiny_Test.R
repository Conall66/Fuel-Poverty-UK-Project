
# RShiny Test Script

ui <- fluidPage(
  "Hello world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)