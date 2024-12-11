
#Thermometer display

library(shiny)

# Define the path to your images folder
image_folder <- "icons"  # Replace with your folder's path

# Add a custom resource path to serve files from this folder
addResourcePath("icons", image_folder)

ui <- fluidPage(
  # Check if the image exists
  uiOutput("image_display")
)

server <- function(input, output, session) {
  # Path to the image
  image_path <- "icons/Thermometer.png"
  
  # Render the image if it exists
  output$image_display <- renderUI({
    if (file.exists(image_path)) {
      img(src = image_path, width = "30px", height = "30px")
    } else {
      p("Image not found")
    }
  })
}

shinyApp(ui, server)