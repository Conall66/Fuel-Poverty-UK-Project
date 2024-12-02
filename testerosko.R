shape_data <- reactive({
  # Read shapefile and transform to WGS84
  sf_data <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp") %>%
    st_transform(4326)  # Transform to WGS84
})

# In the observe block, we need to join the data before creating the map:
observe({
  req(fuel_poverty_data(), shape_data())
  
  # Get the data
  fp_data <- fuel_poverty_data()
  shapes <- shape_data()
  
  # Print column names for debugging
  print("Fuel poverty columns:")
  print(names(fp_data))
  print("Shapefile columns:")
  print(names(shapes))
  
  # Join the data
  # Note: Replace 'LAD22CD' and 'Area Codes' with your actual joining column names
  mapped_data <- shapes %>%
    left_join(fp_data, by = c("LAD22CD" = "Area Codes"))
  
  # Create color palette
  pal <- colorBin("YlOrRd", 
                  domain = if(input$view_type == "total") mapped_data$fuel_poor 
                  else mapped_data$proportion,
                  bins = 7)
  
  # Update map
  leafletProxy("map") %>%
    clearShapes() %>%
    addPolygons(
      data = mapped_data,
      fillColor = ~pal(if(input$view_type == "total") fuel_poor else proportion),
      fillOpacity = 0.7,
      weight = 1,
      color = "#666",
      highlight = highlightOptions(
        weight = 2,
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    )
})