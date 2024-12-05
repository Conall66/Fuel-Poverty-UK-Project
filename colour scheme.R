library(ggplot2)

# Create the color scale for the 3x3 grid
color_matrix <- matrix(
  c("#f7f4f9", "#e7d4e8", "#d4b9da",
    "#d0d1e6", "#a6bddb", "#74a9cf",
    "#3690c0", "#0570b0", "#034e7b"),
  nrow = 3, ncol = 3, byrow = TRUE
)

# Map it to your data
ggplot(data, aes(x = FP, y = WMI, fill = interaction(FP, WMI))) +
  geom_tile() +
  scale_fill_manual(values = as.vector(color_matrix)) +
  theme_minimal()