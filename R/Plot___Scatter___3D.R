# # Assuming df is your data frame
# # Split the row names into three separate columns
# df$X <- as.integer(sub("_.*", "", rownames(df)))
# df$Y <- as.integer(sub(".*_(\\d+)_.*", "\\1", rownames(df)))
# df$Z <- as.integer(sub(".*_(\\d+)$", "\\1", rownames(df)))
#
#
#
#
# Plot___Scatter___3D = function(Data.df, col.names, ){
#   #=============================================================================
#   # install.pacakge
#   #=============================================================================
#   # Check if the 'plotly' package is installed
#   if(!requireNamespace("plotly", quietly = TRUE)) {
#     # If not installed, you can either install it or display a message
#     message("The 'plotly' package is not installed")
#     options(repos = "https://cloud.r-project.org/")
#     install.packages("plotly")
#   }
#   require(plotly)
#
#
#
#
#
#   #=============================================================================
#   # Create a 3D scatter plot
#   #=============================================================================
#
#
#
#
#
#
# }
#
#
#
# # Specify the coordinates to highlight
# highlight_x <- Data.df[68,1]  # Replace X_highlight with the X-coordinate to highlight
# highlight_y <- Data.df[68,2]  # Replace Y_highlight with the Y-coordinate to highlight
# highlight_z <- Data.df[68,3] # Replace Z_highlight with the Z-coordinate to highlight
#
# # Create the 3D scatter plot
# scatter_plot <- plot_ly(data = Data.df, x = ~X, y = ~Y, z = ~Z, type = "scatter3d", mode = "markers") %>%
#   layout(scene = list(xaxis = list(title = "X Coordinate"),
#                       yaxis = list(title = "Y Coordinate"),
#                       zaxis = list(title = "Z Coordinate")),
#          title = "3D Scatter Plot of Coordinates")
#
# # Add a marker to highlight the specific point
# highlighted_point <- plot_ly(x = highlight_x, y = highlight_y, z = highlight_z, type = "scatter3d", mode = "markers",
#                              marker = list(size = 5, color = "red"))
#
# # Combine the scatter plot and the highlighted point
# final_plot <- subplot(scatter_plot, highlighted_point)
#
# # Display the final plot
# final_plot
#
#
#
#
#
#
#
#
#
#
