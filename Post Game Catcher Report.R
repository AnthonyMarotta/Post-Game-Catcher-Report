library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

# Set working directory for Trackman CSV
setwd("~/Downloads/CCBLCSV")

# Load the dataset
df <- read.csv("UConnVsArmy.csv")

# Define catcher details
name <- "Garbowski, Matt"
date <- "Fall Game"
opp <- "Army"
report_title <- paste0(name, " - Catcher Report")  # Add "Catcher Report" to the title
last_name <- str_trim(str_extract(name, "^[^,]+"))
df_who <- filter(df, Catcher == name)

# Define the plate and strike zone dimensions
d <- 2.92 / 12  # diameter of baseball
plate_x_dim <- c(-9.95/12, 9.95/12, 9.95/12, -9.95/12, -9.95/12)
plate_y_dim <- c(1.622, 1.622, 3.37, 3.37, 1.622)
plate_dim_df <- data.frame(x = plate_x_dim, y = plate_y_dim)

plate_edge_x <- c(plate_x_dim[1] - 2.94/12, plate_x_dim[2] + 2.94/12, 
                  plate_x_dim[3] + 2.94/12, plate_x_dim[4] - 2.94/12, 
                  plate_x_dim[5] - 2.94/12)  # accounts for radius of baseball
plate_edge_y <- c(1.622 - 2.94/12, 1.622 - 2.94/12, 3.37 + 2.94/12,
                  3.37 + 2.94/12, 1.622 - 2.94/12)
plate_edges <- data.frame(x = plate_edge_x, y = plate_edge_y)

# Define the plate itself
plate <- data.frame(
  x = c(0, 8.5/12, 8.5/12, -8.5/12, -8.5/12),
  y = c(0, -sqrt((12^2) - (8.5^2)) / 12 / 3, 
        0 - (sqrt((12^2) - (8.5^2)) + 8.5) / 12 / 2.5, 
        0 - (sqrt((12^2) - (8.5^2)) + 8.5) / 12 / 2.5, 
        -sqrt((12^2) - (8.5^2)) / 12 / 3)
)

# Filter for called pitches and classify Strikes Stolen and Strikes Lost
calls <- df_who %>% 
  filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% 
  mutate(
    InZone = ifelse(abs(PlateLocSide) <= 0.83 & PlateLocHeight > 1.479 & PlateLocHeight <= 3.5, 1, 0),
    StrikesStolen = ifelse(PitchCall == "StrikeCalled" & InZone == 0, 1, 0),
    StrikesLost = ifelse(PitchCall == "BallCalled" & InZone == 1, 1, 0),
    Actual = ifelse(InZone == 1, "STRIKE", "BALL")
  )

# Separate data for Strikes Stolen and Strikes Lost
strikes_stolen_data <- calls %>% filter(StrikesStolen == 1)
strikes_lost_data <- calls %>% filter(StrikesLost == 1)

# Function to create a plot for each category, with smaller size and margins
strike_plot <- function(data, title) {
  ggplot() +
    geom_polygon(data = plate, aes(x = x, y = y), fill = NA, color = "black") +  # Add plate outline
    geom_path(data = plate_edges, aes(x = x, y = y), linetype = "dashed") +  # Add dashed edges for strike zone
    geom_path(data = plate_dim_df, aes(x = x, y = y), color = "black") +  # Add rulebook strike zone
    geom_point(data = data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), size = 2) +
    ggtitle(title) +  # Add the title for each plot
    theme_minimal(base_size = 10) +  # Smaller font size
    coord_equal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(2, 2, 2, 2),  # Smaller margins
      legend.title = element_blank(),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)  # Center the plot title
    ) +
    scale_color_manual(values = c("red", "blue", "green"))
}

# Generate the Strikes Stolen and Strikes Lost plots
strikes_stolen_plot <- strike_plot(strikes_stolen_data, "Strikes Stolen")
strikes_lost_plot <- strike_plot(strikes_lost_data, "Strikes Lost")

# Combine the plots side by side
combined_plot <- ggarrange(
  strikes_stolen_plot, strikes_lost_plot, ncol = 2, nrow = 1,
  common.legend = TRUE, legend = "bottom"
)

# Annotate with header and footer information in smaller font
ann_fig <- annotate_figure(
  combined_plot,
  top = text_grob(paste0(date, " - ", opp, " - ", report_title), size = 10, face = "bold"),
  bottom = text_grob(
    paste0("Strikes Stolen: ", sum(strikes_stolen_data$StrikesStolen),
           ", Strikes Lost: ", sum(strikes_lost_data$StrikesLost)),
    size = 10
  )
)

# Custom table styling to change text color
custom_table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(
      hjust = 0.5, 
      x = 0.5, 
      col = "black"  # Text color for table body
    ),  
    bg_params = list(fill = c("white", "#f1f1f1")),  # Alternating row colors
    fontface = "plain", 
    padding = unit(c(5, 5), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontface = "bold", 
      col = "red"  # Header text color
    ), 
    bg_params = list(fill = "navyblue"),  # Header background color
    padding = unit(c(5, 5), "mm")
  ),
  rowhead = list(fg_params = list(fontface = "bold"))
)

# Prepare the table data for strikes table
table_data <- calls %>% 
  filter(StrikesStolen == 1 | StrikesLost == 1) %>% 
  select(AutoPitchType, Pitcher, Catcher, Batter, BatterSide, PitchCall, Actual)

# Convert the strikes table data to a tableGrob with the custom theme
table_grob <- tableGrob(
  table_data,
  rows = NULL,  # Remove row names
  theme = custom_table_theme  # Apply custom theme
)

# Prepare the second table with Pitcher, Catcher, Throw Speed, Pop Time, and Notes
throw_data <- df_who %>% 
  filter(!is.na(ThrowSpeed) & !is.na(PopTime)) %>% 
  select(Pitcher, Catcher, ThrowSpeed, PopTime, OutsOnPlay) %>% 
  mutate(
    Result = ifelse(OutsOnPlay == 1, "Out", "Safe")
  ) %>% 
  select(Pitcher, Catcher, ThrowSpeed, PopTime, Result)

# If the throw data is empty, create a placeholder
if (nrow(throw_data) == 0) {
  throw_data <- data.frame(Message = "No Data Available")
  throw_table_grob <- tableGrob(throw_data, rows = NULL, theme = custom_table_theme)
} else {
  throw_table_grob <- tableGrob(
    throw_data,
    rows = NULL,  # Remove row names
    theme = custom_table_theme  # Apply custom theme
  )
}

# Save to PDF with larger page size and more space for all elements
dir.create(paste0("~/Downloads/CCBLRCodes", date, "_", opp), recursive = TRUE)
setwd(paste0("~/Downloads/CCBLRCodes", date, "_", opp))
pdf(paste0(date, "_", name, "_Catcher_Report_Strikes_Report.pdf"), height = 12, width = 9)

# Create blank space as a spacer
spacer <- textGrob(" ", gp = gpar(fontsize = 1))  # Invisible spacer

# Adjust the layout for better separation between components
grid.arrange(
  ann_fig,  # The annotated figure (combined plot)
  spacer,   # Spacer
  table_grob,  # The strikes table
  spacer,   # Spacer
  throw_table_grob,  # The throw speed and pop time table
  ncol = 1,
  heights = c(4, 0.5, 2, 0.5, 2)  # Adjusted space for each section
)

# Close the PDF device
dev.off()
