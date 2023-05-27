#------------------------LOAD_LIBRARIES--------------------------------

# The 'here' library helps manage file paths and project directories.
library(here)

# The 'tidyverse' library is a collection of packages for data manipulation
# and visualisation. It includes several useful packages, such as 'dplyr',
# 'tidyr', 'readr', etc.
library(tidyverse)

# The 'ggplot2' library provides a system for creating visualisations.
library(ggplot2)

# The 'plotly' library allows you to create an interactive visualisations.
library(plotly)

# The 'htmlwidgets' library enables the plot to be saved as a html widget 
library(htmlwidgets)


#-------------------------IMPORT_DATA--------------------------------

# Specify the file path to the CSV data file in the 'data' directory
data_path <- here("ADV_ADDMN-National-Data.csv")

# Load the data from the CSV file into the 'full_data' variable
full_data <- read.csv(data_path)

# Display the first three rows of the data
head(full_data, n = 3)


#-------------------------CLEAN_THE_DATA--------------------------------

# Select specific columns from the full dataset
cleaned_data <- full_data[, c("year", "male_prev", "female_prev")]

# Remove rows with missing values from the cleaned dataset
cleaned_data <- na.omit(cleaned_data)

# Create a new column named 'Total' by summing up the 'male_prev' and 'female_prev' columns
cleaned_data$Total <- cleaned_data$male_prev + cleaned_data$female_prev


#-------------------CALCULATE_PROPORTIONALITY--------------------------

# Calculate the ratio of the difference to the average for each year
ratios <- (cleaned_data$male_prev - cleaned_data$female_prev) / ((cleaned_data$male_prev +
                                                                    cleaned_data$female_prev) / 2)


# Check if the ratios are approximately constant over time
is_proportional <- all(abs(ratios - mean(ratios)) < 0.05)

# Print the result
if (is_proportional) {
  cat("The difference in ASD diagnosis prevalence is proportional between males and females.\n")
} else {
  cat("The difference in ASD diagnosis prevalence is not proportional between males and females.\n")
}


#------------------------CREATE_STATIC_PLOT--------------------------------

# Create a static plot using ggplot with cleaned_data as the data source
static_plot <- ggplot(cleaned_data, aes(x = year, y = Total, group = 1)) +
  
  # Add lines for male_prev and female_prev with different colors
  geom_line(aes(y = male_prev, color = "Males"), linewidth = 0.8) +
  geom_line(aes(y = female_prev, color = "Females"), linewidth = 0.8) +
  
  # Add points for male_prev and female_prev with different colors
  geom_point(aes(y = male_prev, colour = "Males"), size = 1) +
  geom_point(aes(y = female_prev, colour = "Females"), size = 1) +
  
  # Customize color scale for the legend
  scale_color_manual(
    name = "Biological Sex",
    values = c("Females" = "red", "Males" = "blue"),
    guide = guide_legend(reverse = TRUE)
  ) +
  
  # Set titles, labels, and captions
  # include title, subtitle, caption for the purposes of saving a static image
  labs(
      title = "Prevalence of Autism Spectrum Disorder in 8-year-old Males and Females\nfrom 2002 to 2020 in #the US",
      subtitle = "Divergent Trend: Non-Proportional Prevalence Rates Revealed",
    x = "Calendar Year",
    y = "Prevalence Rate per 1,000 people",
      caption = "Source: ADDM (Autism and Developmental Disabilities Monitoring Network)"
  ) +
  
  # Customise theme settings
  theme(
    panel.background = element_rect(fill = "white", 
                                    color = "grey"),
    panel.border = element_rect(color = "grey", 
                                fill = NA),
    panel.grid.major = element_line(colour = "grey", 
                                    linewidth = 0.2),
    axis.text.x = element_text(angle = 0, hjust = 1, 
                               margin = margin(t = 10, 
                                               unit = "pt")),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.key = element_rect(fill = "white"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.subtitle = element_text(size = 8, 
                                 vjust = -1),
    plot.title = element_text(size = 10),
    plot.caption = element_text(hjust = 0, 
                                size = 8, 
                                vjust = -1)
  ) +
  
  # Set the limits and breaks for x and y axes
  scale_x_continuous(
    limits = c(2002, 2020),
    breaks = seq(2002, 2020, 
                 by = 2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 
                 by = 10),
    expand = c(0, 0)
  )


#------------------------ANIMATE_PLOT------------------------------------

# Convert the static plot to a plotly object
anni_plot <- ggplotly(static_plot) %>%
  
  # Add customisation to the plot
  layout(
    
    # Set the title of the plot
    title = list(
      text = paste0('Prevalence of Autism Spectrum Disorder in 8-year-old Males and Females\nfrom 2002 to 2020 in the US',
                    '<br>',
                    '<sup>',
                    'Divergent Trend: Non-Proportional Prevalence Rates Revealed',
                    '<br>',
                    '</sup>'),
      
      # The x parameter is used to adjust the horizontal position of the title to the left
      x = -4,  
      
      # Set the appropriate font size
      font = list(size = 14)  
    ),
    
    # Set the margins around the plot
    margin = list(l = 50, r = 0, b = 75, t = 75),
    
    # Add a text annotation to cite the data source and adjust accordingly
    annotations = list(
      x = 1, y = -0.3,
      text = "Source: ADDM (Autism and Developmental Disabilities Monitoring Network)",
      xref = 'paper', yref = 'paper',
      showarrow = F,
      xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
      font = list(size = 10)  
    )
  )

# The final ploty object is stored in the variable 'anni_plot' 
anni_plot

#------------------------SAVE_PLOT------------------------------------

# Save the animated graph as an HTML file which enables interaction with hover 
saveWidget(anni_plot, file = "plotly_graph.html")

# Save the static graph as a JPEG file
ggsave("VIS_STATIC_220243551.jpg", plot = static_plot, device = "jpeg", width = 10, height = 8, units = "in")



