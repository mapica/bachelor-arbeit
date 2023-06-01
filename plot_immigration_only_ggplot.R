# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(dplyr)

# Create the data frame
df <- data.frame(
  variable = c("EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration",
               "EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration",
               "EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration"),
  category = c("Total positive", "Total negative", "Total positive", "Total negative",
               "Total positive", "Total negative", "Total positive", "Total negative",
               "Total positive", "Total negative", "Total positive", "Total negative"),
  percentage = c(57.01404, 42.98596, 36.98738, 63.01262,
                 60.32656, 39.67344, 32.17286, 67.82714,
                 65.29526, 34.70474, 34.78632, 65.21368),
  year = c(2014, 2014, 2014, 2014,
           2015, 2015, 2015, 2015,
           2016, 2016, 2016, 2016)
)

# Convert year to a factor for correct ordering in the plot
df$year <- factor(df$year, levels = c(2014, 2015, 2016))

# Plot the data
ggplot(df, aes(x = variable, y = percentage, fill = category)) +
  geom_col(position = "stack", width = 0.5) +  # Make the bars less thick
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), hjust = -0.1) +
  scale_fill_manual(values = c("Total positive" = "green", "Total negative" = "yellow")) +  # Change colors
  theme_minimal() +  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),  # Transparent background
    axis.text.x = element_blank(),  # Erase x-axis text
    axis.text.y = element_text(face = "bold"),  # Make y-axis text bold
    axis.ticks = element_blank(),  # Erase axis ticks
    plot.background = element_rect(fill = "transparent", colour = NA),  # Transparent plot background
    legend.position = "bottom"
  ) +
  coord_flip() +  # Make bars horizontal
  facet_wrap(~year) +  # Create a separate bar plot for each year
  labs()  # Delete titles

# Plot the data
ggplot(df, aes(x = variable, y = percentage, fill = category)) +
  geom_col(position = "stack", width = 0.5) +  # Make the bars less thick
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = 5.5, size = 4.5,  # Place the text below bars and increase font size
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Total negative" = "yellow", "Total positive" = "green")) +  # Swap the order of the elements in the legend
  theme_minimal() +  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),  # Transparent background
    axis.text.x = element_blank(),  # Erase x-axis text
    axis.text.y = element_text(face = "bold"),  # Make y-axis text bold
    axis.ticks = element_blank(),  # Erase axis ticks
    plot.background = element_rect(fill = "transparent", colour = NA),  # Transparent plot background
    legend.position = "bottom",
    legend.title = element_blank(),  # Erase legend title,
    axis.title = element_blank()
  ) +
  coord_flip() +  # Make bars horizontal
  facet_wrap(~year) +  # Create a separate bar plot for each year
  labs(x = NULL, y = NULL)  # Delete x and y axis titles

# Convert variable to a factor for correct ordering in the plot
df$variable <- factor(df$variable, levels = c("Outside EU immigration", "EU immigration"))

# Plot the data
plot <- ggplot(df, aes(x = variable, y = percentage, fill = category)) +
  geom_col(position = "stack", width = 0.5) +  # Make the bars less thick
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = 5, size = 4.5,  # Place the text below bars and increase font size
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Total positive" = "#226ce3", "Total negative" = "#e32222"), guide = guide_legend(reverse=TRUE)) +  # Swap the order of the elements in the legend
  theme_minimal() +  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),  # Transparent background
    axis.text.x = element_blank(),  # Erase x-axis text
    axis.text.y = element_text(face = "bold", size = 10),  # Make y-axis text bold and increase font size
    strip.text = element_text(face = "bold", size = 12),  # Increase font size of the strip text
    axis.ticks = element_blank(),  # Erase axis ticks
    plot.background = element_rect(fill = "transparent", colour = NA),  # Transparent plot background
    legend.position = "bottom",
    legend.title = element_blank()  # Erase legend title
  ) +
  coord_flip() +  # Make bars horizontal
  facet_wrap(~year) +  # Create a separate bar plot for each year
  labs(x = NULL, y = NULL)  # Delete x and y axis titles





