# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(dplyr)

path_2014 <- "data/ZA5932.dta"
path_2015 <- "data/ZA6643.dta"
path_2016 <- "data/ZA6788.dta"

column_labels_2014 <- c("qa11_1", "qa11_2")
column_labels_2015 <- c("qb4_1", "qb4_2")
column_labels_2016 <- c("qb4_1", "qb4_2")

variable_names <- c("EU", "outside EU")

get_percentage <- function(column){
  total <- length(column)
  percent_1_2 <- sum(column %in% 1:2) / total * 100
  percent_3_4 <- sum(column %in% 3:4) / total * 100
  return(c(percent_1_2, percent_3_4))
}

# 2014
df_2014 <- read_dta(path_2014)
df_2014 <- df_2014[!is.na(df_2014$qa11_1) & !is.na(df_2014$qa11_2), ]
df_2014$qa11_1 <- as.numeric(df_2014$qa11_1)
df_2014$qa11_2 <- as.numeric(df_2014$qa11_2)
percentages_2014 <- data.frame(
  variable = c("EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration"),
  category = rep(c("Total positive", "Total negative"), 2),
  percentage = c(get_percentage(df_2014$qa11_1), get_percentage(df_2014$qa11_2))
)
print(percentages_2014)

df_2015$qb4_1

# 2015
df_2015 <- read_dta(path_2015)
df_2015 <- df_2015[!is.na(df_2015$qb4_1) & !is.na(df_2015$qb4_2), ]
df_2015$qb4_1 <- as.numeric(df_2015$qb4_1)
df_2015$qb4_2 <- as.numeric(df_2015$qb4_2)
percentages_2015 <- data.frame(
  variable = c("EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration"),
  category = rep(c("Total positive", "Total negative"), 2),
  percentage = c(get_percentage(df_2015$qb4_1), get_percentage(df_2015$qb4_2))
)
print(percentages_2015)

# 2016
df_2016 <- read_dta(path_2016)
df_2016 <- df_2016[!is.na(df_2016$qb4_1) & !is.na(df_2016$qb4_2), ]
df_2016$qb4_1 <- as.numeric(df_2016$qb4_1)
df_2016$qb4_2 <- as.numeric(df_2016$qb4_2)
percentages_2016 <- data.frame(
  variable = c("EU immigration", "EU immigration", "Outside EU immigration", "Outside EU immigration"),
  category = rep(c("Total positive", "Total negative"), 2),
  percentage = c(get_percentage(df_2016$qb4_1), get_percentage(df_2016$qb4_2))
)
print(percentages_2016)

# bind the three datasets
percentages_2014$year <- 2014
percentages_2015$year <- 2015
percentages_2016$year <- 2016

all_percentages <- rbind(percentages_2014, percentages_2015, percentages_2016)

# Convert year to a factor for correct ordering in the plot
all_percentages$year <- factor(all_percentages$year, levels = c(2014, 2015, 2016))

all_percentages

ggplot(percentages, aes(x = variable, y = percentage, fill = category)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(x = "Variable", y = "Percentage", fill = "Category",
       title = "Percentage of values in specified ranges for each variable")

ggplot(all_percentages, aes(x = factor(variable, levels = c("qa11_2", "qa11_1")), y = percentage, fill = category)) +
  geom_col(position = "stack", width = 0.5) +  # Make the bars less thick
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), hjust = -0.1) +
  scale_fill_manual(values = c("Total positive" = "blue", "Total negative" = "red")) +  # Change colors
  theme_minimal() +  # Use a minimal theme
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),  # Transparent background
    # axis.text = element_blank(),  # Erase axis text
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),  # Make y-axis text bold
    axis.ticks = element_blank(),  # Erase axis ticks
    plot.background = element_rect(fill = "transparent", colour = NA),  # Transparent plot background
    legend.position = "bottom",
    title = element_blank(),
    axis.title = element_blank()
  ) +
  coord_flip() +  # Make bars horizontal
  facet_wrap(~year) +
  labs(y = "Variable", x = "Percentage", fill = "",
       title = "Percentage of values in specified ranges for each variable")

# We manually create the data frame
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

# Convert variable to a factor for correct ordering in the plot
df$variable <- factor(df$variable, levels = c("Outside EU immigration", "EU immigration"))

# Plot the data
plot <- ggplot(df, aes(x = variable, y = percentage, fill = category)) +
  geom_col(position = "stack", width = 0.5) +  # Make the bars less thick
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = 7, size = 4.5,  # Place the text below bars and increase font size
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

print(plot)

ggsave("plot_immigration_segmentated.svg", plot = plot, dpi = 800, width = 10)



