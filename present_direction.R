# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)

# data <- read_dta("data/ZA4994.dta")
# series <- data[[column_label_2009]]
# print(series)
# summary(data$v205)
# length(data$v205)
# data$v205
# data[["v205"]]

path_2008 <- "ZA4819"
column_label_2008 <- "v222"

path_2009 <- "ZA4994"
column_label_2009 <- "v202"

path_2010 <- "ZA5449"
column_label_2010 <- "v286"

path_2011 <- "ZA5567"
column_label_2011 <- "qa9a_2"

path_2012 <- "ZA5685"
column_label_2012 <- "qa10a_2"

path_2013 <- "ZA5877"
column_label_2013 <- "d73_2"

path_2014 <- "ZA5932"
column_label_2014 <- "d73a_2"

path_2015 <- "ZA6643"
column_label_2015 <- "d73a_2"

path_2016 <- "ZA6788"
column_label_2016 <- "d73a_2"

path_2017 <- "ZA6928"
column_label_2017 <- "d73a_2"

path_2018 <- "ZA6963"
column_label_2018 <- "d73a_2"

# 1	Things are going in the right direction
# 2	Things are going in the wrong direction
# 3	Neither the one nor the other (SPONTANEOUS)
# 4	DK

get_direction_df <- function(data_path, column_label, year) {
  df <- read_dta(paste0("data/", data_path, ".dta"))

  print(column_label)
  
  direction_series <- df[[column_label]]
  
  direction_series[is.na(direction_series)] <- 4

  direction_series <- na.omit(direction_series)

  print(direction_series)
  
  direction_df <- data.frame(Year = year, Category = direction_series)

  return(direction_df)
}

# value                label
# 1      Right direction
# 2      Wrong direction
# 3      Neither nor (SPONT.)
# 4      DK

df_2008 <- get_direction_df(path_2008, column_label_2008, "2008") 
df_2009 <- get_direction_df(path_2009, column_label_2009, "2009") 
df_2010 <- get_direction_df(path_2010, column_label_2010, "2010") 
df_2011 <- get_direction_df(path_2011, column_label_2011, "2011") 
df_2012 <- get_direction_df(path_2012, column_label_2012, "2012") 
df_2013 <- get_direction_df(path_2013, column_label_2013, "2013") 
df_2014 <- get_direction_df(path_2014, column_label_2014, "2014") 
df_2015 <- get_direction_df(path_2015, column_label_2015, "2015") 
df_2016 <- get_direction_df(path_2016, column_label_2016, "2016") 
df_2017 <- get_direction_df(path_2017, column_label_2017, "2017") 
df_2018 <- get_direction_df(path_2018, column_label_2018, "2018") 


df <- rbind(df_2008, df_2009, df_2010, df_2011, df_2012,
      df_2013, df_2014, df_2015, df_2016, df_2017,
      df_2018)

result <- as.data.frame.matrix(table(df$Year, df$Category))

print(result)

# manually created using `result`
df <- data.frame(
  Year = 2008:2018,
  `1` = c(10948, 12615, 10583, 7082, 8318, 6932, 10487, 9064, 9792, 11923, 11979),
  `2` = c(9325, 7590, 10719, 15876, 14877, 11636, 10674, 12117, 15968, 13186, 12763),
  `3` = c(5629, 6159, 5933, 5462, 6104, 6132, 7742, 7508, 3124, 3874, 3974),
  `4` = c(4228, 3874, 3545, 3239, 3432, 3219, 4759, 4144, 4012, 4210, 4414)
)

df_long <- df %>%
  pivot_longer(-Year, names_to = "Category", values_to = "Value")

df_long$Category[df_long$Category == "X1"] <- "Right direction"
df_long$Category[df_long$Category == "X2"] <- "Wrong direction"
df_long$Category[df_long$Category == "X3"] <- "Neither nor"
df_long$Category[df_long$Category == "X4"] <- "Don't know"

df_long

# Calculate the total per year
df_total <- df_long %>%
  group_by(Year) %>%
  summarise(Total = sum(Value))

# Join the total to the original data
df_long <- left_join(df_long, df_total, by = "Year")

# Calculate the percentage
df_long <- df_long %>%
  mutate(Percentage = (Value / Total) * 100)


# Plot the data
p <- ggplot(df_long, aes(x = Year, y = Percentage, color = Category)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  scale_x_continuous(breaks = 2008:2018) + # Add a tick for each year
  scale_color_manual(values = c("Right direction" = "#3182bd", "Wrong direction" = "#ef8a62", "Neither nor" = "#7fbf7b", "Don't know" = "#bfbdbd")) +  # Specify the colors
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove the grid

print(p)

ggsave("plot_direction.svg", plot = p, dpi = 800, width = 10, height = 6)
