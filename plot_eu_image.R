# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)

path_2008 <- "ZA4819"
column_label_2008 <- "v233"

path_2009 <- "ZA4994"
column_label_2009 <- "v220"

path_2010 <- "ZA5449"
column_label_2010 <- "v305"

path_2011 <- "ZA5567"
column_label_2011 <- "qa11"

path_2012 <- "ZA5685"
column_label_2012 <- "qa12"

path_2013 <- "ZA5876"
column_label_2013 <- "qa11"

path_2014 <- "ZA5933"
column_label_2014 <- "d75"

path_2015 <- "ZA6644"
column_label_2015 <- "d78"

path_2016 <- "ZA6791"
column_label_2016 <- "d78"

path_2017 <- "ZA6939"
column_label_2017 <- "d78"

path_2018 <- "ZA7556"
column_label_2018 <- "d78"


get_image_df <- function(data_path, column_label, year) {
  df <- read_dta(paste0("data/", data_path, ".dta"))
  
  print(column_label)
  
  image_series <- df[[column_label]]
  
  print("How many NA values?")
  print(sum(is.na(image_series)))
  
  image_series <- as.factor(image_series)
  print(image_series)
  print(levels(image_series))
  levels(image_series) <- c(levels(image_series), 6)
  image_series[is.na(image_series)] <- 6

  image_series <- na.omit(image_series)
  
  print(image_series)
  
  image_df <- data.frame(Year = year, Category = image_series)
  
  return(image_df)
}

# 1   Very positive
# 2 Fairly positive
# 3         Neutral
# 4 Fairly negative
# 5   Very negative
# 6   Don't know

summary(df_2008)

df_2008 <- get_image_df(path_2008, column_label_2008, "2008") 
df_2009 <- get_image_df(path_2009, column_label_2009, "2009") 
df_2010 <- get_image_df(path_2010, column_label_2010, "2010") 
df_2011 <- get_image_df(path_2011, column_label_2011, "2011") 
df_2012 <- get_image_df(path_2012, column_label_2012, "2012") 
df_2013 <- get_image_df(path_2013, column_label_2013, "2013") 
df_2014 <- get_image_df(path_2014, column_label_2014, "2014") 
df_2015 <- get_image_df(path_2015, column_label_2015, "2015") 
df_2016 <- get_image_df(path_2016, column_label_2016, "2016") 
df_2017 <- get_image_df(path_2017, column_label_2017, "2017") 
df_2018 <- get_image_df(path_2018, column_label_2018, "2018") 

result
df <- rbind(df_2008, df_2009, df_2010, df_2011, df_2012,
            df_2013, df_2014, df_2015, df_2016, df_2017,
            df_2018)

result <- as.data.frame.matrix(table(df$Year, df$Category))

result

df <- result

names(df) <- c("Very positive", "Fairly positive", "Neutral", "Fairly Negative", "Very negative", "Don't know")

# Convert data from wide to long format
df_long <- df %>% 
  rownames_to_column("Year") %>%  # Convert row names to a column
  pivot_longer(-Year, names_to = "Category", values_to = "Value")

# Convert the Year column to numeric (it's character after rownames_to_column)
df_long$Year <- as.numeric(df_long$Year)


# Calculate the total per year
df_total <- df_long %>%
  group_by(Year) %>%
  summarise(Total = sum(Value))

# Join the total to the original data
df_long <- left_join(df_long, df_total, by = "Year")

# Calculate the percentage
df_long <- df_long %>%
  mutate(Percentage = (Value / Total) * 100)

df_long


# Plot the data
p <- ggplot(df_long, aes(x = Year, y = Percentage, color = Category)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  scale_x_continuous(breaks = 2008:2018) + # Add a tick for each year
  scale_color_manual(values = c("Very positive" = "#0571b0", "Fairly positive" = "#92c5de", "Neutral" = "#d49dd4", "Fairly Negative" = "#ef8a62", "Very negative" = "#ca0020", "Don't know" = "#bfbdbd")) +  # Specify the colors
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove the grid

print(p)

ggsave("plot_eu_image.svg", plot = p, dpi = 800, width = 10, height = 6)

