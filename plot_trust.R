library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)


path_2008 <- "ZA4819"
column_label_2008 <- "v230"

path_2009 <- "ZA4994"
column_label_2009 <- "v216"

path_2010 <- "ZA5449"
column_label_2010 <- "v302"

path_2011 <- "ZA5567"
column_label_2011 <- "qa10_8"

path_2012 <- "ZA5685"
column_label_2012 <- "qa11_8"

path_2013 <- "ZA5876"
column_label_2013 <- "qa10_8"

path_2014 <- "ZA5932"
column_label_2014 <- "qa8a_13"

path_2015 <- "ZA6643"
column_label_2015 <- "qa8a_10"

path_2016 <- "ZA6788"
column_label_2016 <- "qa8a_14"

path_2017 <- "ZA6928"
column_label_2017 <- "qa8a_14"

path_2018 <- "ZA6963"
column_label_2018 <- "qa8a_10"

# 1	Things are going in the right direction
# 2	Things are going in the wrong direction
# 3	Neither the one nor the other (SPONTANEOUS)
# 4	DK

get_trust_df <- function(data_path, column_label, year) {
  df <- read_dta(paste0("data/", data_path, ".dta"))
  
  print(column_label)
  
  trust_series <- df[[column_label]]
  

  trust_series <- as.factor(trust_series)

  levels(trust_series) <- c(levels(trust_series), 3)
  trust_series[is.na(trust_series)] <- 3
  
  trust_series <- na.omit(trust_series)
  
  print(trust_series)
  
  trust_df <- data.frame(Year = year, Category = trust_series)
  
  return(trust_df)
}

# value                label
# 1      Tend to trust
# 2      Tend not to trust
# 3 Don't know
# NA(d)                   DK

df_2008 <- get_trust_df(path_2008, column_label_2008, "2008") 
df_2009 <- get_trust_df(path_2009, column_label_2009, "2009") 
df_2010 <- get_trust_df(path_2010, column_label_2010, "2010") 
df_2011 <- get_trust_df(path_2011, column_label_2011, "2011") 
df_2012 <- get_trust_df(path_2012, column_label_2012, "2012") 
df_2013 <- get_trust_df(path_2013, column_label_2013, "2013") 
df_2014 <- get_trust_df(path_2014, column_label_2014, "2014") 
df_2015 <- get_trust_df(path_2015, column_label_2015, "2015") 
df_2016 <- get_trust_df(path_2016, column_label_2016, "2016") 
df_2017 <- get_trust_df(path_2017, column_label_2017, "2017") 
df_2018 <- get_trust_df(path_2018, column_label_2018, "2018") 


df <- rbind(df_2008, df_2009, df_2010, df_2011, df_2012,
            df_2013, df_2014, df_2015, df_2016, df_2017,
            df_2018)

result <- as.data.frame.matrix(table(df$Year, df$Category))

result

df <- data.frame(
  Year = 2008:2018,
  `1` = c(15244, 15781, 14646, 12252, 12177, 12035, 13922, 11995, 13272, 14720, 14985),
  `2` = c(11450, 11153, 12943, 16244, 17294, 17267, 14896, 15912, 15797, 14448, 14267),
  `3` = c(3436, 3304, 3191, 3163, 3260, 3109, 4844, 4926, 3827, 4025, 3878)
)

df_long <- df %>%
  pivot_longer(-Year, names_to = "Category", values_to = "Value")

df_long$Category[df_long$Category == "X1"] <- "Tend to trust"
df_long$Category[df_long$Category == "X2"] <- "Tend not to trust"
df_long$Category[df_long$Category == "X3"] <- "Don't know"

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
  scale_color_manual(values = c("Tend to trust" = "#3182bd", "Tend not to trust" = "#ef8a62", "Don't know" = "#bfbdbd")) +  # Specify the colors
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove the grid

print(p)

ggsave("plot_trust.svg", plot = p, dpi = 800, width = 10, height = 6)


