library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(car)
library(stargazer)

# only the 2014 data has questions about refugees
# gvrfgap: Government should be generous judging applications for refugee status
raw_data_2014 <- read_csv("./data/ESS7e02_2/ESS7e02_2.csv")

# imdfetn
# Allow many/few immigrants of different race/ethnic group from majority
variables <- c("imdfetn","gvrfgap")
df <- raw_data_2014[, variables]
print(sapply(df, function(x) sum(is.na(x))))
df <- subset(df, imdfetn >= 1 & imdfetn <= 4
               & gvrfgap >= 1 & gvrfgap <= 5)
correlation <- cor(df$imdfetn, df$gvrfgap)
print("Correlation for imdfetn is")
print(correlation)

# imsmetn
# Allow many/few immigrants of same race/ethnic group as majority
variables <- c("imsmetn","gvrfgap")
df <- raw_data_2014[, variables]
print(sapply(df, function(x) sum(is.na(x))))
df <- subset(df, imsmetn >= 1 & imsmetn <= 4
               & gvrfgap >= 1 & gvrfgap <= 5)
correlation <- cor(df$imsmetn, df$gvrfgap)
print("Correlation for imsmetn is")
print(correlation)

# impcntr
# Allow many/few immigrants from poorer countries outside Europe
variables <- c("impcntr","gvrfgap")
df <- raw_data_2014[, variables]
print(sapply(df, function(x) sum(is.na(x))))
df <- subset(df, impcntr >= 1 & impcntr <= 4
               & gvrfgap >= 1 & gvrfgap <= 5)
correlation <- cor(df$impcntr, df$gvrfgap)
print("Correlation for impcntr is")
print(correlation)

Correlation entre "Allow many/few immigrants of different race/ethnic group from majority" y "Government should be generous judging applications for refugee status"
0.3917902

Correlation entre "Allow many/few immigrants of same race/ethnic group as majority" y "Government should be generous judging applications for refugee status"
0.2715783

Correlation entre "Allow many/few immigrants from poorer countries outside Europe" y "Government should be generous judging applications for refugee status"
0.4140282