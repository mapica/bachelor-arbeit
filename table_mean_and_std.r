library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(car)
library(stargazer)

data_2012 <- read_csv("./data/ZA5685")
trust_2012 <- data_2012$qa11_8 # 1, 2
image_2012 <- data_2012$qa12 # 1 -> 5
direction_2012 <- data_2012$qa10a_2 # 1 -> 3

# first swap 2 by 3 in direction because 3 is neither nor and 2 is wrong direction
tmp_dir <- direction_2012
direction_2012[tmp_direction_2012 == 2] <- 3
direction_2012[tmp_direction_2012 == 3] <- 2 

trust_2012 <- trust_2012[trust_2012 >= 1 & trust_2012 <= 2]
image_2012 <- image_2012[image_2012 >= 1 & image_2012 <= 5]
direction_2012 <- direction_2012[direction_2012 >= 1 & direction_2012 <= 3]


data_2014 <- read_csv("./data/ZA5932")
trust_2014 <- data_2014$qa8a_13
image_2014 <- data_2014$d75
direction_2014 <- data_2014$d73a_2

# first swap 2 by 3 in direction because 3 is neither nor and 2 is wrong direction
tmp_dir <- direction_2014
direction_2014[tmp_direction_2014 == 2] <- 3
direction_2014[tmp_direction_2014 == 3] <- 2 

trust_2014 <- trust_2014[trust_2014 >= 1 & trust_2014 <= 2]
image_2014 <- image_2014[image_2014 >= 1 & image_2014 <= 5]
direction_2014 <- direction_2014[direction_2014 >= 1 & direction_2014 <= 3]




