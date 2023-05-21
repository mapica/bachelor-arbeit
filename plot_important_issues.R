# Copyright (C) 2023 Macarena Picazo Mora
# Examining the effect of the Refugee Crisis. The relationship between attitudes toward Immigration and public Euroscepticism in EU member states

library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)

path_2012 <- "data/ZA5685_v2-0-0.dta"
path_2014 <- "data/ZA5932_v3-0-0.dta"
path_2016 <- "data/ZA6788_v2-0-0.dta"

column_labels_2012 <- c("qa5a_1", "qa5a_2", "qa5a_3" ,"qa5a_4", "qa5a_5",
                        "qa5a_6", "qa5a_7", "qa5a_8" ,"qa5a_9", "qa5a_10",
                        "qa5a_11", "qa5a_12", "qa5a_13" ,"qa5a_14", "qa5a_15", "qa5a_16")

column_labels_2014_and_2016 <- c("qa3a_1", "qa3a_2", "qa3a_3" ,"qa3a_4", "qa3a_5",
                                "qa3a_6", "qa3a_7", "qa3a_8" ,"qa3a_9", "qa3a_10",
                                "qa3a_11", "qa3a_12", "qa3a_13",
                                "qa3a_14", "qa3a_15", "qa3a_16")

variable_names <- c("Crime", "Economic situation", "Rising prices / Inflation",          
                    "Taxation", "Unemployment", "Terrorism",
                    "Housing", "Government debt", "Immigration",       
                    "Health and social security", "The education system", "Pensions",            
                    "The environment, climate and energy issues", "Other",
                    "None", "Don't know")

issues_percentage <- function(data_path, column_labels, column_names, year) {
  df <- read_dta(data_path)
  
  missing_rows <- sum(!complete.cases(df))
  
  important_issues <- na.omit(df[column_labels])
  
  colnames(important_issues) <- column_names
  
  col_percentages <- round(colSums(important_issues) / nrow(important_issues) * 100)
  
  col_percentages_df <- data.frame(Variable = column_names,
                                   Percentage = col_percentages,
                                   Year = year)
  
  col_percentages_df <- col_percentages_df[order(-col_percentages_df$Percentage), ]
  
  col_percentages_df$Variable <- with(col_percentages_df, reorder(Variable, Percentage))
  
  return(col_percentages_df)
}

percentages_2012 <- issues_percentage(path_2012, column_labels_2012, variable_names, "Year 2012")
percentages_2014 <- issues_percentage(path_2014, column_labels_2014_and_2016, variable_names, "Year 2014")
percentages_2016 <- issues_percentage(path_2016, column_labels_2014_and_2016, variable_names, "Year 2016")

print(percentages_2012)
print(percentages_2014)
print(percentages_2016)

df <- rbind(percentages_2012, percentages_2014, percentages_2016)

p <- ggplot(df, aes(x = Variable, y = Percentage, fill = Year)) +
# geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.8) +
  geom_text(aes(label = paste0(Percentage, "%")), colour="black", position = position_dodge(0.8), hjust = -0.3, size = 2.5) +
  scale_fill_manual(values = c("Year 2012" = "#cde3f7", "Year 2014" = "#9ecae1", "Year 2016" = "#3182bd"),
                    guide = guide_legend(reverse = TRUE)) +
  coord_flip() +  # This makes the bar plot horizontal
  theme_bw() +  # This makes the background white
  theme(
    panel.border = element_blank(),  # This removes the borders
    panel.grid.major = element_blank(),  # This removes the major grid
    panel.grid.minor = element_blank(),  # This removes the minor grid
    axis.title.x = element_blank(),  # This removes the x-axis label
    axis.title.y = element_blank(),  # This removes the x-axis label
    axis.text.x = element_blank(),  # This removes the x-axis label,
    axis.ticks = element_blank()
  )

print(p)

ggsave("plot.svg", plot = p, dpi = 800, width = 10, height = 6)



