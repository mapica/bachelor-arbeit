library(tidyverse)
library(haven)
library(ggplot2)
library(gridExtra)
library(svglite)
library(car)
library(stargazer)

df <- read_csv("./data/ESS8e02_2/ESS8e02_2.csv")

# Selected countries
selected_countries <- c("HU", "DE", "SE", "AT", "IT", "FR", "NL", "BE", "GB", "FI")

# EU countries 
eu_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE")

# Other EU countries that are not selected
other_eu_countries <- setdiff(eu_countries, selected_countries)

df$CountryCategory <- ifelse(df$cntry %in% selected_countries, "Respondents from EU countries that accepted\na large number of refugees with granted asylum", ifelse(df$cntry %in% other_eu_countries, "Other EU countries", NA))

# Remove NA rows (countries that are not in selected or other EU categories)
df <- df[!is.na(df$CountryCategory), ]

df <- subset(df, euftf <= 10)
# we invert the variable such that a larger value indicates more eurosceptism
df$euftf <- 10 - df$euftf

# Calculate averages  
average_euftf <- df %>%
  group_by(CountryCategory) %>%
  summarize(mean_euftf = mean(euftf, na.rm = TRUE))

# Print averages
print(average_euftf)

# Plot averages
plot <- ggplot(average_euftf, aes(x = CountryCategory, y = mean_euftf)) +
  geom_bar(stat="identity", fill = "skyblue") +
  labs(title="Average level of Euroscepticism across EU member states", x="Country Category", y = "Mean level of Euroscepticism 0-10 scale") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", colour=NA),  # Transparent plot background
    axis.text.x = element_text(size = 12),          # increase x-axis text size
    axis.text.y = element_text(size = 12),          # increase y-axis text size
    axis.title.y = element_text(size = 14),         # increase y-axis title size
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(plot)

ggsave("mean_euroscepticism.svg", plot = plot, dpi = 800, width = 10)


