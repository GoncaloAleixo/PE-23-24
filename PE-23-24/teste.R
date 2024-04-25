library(ggplot2)
theme_set(theme_gray())

url <- 'https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/LE%20vs%20GDP.csv'
dados <- read.csv(url, check.names = FALSE)
ano <- 1966
continentes <- c("Africa", "Europe", "Asia")

dados |>
  subset(Year == ano & Continent %in% continentes) |>
  ggplot() +
  geom_point(aes(`GDP per capita`, `Life expectancy`, color = Continent)) +
  scale_x_log10() +
  labs(title = paste("Life expectancy vs GDP per capita in", ano),
       x = "GDP per capita (2011 US dollars)",
       y = "Life expectancy (years)")