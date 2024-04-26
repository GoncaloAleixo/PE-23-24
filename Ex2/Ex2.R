library(ggplot2)

# Leitura dos dados do arquivo CSV
dados <- read.csv("https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/master.csv")

# Selecionar os dados do ano de 2002 para o grupo etário de 55-74 anos
dados_filtrados <- subset(dados, year == 2002 & age == "55-74 years")

# Criar o gráfico com boxplots comparativos para homens e mulheres
ggplot(dados_filtrados, aes(x = sex, y = suicides.100k.pop, fill = sex)) +
  geom_boxplot() +
  labs(title = "Suicide Rates by Sex for Age Group 55-74 in 2002",
       x = "Sex",
       y = "Suicides per 100k Population") +
  theme_minimal()

