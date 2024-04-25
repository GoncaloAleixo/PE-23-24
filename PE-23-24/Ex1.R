library(ggplot2)
theme_set(theme_gray())

# Defina os países para destacar
paises_destaque <- c("Lithuania", "Iceland", "United States", "Saint Lucia")

# Ler os dados
url <- 'https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/Paises_PIB_ICH.csv'
dados <- read.csv(url, check.names = FALSE)

# Selecionar os dados dos continentes Europa e Américas
dados_selecionados <- subset(dados, Continent %in% c("Europe", "Americas"))

# Criar o gráfico de dispersão
ggplot(dados_selecionados, aes(x = GDP, y = HCI, color = Continent)) +
  geom_point(aes(shape = ifelse(Country %in% paises_destaque, Country, "Other")), size = 3) +
  scale_x_log10() +
  scale_shape_manual(values = c(rep(16, 4), 17)) +  # Configuração manual das formas
  theme(legend.position = "bottom") +
  labs(title = "Human Capital Index vs GDP per Capita (2023, IMF estimates)",
       x = "GDP per Capita (log scale, international dollars)",
       y = "Human Capital Index (2020, World Bank)",
       color = "Continent", 
       shape = "Country") +
  geom_text(aes(label = ifelse(Country %in% paises_destaque, as.character(Country), "")), hjust = 0.5, vjust = -1)

# Exibir o gráfico
grafico
