library(ggplot2)
library(readxl)

# Carregando os dados
dados <- read_xlsx("electricity.xlsx")

# Convertendo YEAR e MONTH para numérico
dados$YEAR <- as.numeric(as.character(dados$YEAR))
dados$MONTH <- as.numeric(as.character(dados$MONTH))

# Garantindo que VALUE é numérico
dados$VALUE <- as.numeric(as.character(dados$VALUE))

# Filtrando os dados a partir de 2015 e para os países específicos
dados_filtrados <- subset(dados, YEAR >= 2015 & COUNTRY %in% c("IEA Total", "Italy", "Latvia"))

# Criando uma coluna de data no formato "ano-mês"
dados_filtrados$Date <- as.Date(paste(dados_filtrados$YEAR, dados_filtrados$MONTH, "1", sep = "-"), format="%Y-%m-%d")

# Calculando a soma total de energia e a energia renovável por mês e país
total_energia <- aggregate(VALUE ~ Date + COUNTRY, data = dados_filtrados, FUN = sum)
renovaveis <- aggregate(VALUE ~ Date + COUNTRY, data = subset(dados_filtrados, PRODUCT == "Renewables"), FUN = sum)

# Renomeando as colunas para facilitar o merge
colnames(total_energia) <- c("Date", "COUNTRY", "TotalEnergy")
colnames(renovaveis) <- c("Date", "COUNTRY", "RenewableEnergy")

# Combinando os dados
dados_combinados <- merge(renovaveis, total_energia, by = c("Date", "COUNTRY"))

# Calculando a proporção de energia renovável
dados_combinados$RenewableShare <- (dados_combinados$RenewableEnergy / dados_combinados$TotalEnergy) * 100

# Certificando que a coluna RenewableShare está dentro de um intervalo razoável (0-100)
summary(dados_combinados$RenewableShare)

# Selecionando apenas as colunas necessárias para o gráfico
dados_para_grafico <- dados_combinados[, c("Date", "COUNTRY", "RenewableShare")]

# Gráfico da evolução mensal da proporção de energia renovável
ggplot(dados_para_grafico, aes(x = Date, y = RenewableShare, color = COUNTRY, group = COUNTRY)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
  labs(title = "Monthly Evolution of Renewable Energy Production Share",
       subtitle = "Proportion of electricity from renewable sources since 2015",
       x = "Date",
       y = "Renewable Energy Share (%)") +
  theme_minimal()
