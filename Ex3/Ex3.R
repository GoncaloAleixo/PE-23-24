library(ggplot2)
library(readxl)

# Carregando os dados
dados <- read_xlsx("electricity.xlsx")

# Assegurando que YEAR e MONTH são numéricos
dados$YEAR <- as.numeric(as.character(dados$YEAR))
dados$MONTH <- as.numeric(as.character(dados$MONTH))

# Convertendo a coluna 'share' para numérica
# Substituindo possíveis vírgulas por pontos e convertendo para numérico
dados$share <- as.numeric(gsub(",", ".", as.character(dados$share)))

# Verificando se ainda há algum NA após a conversão que pode causar problemas
#sum(is.na(dados$share))

# Filtrando os dados a partir de 2015 e para os países específicos
dados_filtrados <- subset(dados, YEAR >= 2015 & COUNTRY %in% c("IEA Total", "Italy", "Latvia"))

# Isolando apenas os dados referentes a energias renováveis
dados_renovaveis <- subset(dados_filtrados, PRODUCT == "Renewables")

# Criando uma coluna de data no formato "ano-mês"
dados_renovaveis$Date <- as.Date(paste(dados_renovaveis$YEAR, dados_renovaveis$MONTH, "1", sep = "-"), format="%Y-%m-%d")

# Agora selecionamos apenas as colunas necessárias para o gráfico
dados_para_grafico <- dados_renovaveis[, c("Date", "COUNTRY", "share")]

# Verificar se todos os valores de 'share' estão corretamente convertidos
#str(dados_para_grafico)

# Se estiver tudo certo, prosseguimos para o gráfico
# Gráfico da evolução mensal da proporção de energia renovável
ggplot(dados_para_grafico, aes(x = Date, y = share * 100, color = COUNTRY, group = COUNTRY)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
  labs(title = "Monthly Evolution of Renewable Energy Production Share",
       subtitle = "Proportion of electricity from renewable sources since 2015",
       x = "Date",
       y = "Renewable Energy Share (%)") +
  theme_minimal()
