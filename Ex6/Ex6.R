set.seed(1973)

n <- 40
lambda <- 1 / 4
num_amostras <- 1000
threshold <- 126

# Gerar amostras
amostras <- replicate(num_amostras, sum(rexp(n, rate = lambda)))

# Calcular a proporção de valores simulados de Y que são maiores do que 126
proporcao_simulada <- mean(amostras > threshold)

# Parâmetros da distribuição gama
shape <- 40
rate <- 1 / 4

# Calcular a probabilidade exata usando a distribuição gama
proporcao_exata <- pgamma(threshold, shape, rate = rate, lower.tail = FALSE)

# Calcular a diferença entre os resultados das duas abordagens
diferenca <- abs(proporcao_simulada - proporcao_exata) * 100
diferenca <- round(diferenca, 4)

# Imprimir os resultados
cat("Proporção Simulada:", proporcao_simulada, "\n")
cat("Proporção Exata:", proporcao_exata, "\n")
cat("Diferença:", diferenca, "\n")
