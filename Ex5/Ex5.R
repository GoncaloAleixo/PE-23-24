set.seed(1950)

n <- 23
r <- 300
m <- 170
threshold <- 1.5

# Função para gerar valores de T
gerar_valores_T <- function(n, m) {
  replicate(m, {
    Z <- rnorm(n + 1)
    T <- sqrt(n) * Z[1] / sqrt(sum(Z[2:(n + 1)]^2))
    return(T)
  })
}

# Gerar amostras de valores de T
proporcoes <- replicate(r, {
  valores_T <- gerar_valores_T(n, m)
  mean(valores_T <= threshold)
})

# Calcular a média das proporções
media_proporcoes <- mean(proporcoes)

# Calcular a probabilidade exata usando a distribuição t-Student
probabilidade_exata <- pt(threshold, df = n)

# Calcular a diferença entre as aproximações
diferenca <- abs(media_proporcoes - probabilidade_exata) * 100
diferenca <- round(diferenca, 5)

# Imprimir os resultados
cat("Média das Proporções Simuladas:", media_proporcoes, "\n")
cat("Probabilidade Exata:", probabilidade_exata, "\n")
cat("Diferença (x100):", diferenca, "\n")
