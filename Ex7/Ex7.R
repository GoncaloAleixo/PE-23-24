# Carregar o pacote necessário
if(!require(stats4)) install.packages("stats4", dependencies=TRUE)
library(stats4)

# Dados fornecidos
dados <- c(8.54, 4.76, 5.15, 4.96, 6.25, 7.22, 12.9, 6.04, 8.86, 4.88, 6.54, 4.53, 4.7, 5.38, 5.96, 5.17, 5.09, 5.11)

# Definir a função de verossimilhança negativa
verossimilhança <- function(theta) {
  a <- 4.5
  n <- length(dados)
  -sum(log(theta * a^theta * dados^(-theta - 1)))
}

# Usar a função mle para encontrar a estimativa de máxima verossimilhança de θ
resultado_mle <- mle(verossimilhança, start = list(theta = 3.4))

# Obter a estimativa de θ
theta_mle <- coef(resultado_mle)
theta_mle

# Calcular o quantil de probabilidade p=0.25 para a distribuição com o θ estimado
p <- 0.25
a <- 4.5
quantil_estimado <- a * (1 / (1 - p))^(1 / theta_mle)
quantil_estimado

# Calcular o verdadeiro valor do quantil quando θ=3.4
theta_verdadeiro <- 3.4
quantil_verdadeiro <- a * (1 / (1 - p))^(1 / theta_verdadeiro)
quantil_verdadeiro

# Calcular o desvio absoluto entre a estimativa e o verdadeiro valor do quantil
desvio_absoluto <- abs(quantil_estimado - quantil_verdadeiro)
desvio_absoluto
