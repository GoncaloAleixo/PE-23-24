set.seed(2822)

# Definindo os parâmetros
lambda_0 <- 2.90
lambda_1 <- 3.15
k <- 3.234
n <- 100
m <- 5000

# Função para calcular a média de uma amostra de Poisson
sample_mean <- function(lambda, n) {
  return(mean(rpois(n, lambda)))
}

# Inicializando contadores para os erros
type_1_error <- 0  # Erro de 1ª espécie
type_2_error <- 0  # Erro de 2ª espécie

# Realizando a simulação
for (i in 1:m) {
  # Gerando amostras sob H0 e H1
  sample_H0 <- sample_mean(lambda_0, n)
  sample_H1 <- sample_mean(lambda_1, n)
  
  # Decidindo se rejeitamos H0
  if (sample_H0 > k) {
    type_1_error <- type_1_error + 1
  }
  
  # Decidindo se falhamos em rejeitar H0
  if (sample_H1 <= k) {
    type_2_error <- type_2_error + 1
  }
}

# Calculando as frequências relativas dos erros
freq_type_1_error <- type_1_error / m
freq_type_2_error <- type_2_error / m

# Calculando o quociente entre a probabilidade de erro de 2ª espécie e a probabilidade de erro de 1ª espécie
quotient <- freq_type_2_error / freq_type_1_error

# Output das frequências relativas e do quociente
print(paste("Frequência relativa de erro de 1ª espécie:", freq_type_1_error))
print(paste("Frequência relativa de erro de 2ª espécie:", freq_type_2_error))
print(paste("Quociente entre a probabilidade de erro de 2ª espécie e a probabilidade de erro de 1ª espécie:", quotient))