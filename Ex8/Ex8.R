set.seed(1820)

# Dados fornecidos
dados <- c(34.0, 39.5, 33.2, 38.1, 29.9, 37.4, 32.1, 36.5, 31.4, 34.1, 33.1, 31.5, 33.9, 33.9)

# Extração de uma amostra de tamanho n = 8
amostra <- sample(dados, size = 8, replace = FALSE)
amostra

# Definindo o nível de confiança
gamma <- 0.95
n <- 8
alpha <- (1 - gamma) / 2

# Calculando os quantis a e b
a <- qchisq(alpha, df = n - 1)
b <- qchisq(1 - alpha, df = n - 1)

# Calculando a variância amostral
s2 <- var(amostra)

# Calculando o intervalo de confiança (a, b)
IC_inf <- (n - 1) * s2 / b
IC_sup <- (n - 1) * s2 / a

intervalo_ab <- c(IC_inf, IC_sup)
intervalo_ab

if(!require(pracma)) install.packages("pracma", dependencies=TRUE)
library(pracma)

# Funções para encontrar c e d
f1 <- function(x) pchisq(x[2], df = n - 1) - pchisq(x[1], df = n - 1) - gamma
f2 <- function(x) dchisq(x[2], df = n + 3) - dchisq(x[1], df = n + 3)

# Usando fsolve para encontrar c e d
solucao <- fsolve(function(x) c(f1(x), f2(x)), c(a, b))
c <- solucao$x[1]
d <- solucao$x[2]

# Calculando o novo intervalo de confiança (c, d)
IC_inf_cd <- (n - 1) * s2 / d
IC_sup_cd <- (n - 1) * s2 / c

intervalo_cd <- c(IC_inf_cd, IC_sup_cd)
intervalo_cd

# Amplitude dos intervalos
amplitude_ab <- intervalo_ab[2] - intervalo_ab[1]
amplitude_cd <- intervalo_cd[2] - intervalo_cd[1]

# Diferença entre as amplitudes
diferenca_amplitude <- amplitude_ab - amplitude_cd

# Imprimir os resultados
print(paste("Intervalo de confiança (a, b):", intervalo_ab))
print(paste("Intervalo de confiança (c, d):", intervalo_cd))
print(paste("Diferença entre as amplitudes:", diferenca_amplitude))
