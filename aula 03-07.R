install.packages("readxl")
library(readxl)

dados <- read_excel("Importar Arquivo Excel.xlsx", sheet = 1)

summary(dados)

plot(dados$AA, type = "l", col = 1, lwd = 3)  # linha azul
lines(dados$EE, col = 2, lwd = 2)  # linha vermelha
lines(dados$FF, col = 3, lwd = 2)  # linha verde
lines(dados$DD, col = 4, lwd = 2)  # linha preta
lines(dados$BB, col = 5, lwd = 2)  # linha roxa
lines(dados$CC, col = 6, lwd = 2)  # linha laranja

plot(dados$BB, dados$AA, xlab = "BB", ylab = "AA", main = "Gráfico de Dispersão")
abline(modelo, col = "magenta", lwd = 2)

# regressão linear simples
regressao <- lm(AA ~ BB, data = dados)
summary(regressao)

plot(dados$BB, dados$AA, xlab = "BB", ylab = "AA", main = "Gráfico de Dispersão da regressão linear")
abline(lm(AA ~ BB, data = dados), col = "cyan", lwd = 2) 

'calcular a expressão: sendo o resultado da regressão linear igual = a +b*x
z=20*[Coeficiente da regressão- termo independente, coeficiente a] + 30*[coeficiente do termo dependente, coeficiente b]'
coeficientes <- coef(regressao)
z <- 20 * coeficientes[1] + 30 * coeficientes[2]
z

# Regressão Linear Múltipla
modelo <- lm(AA ~ BB + CC + DD + EE + FF, data = dados)
summary(modelo)
coeficientes <- coef(modelo)
print(coeficientes)

