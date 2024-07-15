'1) Crie um vector com 1000 número aleatórios que seguem distribuição normal com média 100
e desvio padrão 50.'
set.seed(21) # usei a semente 21 para reprodutibilidade
N <- 1000
y <- rnorm(N, 100, 50)
y

'i) Determine a diferença entre os 500 maiores valores da série e os 500
valores menores da série.'
y_crescente <- sort(y)
y_crescente

y_500menores <- y_crescente[1:500]
y_500maiores <- y_crescente[501:1000]

diferenca <- y_500maiores - y_500menores
diferenca

'ii) Determine o desvio padrão dos 100 maiores valores e dos 100
menores valores (use o comando sort( ) para ordenar de forma crescente).'

y_100maiores <- y_crescente[901:N]
y_100menores <- y_crescente[1:100]

#desvios padrões
desvio_padrao_100maiores <- sd(y_100maiores)
desvio_padrao_100menores <- sd(y_100menores)

desvio_padrao_100maiores
desvio_padrao_100menores

'2) (4,0) Os preços da saca de açúcar foram pesquisados em 3 estabelecimentos, nos meses de jan a dez de
2023, para os supermercados A, B e C. Os resultados foram:
a) elaborar a estatística descritiva dos preços de cada supermercado (1,0); estruturar a matriz de dados,
colocar os nomes das colunas e nomes das linhas, tanto dos preços quanto a matriz do IPCA, utilizar a
ferramenta summary, construir os cálculos da média, desvio padrão;'

meses <- c("Jan/23", "Fev/23", "Mar/23", "Abr/23", "Mai/23", "Jun/23", "Jul/23", "Ago/23", "Set/23", "Out/23", "Nov/23", "Dez/23")
precos <- data.frame(
  Mes = meses,
  A = c(124, 116, 101, 118, 118, 120, 110, 127, 106, 130, 120, 121),
  B = c(111, 101, 130, 107, 127, 129, 122, 103, 122, 127, 130, 120),
  C = c(117, 142, 121, 123, 121, 148, 141, 122, 139, 125, 122, 121),
  IPCA = c(1.0, 1.5, 0.5, 0.8, 0.6, 0.5, 0.8, 0.7, 0.9, 1.1, 0.3, 0.4)
)

# estatísticas descritivas
summary(precos)

media_A <- mean(precos$A)
media_B <- mean(precos$B)
media_C <- mean(precos$C)
sd_A <- sd(precos$A)
sd_B <- sd(precos$B)
sd_C <- sd(precos$C)

cat("Supermercado A - Média:", media_A, "e desvio padrão:", sd_A)
cat("Supermercado B - Média:", media_B, "e desvio padrão:", sd_B)
cat("Supermercado C - Média:", media_C, "e desvio padrão:", sd_C)


'b) construir o histograma e gráfico tipo Box–Plot, para cada supermercado (1,0); utilizar a ferramenta
box-plot e colocar no mesmo gráfico os três supermercados, o nome dos eixos x e y;'

par(mfrow = c(3, 1))
hist(precos$A, main = "Histograma - Supermercado A", xlab = "Preço", col = "darkblue")
hist(precos$B, main = "Histograma - Supermercado B", xlab = "Preço", col = "darkgreen")
hist(precos$C, main = "Histograma - Supermercado C", xlab = "Preço", col = "brown")

# Gráfico Box-Plot
par(mfrow = c(1, 1))
boxplot(precos$A, precos$B, precos$C, names = c("A", "B", "C"), main = "Box-Plot dos Preços nos Supermercados",
        xlab = "Supermercados", ylab = "Preço")


'c) corrigir os preços para a base: dezembro de 2023, considerando a correção do IPCA acumulado (1,0);
criar uma nova matriz dos preços atualizando pelo IPCA acumulado do mês corrente até o mês base, de
dezembro de 2023, multiplicando a matriz original, pela matriz de IPCA acumulada;'

precos$IPCA_acumulado <- cumprod(1 + precos$IPCA / 100)
precos_base_dec <- precos

for (i in 1:3) {
  precos_base_dec[, i + 1] <- precos[, i + 1] / precos$IPCA_acumulado * precos$IPCA_acumulado[12]
}

print(precos_base_dec)


'd) calcular o preço projetado para dezembro de 2024, com a base de preços em dezembro de 2023 -
(utilizar regressão linear); utilizar a matriz obtida no item anterior para fazer as regressões lineares,
para cada supermercado, e depois projetar o resultado para saber o preço de cada supermercado em
dezembro de 2024 (1,0)'

reg_A <- lm(A ~ IPCA_acumulado, data = precos_base_dec)
reg_B <- lm(B ~ IPCA_acumulado, data = precos_base_dec)
reg_C <- lm(C ~ IPCA_acumulado, data = precos_base_dec)

# supondo uma taxa de inflação média 4% ao ano
ipca_proj_2024 <- precos_base_dec$IPCA_acumulado[12] * (1 + 0.04)

preco_proj_A <- predict(reg_A, newdata = data.frame(IPCA_acumulado = ipca_proj_2024))
preco_proj_B <- predict(reg_B, newdata = data.frame(IPCA_acumulado = ipca_proj_2024))
preco_proj_C <- predict(reg_C, newdata = data.frame(IPCA_acumulado = ipca_proj_2024))

cat("Preço projetado para dezembro de 2024 - Supermercado A:", preco_proj_A)
cat("Preço projetado para dezembro de 2024 - Supermercado B:", preco_proj_B)
cat("Preço projetado para dezembro de 2024 - Supermercado C:", preco_proj_C)



'3) (2,0) Um banco personaliza as taxas de juro dos depósitos dos seus clientes.
i) Crie, para 10 clientes, um hipotético vector de taxas de juros, um vetor de saldos; calcule os juros a pagar a cada cliente.
ii) Capitalize esses saldos com as taxas de juros para 5 anos. Utilizar Function.'

#Ex.2. Um banco personaliza as taxas de juro dos depósitos dos seus
#clientes. i) Crie, para 10 clientes, um hipotético vector de taxas de
#juro, um vector de saldos e calcule os juros a pagar a cada cliente. ii)
#Capitalize esses saldos com a taxa de juro respectiva a 5 anos.

#i)
Tx.juros<- c(7.5, 31, 2.3, 6, 21, 15, 4.6, 4.8, 3.6, 5)/100

Saldos<- c(12, 15, 425, 21, 10, 75, 15, 67, 14, 2)*1000

Juros <- Saldos*Tx.juros
Juros

#ii)
Capital.final<- Saldos*(c(1,1,1,1,1,1,1,1,1,1) + Tx.juros)^5
Capital.final

'4) (2,0) Escreva uma função que tem como variáveis o Capital e Trabalho, utilizando a função
Cobb-Douglas, para calcular a produção y = A*Kα*Lβ Com A = 2, α = 0,8 e β = 0,2. Determine a
Produção final se aplicados R$ 1.000 com 100 horas de trabalho para definir a produção.
Experimente usar a função para determinar qual a produção se α = 0,7 e β = 1,2 com R$ 20.000 e
250 horas. Escrever a solução utilizando a linguagem R usando a “FUNCTION”, COM
PARÂMETROS EM UM VETOR DE ENTRADA DE DADOS.'

funcao_producao <- function(A,a,b,K,L){
  cobb_douglas <- A*(K^a)*L^(b)
  return(cobb_douglas)
}

# i) Produção final se aplicados R$ 1.000 com 100 horas de trabalho para definir a produção.
A <-2
a1 <-0.8
b1 <-0.2
K1 <- 1000
L1 <- 100
producao1 <- funcao_producao(A,a1,b1,K1,L1)
cat("A produção final considerando as variáveis dadas A=", A, ", alfa=", a1,
    ", beta=", b1, ", K=", K1, "e L=", L1, "é de:", producao1)

# ii) Determinar qual a produção se α = 0,7 e β = 1,2 com R$ 20.000 e 250 horas

a2 <- 0.7
b2<- 1.2
K2<- 20000
L2<- 250
producao2 <- funcao_producao(A, a2, b2, K2, L2)
cat("A produção final considerando as variáveis dadas A=", A, ", alfa=", a2,
    ", beta=", b2, ", K=", K2, "e L=", L2, "é de:", producao2)

'5) (1,0) Elaborar o gráfico da curva de demanda utilizando a programação em R, “quebrada”,
sabendo-se que: (utilizar as seguintes variáveis: b1= (média =0,10 e desvio padrão = 0,02) b2=(média
=0,25 e desvio padrão = 0,03)2 b3= (média =0,35 e desvio padrão = 0,04)
Se 0<=x<=1000 a demanda é y = 1100 – b1*x;
Se 1000<x<=2000 a demanda é y = 1250 – b2*x;
Se 2000<x<=3000 a demanda é y = 1590 – b3*x.'

b1 <- rnorm(1, mean = 0.1, sd = .02)
b2 <- rnorm(1, mean = 0.25, sd = .03)
b3 <- rnorm(1, mean = 0.35, sd = .04)

demanda <- function(x){
  if (x >= 0 & x <= 1000){
    return(1100 - b1 * x)
  } else if (x < 2000){
    return(1250 - b2 * x)
  } else if (x <= 3000){
    return (1590 - b3 *x)
  } else {
    return(0)
  }
}

x <- seq(0, 3000, by = 1)
y <- sapply(x, demanda)

plot(x, y, type = "l", col = "lightgreen", lwd = 3, xlab = "Quantidade (x)",
     ylab = "Demanda (y)", main = "Curva da Demanda Quebrada")
grid()
# Adicionar linhas verticais para os pontos de quebra
abline(v = c(1000, 2000), col = "brown", lty = 2)


'6) (1,0) Considere as Matrizes A, B e C e calcule y = 2*A + B^2 - 3*C.
A:    B:    C:
4 7   2 4   1 3
3 2   2 2   1 5'

matrizA <- matrix(c(4, 7,
                    3, 2),
                  nrow = 2, byrow = TRUE)
matrizB <- matrix(c(2, 4,
                    2, 2),
                  nrow = 2, byrow = TRUE)
matrizC <- matrix(c(1,3,
                    1,5),
                  nrow = 2, byrow = TRUE)

produto_matrizes <- 2*matrizA + matrizB^2 - 3*matrizC
cat("O resultado do produto das matrizes y = 2*A + B^2 - 3*C é:")
produto_matrizes


'7) Utilize o comando “for” e elabore o algoritmo para calcular a soma dos primeiros 20 números
de x, sendo a função definida pela equação y1 = 2 + x^2-0.05*x^3. Se soma em y1(x) for menor que
zero, a função soma passa da função y1(x) para y2(x), com y2(x) = 2 + x^2-0.01*x^3. Faça o gráfico
da função soma acumulada.'

set.seed(21)
x <- rnorm(40, 1, 0.01)

soma <- numeric(20)
y1 <- numeric(20); y2 <- numeric(20)

for (i in 1:20) {
  y1[i] <- 2 + x[i]^2 - 0.05 * x[i]^3
  if (y1[i] < 0) {
    y2[i] <- 2 + x[i]^2 - 0.01 * x[i]^3
    soma[i] <- y2[i]
  } else {
    soma[i] <- y1[i]
  }
}

print(sum(soma))

x20 <- x[1:20]

plot(x20, soma, type = "l", col = "magenta", lwd = 3, xlab = "x",
     ylab = "y", main = "Função soma acumulada")
grid()



'8) Calcular o lucro máximo e o respectivo preço que a empresa deveria praticar,
considerando o padrão da função custo indicado abaixo, com três tipos de curva e as
respectivas demandas. Fazer o gráfico dos custos, lucros e receita.
Ci= xia + b*xic + d*xi + e; Função Demanda: yi= a + b*xi; sendo i = 1.'

custo <- function(x, a, b, c, d, e){
  return(x^a + b*x^c + d*x + e)
}

demanda <- function(x, a_dem, b_dem){
  return(a_dem + b_dem*x)
}

# intervalo de x para análise
x <- seq(0, 100, by = 0.1)

# curvas de custo
curva_custo1 <- custo(x, 3, -300, 2, -9, 150)
curva_custo2 <- custo(x, 3, -200, 2, -10, 140)
curva_custo3 <- custo(x, 3, -100, 2, -15, 100)

plot(x, curva_custo1, type = "l", col = "pink", ylim = c(min(curva_custo1, curva_custo2, curva_custo3),
                                                   max(curva_custo1, curva_custo2, curva_custo3)), lwd = 2, xlab = "Quantidade", ylab = "Preço",
     main = "Gráfico dos Custos")
grid()
lines(x, curva_custo2, col = "lightblue", lwd = 2)
lines(x, curva_custo3, col = "lightgreen", lwd = 2)
legend("topleft", inset = c(0, 0), legend = c("Custo 1", "Custo 2", "Custo 3"),
       col = c("pink", "lightblue", "lightgreen"), lty = 1, lwd = 3, xpd = FALSE, cex = 0.8)


# curvas da demanda
curva_demanda1 <- demanda(x, 30000, -0.5)
curva_demanda2 <- demanda(x, 20000, -0.4)
curva_demanda3 <- demanda(x, 20500, -0.3)

plot(x, curva_demanda1, type = "l", col = "pink", ylim = c(min(curva_demanda1, curva_demanda2, curva_demanda3),
                                                         max(curva_demanda1, curva_demanda2, curva_demanda3)), lwd = 2, xlab = "Quantidade", ylab = "Preço",
     main = "Gráfico das Demandas")
grid()
lines(x, curva_demanda2, col = "lightblue", lwd = 2)
lines(x, curva_demanda3, col = "lightgreen", lwd = 2)
legend("topleft", inset = c(0, 0), legend = c("Demanda 1", "Demanda 2", "Demanda 3"),
       col = c("pink", "lightblue", "lightgreen"), lty = 1, lwd = 3, xpd = FALSE, cex = 0.8)


#Receita da curva 1
receitas1 <- curva_demanda1 * x
lucros1 <- receitas1 - curva_custo1

lucro_maximo1 <- max(lucros1)
preco_maximo1 <- x[which.max(lucros1)]

cat("Lucro máximo: ", lucro_maximo1, "\n")
cat("Preço que maximiza o lucro da curva 1: ", preco_maximo1, "\n")

#Receita da curva 2
receitas2 <- curva_demanda2 * x
lucros2 <- receitas2 - curva_custo2

lucro_maximo2 <- max(lucros2)
preco_maximo2 <- x[which.max(lucros2)]

cat("Lucro máximo: ", lucro_maximo2, "\n")
cat("Preço que maximiza o lucro da curva 2: ", preco_maximo2, "\n")

#Receita da curva 3
receitas3 <- curva_demanda3 * x
lucros3 <- receitas3 - curva_custo3

lucro_maximo3 <- max(lucros3)
preco_maximo3 <- x[which.max(lucros3)]

cat("Lucro máximo: ", lucro_maximo3, "\n")
cat("Preço que maximiza o lucro da curva 3: ", preco_maximo3, "\n")

plot(x, receitas1, type = "l", col = "pink", ylim = c(min(receitas1, receitas2, receitas3),
                                                           max(receitas1, receitas2, receitas3)), lwd = 2, xlab = "Quantidade", ylab = "Preço",
     main = "Gráfico das Receitas")
grid()
lines(x, receitas2, col = "lightblue", lwd = 2)
lines(x, receitas3, col = "lightgreen", lwd = 1)
legend("topleft", inset = c(0, 0), legend = c("Receita 1", "Receita 2", "Receita 3"),
       col = c("pink", "lightblue", "lightgreen"), lty = 1, lwd = 3, xpd = FALSE, cex = 0.8)

plot(x, lucros1, type = "l", col = "pink", ylim = c(min(lucros1, lucros2, lucros3),
                                                    max(lucros1, lucros2, lucros3)), lwd = 2, xlab = "Quantidade", ylab = "Preço",
     main = "Gráfico do Lucros")
grid()
lines(x, lucros2, col = "lightblue", lwd = 2)
lines(x, lucros3, col = "lightgreen", lwd = 2)
legend("topleft", inset = c(0, 0), legend = c("Lucro 1", "Lucro 2", "Lucro 3"),
       col = c("pink", "lightblue", "lightgreen"), lty = 1, lwd = 3, xpd = FALSE, cex = 0.8)


'9) (1,0) Escrever o algoritmo em R para calcular a integral definida.'

f <- function(x) {
  return(1 / (16 + x^2))
}

resultado <- integrate(f, lower = 0, upper = 3)
 
print(resultado$value)

'10) (2,0) Utilizando programação matemática em R: Definir a curva de Demanda para o produto listado abaixo.
1) Fazer o Gráfico da Curva de Demanda, ORIGINAL E CALCULADA; 2) Fazer a regressão linear da equação da
reta – demanda linear – Definir os coeficientes a e b da equação y = a + b*x :'

consumidores <- c(1:34)
precos <- c(3000, 1000, NA, 2500, 4200, 5570, 900, NA, 3000, 10000, 
            5000, NA, 3200, 15000, 1000, 3500, 5000, NA, 5000, 3000, 
            NA, 2000, NA, 15000, 5000, 10000, 4000, 5000, 10000, 
            8000, NA, 4000, 2500, 2000)

precos[is.na(precos)] <- mean(precos, na.rm = TRUE)
tabela <- data.frame(Consumidores = consumidores, Precos = precos)
View(tabela)

plot(tabela$Precos, tabela$Consumidores, type="b", col="darkgreen", lwd = 1.6,
     xlab="Preço", ylab="Quantidade Consumida", main="Curva de Demanda - Original")

demanda_regressao <- lm(Precos ~ Consumidores, data = tabela)

summary(demanda_regressao)

coeficientes <- coef(demanda_regressao)
a <- coeficientes[1]
b <- coeficientes[2]

cat("Coeficiente a:", a)
cat("Coeficiente b:", b)

plot(tabela$Precos, tabela$Consumidores, type="p", col="darkgreen", 
     xlab="Preço", ylab="Quantidade Consumida", main="Curva de Demanda - Original e Calculada")
points(tabela$Precos, tabela$Consumidores, col="darkgreen", pch=19)
abline(demanda_regressao, col="darkred", lwd=2)
legend("topright", legend=c("Original", "Calculada"), col=c("darkgreen", "darkred"), lty=1, lwd=2)


'11) (1,0) UTILIZAR O SCRIPT: #https://rpubs.com/Lucas_Venturini/661262 para indicar a combinação
ótima da carteira com os seguintes ativos: CARTEIRA 1: ELET3; ITUB4; VALE3; CARTEIRA 2:
MGLU3; PETR3; BBAS3.'

if (!require("quantmod")) install.packages("quantmod", dependencies=TRUE)
if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics", dependencies=TRUE)
if (!require("quadprog")) install.packages("quadprog", dependencies=TRUE)

library(quantmod)
library(PerformanceAnalytics)
library(quadprog)

tickers_carteira1 <- c('ELET3.SA', 'ITUB4.SA', 'VALE3.SA')
tickers_carteira2 <- c('MGLU3.SA', 'PETR3.SA', 'BBAS3.SA')

obter_dados <- function(tickers) {
  dados <- NULL
  for (ticker in tickers) {
    temp <- getSymbols(ticker, src = "yahoo", from = "2020-01-01", to = "2023-01-01", auto.assign = FALSE)
    dados <- cbind(dados, Cl(temp))
  }
  retornos <- na.omit(ROC(dados, type = "discrete"))
  colnames(retornos) <- tickers
  return(retornos)
}

retornos_carteira1 <- obter_dados(tickers_carteira1)
retornos_carteira2 <- obter_dados(tickers_carteira2)

estatisticas_portfolio <- function(pesos, retornos) {
  retorno_portfolio <- sum(colMeans(retornos) * pesos) * 252
  volatilidade_portfolio <- sqrt(t(pesos) %*% (cov(retornos) * 252) %*% pesos)
  return(c(retorno_portfolio, volatilidade_portfolio))
}

minimizar_volatilidade <- function(pesos, retornos) {
  return(estatisticas_portfolio(pesos, retornos)[2])
}

restricoes <- function(pesos) sum(pesos) - 1
limites <- cbind(rep(0, length(tickers_carteira1)), rep(1, length(tickers_carteira1)))

pesos_iniciais <- rep(1/length(tickers_carteira1), length(tickers_carteira1))
resultado1 <- optim(par = pesos_iniciais, fn = minimizar_volatilidade, retornos = retornos_carteira1, method = "L-BFGS-B", lower = limites[, 1], upper = limites[, 2], constraints = restricoes)

pesos_otimos_carteira1 <- resultado1$par
print(paste("Pesos ótimos para a Carteira 1:", paste(round(pesos_otimos_carteira1, 4), collapse = ", ")))

limites2 <- cbind(rep(0, length(tickers_carteira2)), rep(1, length(tickers_carteira2)))
pesos_iniciais2 <- rep(1/length(tickers_carteira2), length(tickers_carteira2))
resultado2 <- optim(par = pesos_iniciais2, fn = minimizar_volatilidade, retornos = retornos_carteira2, method = "L-BFGS-B", lower = limites2[, 1], upper = limites2[, 2], constraints = restricoes)

pesos_otimos_carteira2 <- resultado2$par
print(paste("Pesos ótimos para a Carteira 2:", paste(round(pesos_otimos_carteira2, 4), collapse = ", ")))

'12) (2,0) Utilizando programação matemática em R: Definir o lucro máximo e o respectivo preço que as
empresas deveriam praticar, considerando o padrão das funções custos indicados abaixo. O Fazer o gráfico
dos custos, lucros e receitas de cada empresa, INCLUIR O NOME DOS GRÁFICOS, O NOME DOS EIXOS X e Y. A
quantidade produzida deverá ser aquela que maximize o lucro, considerando o preço.'
# Parâmetros das curvas de custo
curvas <- data.frame(
  Curva = c("Curva 1", "Curva 2", "Curva 3"),
  A = c(0.10, 0.15, 0.18),
  a = c(3, 3, 3),
  B = c(-4, -5, -10),
  c = 2,
  d = c(1500, 1200, 1300)
)

# Função para calcular o custo total
calcular_custo <- function(A, a, B, c, d, x) {
  A * x^a + B * x^c + d
}

# Função para calcular a receita
calcular_receita <- function(preco, x) {
  preco * x
}

# Função para calcular o lucro
calcular_lucro <- function(A, a, B, c, d, preco, x) {
  receita <- calcular_receita(preco, x)
  custo <- calcular_custo(A, a, B, c, d, x)
  lucro <- receita - custo
  return(lucro)
}

# Função para encontrar a quantidade que maximiza o lucro
maximizar_lucro <- function(A, a, B, c, d, preco) {
  f <- function(x) -calcular_lucro(A, a, B, c, d, preco, x)
  resultado <- optimize(f, interval = c(0, 1000))
  quantidade_maxima <- resultado$minimum
  lucro_maximo <- -resultado$objective
  return(list(quantidade_maxima = quantidade_maxima, lucro_maximo = lucro_maximo))
}

# Intervalo de preços
precos <- seq(0, 1000, by = 10)

# Gráficos
par(mfrow = c(3, 1))
for (i in 1:nrow(curvas)) {
  A <- curvas$A[i]
  a <- curvas$a[i]
  B <- curvas$B[i]
  c <- curvas$c[i]
  d <- curvas$d[i]
  
  lucros <- sapply(precos, function(preco) maximizar_lucro(A, a, B, c, d, preco)$lucro_maximo)
  quantidade_maxima <- sapply(precos, function(preco) maximizar_lucro(A, a, B, c, d, preco)$quantidade_maxima)
  
plot(precos, lucros, type = "l", col = "blue", lwd = 2, 
       main = paste("Curva de Lucro -", curvas$Curva[i]), 
       xlab = "Preço", ylab = "Lucro")
abline(h = 0, col = "red")
  
plot(precos, quantidade_maxima, type = "l", col = "green", lwd = 2, 
       main = paste("Quantidade que Maximiza o Lucro -", curvas$Curva[i]), 
       xlab = "Preço", ylab = "Quantidade")
abline(h = 0, col = "red")
}

# Mostrar resultados
for (i in 1:nrow(curvas)) {
  A <- curvas$A[i]
  a <- curvas$a[i]
  B <- curvas$B[i]
  c <- curvas$c[i]
  d <- curvas$d[i]
  
  resultado <- maximize_lucro(A, a, B, c, d, precos)
  print(paste("Resultados para", curvas$Curva[i]))
  print(paste("Quantidade que maximiza o lucro:", round(resultado$quantidade_maxima, 2)))
  print(paste("Lucro máximo:", round(resultado$lucro_maximo, 2)))
}



'13) (2,0) Utilizando programação matemática em R: Utilize o comando “for” e
elabore o algoritmo para calcular a série (Faça para x= 5) dos 100, n = 100,
primeiros termos.'

x <- 5
soma <- 0

fatorial <- function(n) {
  if (n == 0) return(1)
  return(prod(1:n))
}

# loop para calcular a série até n = 100
for (n in 1:100) {
  if (n %% 2 == 1) {
    termo <- - (n + 2) * x^n / ((n + 1) * fatorial(n))
  } else {
    termo <- (n + 2) * x^n / ((n + 1) * fatorial(n))
  }
  soma <- soma + termo
}

# Exibir o resultado da soma
cat("A soma dos primeiros 100 termos da série para x =", x, "é:", soma)


'14) (2,0) Utilizando programação matemática em R: Resolver o sistema linear em R:
x + 2y - z = 2
2x – y + z = 3
x + y + z = 6'

A <- matrix(c(1, 2, 1,
              2, -1, 1,
              1, 1, 1), nrow = 3, byrow = TRUE)

coeficientes <- c(2, 3, 6)

solucao <- solve(A, coeficientes)

print(solucao)
cat("A solução do sistema é: x =", solucao[1], ", y =", solucao[2], "e z =", solucao[3])







