'2) Escreva uma função que tem como variáveis o Capital e Trabalho, utilizando a função
Cobb-Douglas adicionada, para calcular a produção y(K,L) = A*Kα*L2*β + (1-A)* K2*
α*Lβ+0,1 
Com A = 2, α = 0,8 e β = 0,2. Determine a Produção final se aplicados R$ 1.000 com 100 horas de trabalho
para definir a produção. Usar a função para determinar qual a produção se α = 0,7 e β = 1,2 com R$
20.000 e 250 horas. Escrever a solução utilizando a linguagem R usando a “FUNCTION”, COM
PARÂMETROS EM UM VETOR DE ENTRADA DE DADOS.'

#função
funcao_producao <- function(A,a,b,K,L) {
  resultado <- A*(K^a)*L^(2*b)+(1-A)*K^(2*a)*L^(b+0.1)
  return(resultado)
}

#parâmetros
A <-2
a1 <-0.8
b1 <-0.2
K1 <- 1000
L1 <- 100

producao1 <- funcao_producao(A,a1,b1,K1,L1)
cat("A produção final considerando as variáveis dadas A=", A, ", alfa=", a1,
    ", beta=", b1, ", K=", K1, "e L=", L1, "é de:", producao1)

#Novos parâmetros
a2 <- 0.7
b2 <- 1.2
K2 <- 20000
L2 <- 250
producao2 <- funcao_producao(A,a2,b2,K2,L2)
cat("A produção final considerando as variáveis dadas A=", A, ", alfa=", a2,
    ", beta=", b2, ", K=", K2, "e L=", L2, "é de:", producao2)

'3) Considere as Matrizes A B e C e calcule: y = 2*A3 + 3*B2 - 3*C4. Escrever a solução utilizando a linguagem R.
A:    B:   C:
3 3   2 2   1 1
3 3   2 2   1 1'

matrizA <- matrix(c(3, 3,
                    3, 3),
                    nrow = 2, byrow = TRUE)
matrizB <- matrix(c(2, 2,
                    2, 2),
                  nrow = 2, byrow = TRUE)
matrizC <- matrix(c(1,1,
                    1,1),
                  nrow = 2, byrow = TRUE)

produto_matrizes <- 2*matrizA^3 + 3*matrizB^2 - 3*matrizC^4
cat("O resultado do produto das matrizes y = 2*A^3 + 3*B^2 - 3*C^4 é:")
produto_matrizes


'4) Utilize o comando “for” e elabore o algoritmo para calcular a soma dos primeiros 50 números
de x, sendo a função definida pela equação y1(x) = 2 + x^2-0.05*x^3. Se soma y1(x) for menor que
zero, a função passa para y2(x) = 2 + x^2-0.01*x^3.Faça o gráfico da função soma acumulada.
Escrever a solução utilizando a linguagem R'

y1 <- function(x) {
  2 + x^2 - 0.05*x^3
  }

y2 <- function(x) {
  2 + x^2 - 0.1*x^3
}


soma_condicionada <- function(x){
  soma <- 0
  soma_acumulada <- numeric(length(x))
  
  for (i in 1:length(x)){
    valor <- y1(x[i])
    soma <- soma + valor
    
    if (soma < 0) {
      valor <- y2(x[i])
      soma <- soma - y1(x[i] + valor)
    }
    
    soma_acumulada[i] <- soma
  }
  
  return(soma_acumulada)
}

x <- rnorm(100,2,1)
x50 <- x[1:50]
soma_acumulada <- soma_condicionada(x50)

print(soma_acumulada)

plot(1:50, soma_acumulada, type = "l", col = "magenta", lwd = 3,
     xlab = "Índice", ylab = "Soma Acumulada",
     main = "Gráfico da Soma Acumulada dos 50 primeiros termos")


'5) Calcular o lucro máximo e o respectivo preço que a empresa deveria praticar,
considerando o padrão da função custo indicado abaixo, com três tipos de curva e as
respectivas demandas. Fazer o gráfico dos custos, lucros e receita.
Ci= xia + b*xic + d*xi + e; Função Demanda: yi= a + b*xi; sendo i = 1.'

custo1 <- function(x, a, b, c, d, e){
  return(x^a + b*x^c + d*x + e)
}

demanda <- function(x, a_dem, b_dem){
  return(a_dem + b_dem*x)
}

#parâmetros - custos
a <- 3
b <- -2
c <- 2
d <- -1
e <- 150

# parâmetro - demanda
a_dem <- 30000
b_dem <- -0.5

# intervalo de x para análise
x <- seq(0, 100, by = 0.1)

custos <- custo1(x, a, b, c, d, e)
receitas <- demanda(x, a_dem, b_dem) * x
lucros <- receitas - custos


lucro_maximo <- max(lucros)
preco_maximo <- x[which.max(lucros)]

cat("Lucro máximo: ", lucro_maximo, "\n")
cat("Preço que maximiza o lucro: ", preco_maximo, "\n")


plot(x, custos, type = "l", col = "pink", ylim = c(min(custos, lucros, receitas),
    max(custos, lucros, receitas)), lwd = 4, xlab = "Quantidade", ylab = "Valor",
    main = "Gráfico dos Custos, Lucros e Receita")
grid()
lines(x, receitas, col = "lightblue", lwd = 4)
lines(x, lucros, col = "lightgreen", lwd = 4)
legend("topleft", inset = c(0, 0), legend = c("Custos", "Receitas", "Lucros"),
       col = c("pink", "lightblue", "lightgreen"), lty = 1, lwd = 3, xpd = FALSE)


'6) Utilizando programação matemática em R: Utilize o comando “for” e elabore o algoritmo
para calcular a série (Faça para x= 5) dos 100, n = 100, primeiros termos:
Sendo o somatório definido, em x, com n termos:'

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
cat("A soma dos primeiros 100 termos da série para x =", x, "é:", soma, "\n")







'Com 3 curvas de custo'
# Função para calcular o custo
custo <- function(x, a, b, c, d, e) {
  return(x^a + b*x^c + d*x + e)
}

# Função para calcular a demanda
demanda <- function(x, a_dem, b_dem) {
  return(a_dem + b_dem*x)
}

# Parâmetros das funções de custo e demanda para as três curvas
curvas <- list(
  curva1 = list(custo = list(a = 3, b = -2, c = 2, d = -1, e = 150),
                demanda = list(a_dem = 30000, b_dem = -0.5)),
  curva2 = list(custo = list(a = 2, b = -1, c = 1.5, d = -0.8, e = 100),
                demanda = list(a_dem = 25000, b_dem = -0.4)),
  curva3 = list(custo = list(a = 4, b = -3, c = 2.5, d = -1.2, e = 200),
                demanda = list(a_dem = 35000, b_dem = -0.6))
)

# Intervalo de x para análise
x <- seq(0, 100, by = 0.1)

# Preparar os gráficos
par(mfrow = c(3, 1))

# Função para calcular e plotar os gráficos para cada curva
plot_curva <- function(curva, nome) {
  custos <- custo(x, curva$custo$a, curva$custo$b, curva$custo$c, curva$custo$d, curva$custo$e)
  receitas <- demanda(x, curva$demanda$a_dem, curva$demanda$b_dem) * x
  lucros <- receitas - custos
  
  # Encontrar o lucro máximo e o respectivo preço
  lucro_maximo <- max(lucros)
  preco_maximo <- x[which.max(lucros)]
  
  # Exibir o lucro máximo e o respectivo preço
  cat("Curva:", nome, "\n")
  cat("Lucro máximo: ", lucro_maximo, "\n")
  cat("Preço que maximiza o lucro: ", preco_maximo, "\n\n")
  
  # Gráficos
  plot(x, custos, type = "l", col = "red", ylim = c(min(custos, lucros, receitas), max(custos, lucros, receitas)), 
       xlab = "Quantidade", ylab = "Valor", main = paste("Gráfico dos Custos, Lucros e Receita -", nome))
  lines(x, receitas, col = "blue")
  lines(x, lucros, col = "green")
  legend("topleft", legend = c("Custos", "Receitas", "Lucros"), col = c("red", "blue", "green"), lty = 1)
  grid()
}

# Loop para processar cada curva
for (nome_curva in names(curvas)) {
  plot_curva(curvas[[nome_curva]], nome_curva)
}

# Resetar layout dos gráficos
par(mfrow = c(1, 1))
