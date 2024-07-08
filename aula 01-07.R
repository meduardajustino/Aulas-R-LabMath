'Sistemas lineares

Resolver sistemas de equações lineares
Resolver sistemas de Imaginemos o seguinte sistema de equações:

-4x + 0.3y = 12.3
54.3x - 4y = 45

Podemos resolvê-lo em R da seguinte forma:
'
coefs <-  matrix(c(-4, 0.3, 54.3, -4), 2, 2, byrow = T)
coefs
ys <- c(12.3, 45)
ys
solve(coefs,ys)
ys[1]
ys[2]

coeficientes <- solve(coefs, ys)
coeficientes[1]
coeficientes[2]

'Para usar os resultados da regressão obtida, os seus coeficientes, utilizar o
sistema acima, indicando a posição do termo, de acordo com a indicação das variáveis
sequenciais dos dados de entrada'

'Trabalhando com GRÁFICOS'

#Exemplo 1:
a <- 1:20
b <- a^2
plot(a, b, col="pink")

#Exemplo 2:
a <- 1:20
b <- a^2
plot(a, b, type="l", col="pink")

#Exemplo 3:
a <- 1:20
b <- a^2
plot(a, b, col="pink")
lines(rev(a), b, col="lightblue") #adição de linhas
points(a, 400-b, col="lightgreen") #adição de pontos

#Exemplo 4:
a <- 1:20; b <- a^2
plot(a, b, pch=2)
points(a, 400-b, pch = 5)
points(a, 200-b, pch = 10)

plot(0:20, 0:20, pch=0:20)


#Exemplo 5:
a <- 1:20; b <- a^2
plot(a, b, type="l", col="darkgreen")
lines(a, 2*b, lwd=4, col= "darkred")
lines(a, 0.5*b, lty=2, col="lightblue" )
lines(a, 3*b, lty=3, col="brown")
lines(a,4*b, lty=2, lwd=4, col="yellow")

#Exemplos 6:
x <- 0:20
y <- x^2
plot(c(0, 20), c(-8000, 8000), type='n', xlab=NA, ylab=NA)
lines(x, y)
lines(x, -y, col='pink')
title("Gráfico de duas funções", xlab="valores de x", ylab="valores de y")

#Exemplo 7:
#Duas séries
ano <- 2001:2009
tril1 <- c(72.8, 66.2, 69.2, 65.9, 62.4, 67.8, 61.3, 68.5, 70.4)
tril2 <- c(60.6, 53.7, 55.3, 56.7, 56.4, 57.8, 57.5, 59.8, 63.3)
plot(ano, tril1, type="l", main="Taxa de ocupação por trimestre dos hotéis - Município
     do Rio de Janeiro", xlab="ano", ylab="Taxa de ocupação %", col="darkblue",
     ylim=c(50,80)) #limite do range de y
lines(ano, tril2, col="darkred")

#Exemplo 8:
'Regressão linear com Gráficos'

x <- c(18,23,25,35,65,54,56,72,19,23,42,18,39,37)
y <- c(202, 186, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178)

plot(x,y)
abline(lm(y ~ x)) #gráfico da regressão linear
lm(formula = y ~x)
lmregress <- lm(y~x)
summary(lmregress)
lmregress[1]
lmregress[2]

#Exemplo 9:
'Através do parÂmetro "plot.type" podemos indicar que pretendemos um só gráfico com
todas as séries'

m <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 6), frequency = 12)
m
plot(m, plot.type = "single", col = 1:3)
legend("topright", legend = colnames(m), col = 1:3, lty = 1)
















