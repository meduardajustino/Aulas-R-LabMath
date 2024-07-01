#1. Selecionar valores de um vetor que sejam maiores que 3, dado:
x1<- c(1,3,5,4,7,9,2,4,5,10)

#obter valores maiores que 3
x2 <- x1[x1>3]
#print x2
print(x2)

#2. Selecionar valores onde x>3 e menores do que 9:
x3 <- x1[(x1>3)&(x1<9)]
print(x3)

#3. Selecionar valores menor do que 3 ou maiores do que 9:
x4 <- x1[(x1<3)|(x1>9)]
print(x4)

#4. Sejam: set x5 e x6
x5 <- c(1,4,6,8,12)
x6<- c(-2,-3,4,10,14)

#encontrar elementos x5 onde x6 são positivos
x7<- x5[x6>0]
print(x7)


# Joaquim Schork
'Gerar alguns. Especificar o tamanho da amostra.'

set.seed(1357531) #define sementes aleatórias para reprodutibilidade

N <- 1000
y <- rnorm(N, 2, 3)
y

x1 <- rnorm(N) #criar variáveis aleatórias
x2 <- runif(N) + 0.25 * x1
x3 <- rpois(N, 3) - 0.3 * x1 + 0.5 * x2
y <- rnorm(N, 2, 3) + 5 * x1 + 2 * x2 + 0.5 * x3


'Exercício'
'1) Plotar as funções x1, x2, x3, y'
grafx1 <- plot(x1)
grafx2 <- plot(x2)
grafx3 <- plot(x3)
grafy <- plot(y)
graficos <- plot(x1, x2)

'2) elaborar matriz de correlação entre as variáveis'
a <- cor(x1, x2)
b <- cor(x1, x3)
c <- cor(x1, y)
d <- cor(x2, x1)
e <- cor(x2, x3)
f <- cor(x2, y)
g <- cor(x3, x1)
h <- cor(x3, x2)
i <- cor(x3, y)
j <- cor(y, x1)
k <- cor(y, x2)
l <- cor(y, x3)

matrizcor <- matrix(c(1,a,b,c,d,1,e,f,g,h,1,i,j,k,l,1), nrow = 4, ncol = 4)
matrizcor

rownames(matrizcor) <- c("x1", "x2", "x3", "y")
colnames(matrizcor) <- c("x1", "x2", "x3", "y")
View(matrizcor)

'3) calcular y = f(x1, x2, x3)'
ylm <- lm(y~x1 + x2 + x3)
summary(ylm)

'4) calcular y(1); y(10); y(100)'

x11 <- 1; x22 <- 1; x33 <- 1 
#ylm1 <- intercept + cofx1*x1 + cofx2*x2 + cof3*x3
ylm1 <- 2.4243+5.0387*x1+1.4673*x2+0.4775*x3 
ylm1

x11 <- 10; x22 <- 10; x33 <- 10 
ylm10 <- 2.4243+5.0387*x11+1.4673*x22+0.4775*x33 
ylm10

x11 <- 100; x22 <- 100; x33 <- 100 
ylm100 <- 2.4243+5.0387*x11+1.4673*x22+0.4775*x33 
ylm100

'5) calcular y1 = log(y)'
y1 <- log(y)
y1

'6) Sumário de todas variáveis'
summary(x1)
summary(x2)
summary(x3)
summary(y)
