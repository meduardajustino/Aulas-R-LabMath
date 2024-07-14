'Preparar o script para a resolução da série de Taylor. Com f(n) a enésima
derivada da função complexa e de difícil "trabalho” no ponto a.
Utilizar os comando de laços.'

#função para derivadas da função exponencial
f_expo <- function(x, n) {
  return(exp(x))  
}

x <- 1
a <- 0
N <- 10  #número de termos da série

serie_taylor <- 0

# comando de laços (loop)
for (n in 0:N) {
  termo <- (f_expo(a, n) / factorial(n)) * (x - a)^n
  serie_taylor <- serie_taylor + termo
}

# para x=1
print(serie_taylor)

'Desenvolver o script para calcular as séries de Taylor para as seguintes
funções, para o entorno do ponto x = 1,1 até o 4º termo da série:'

library(ggplot2)

# calcular a série de Taylor até o n-ésimo termo
serie_taylor <- function(f, df, x0, x, n) {
  soma <- 0
  for (i in 0:n) {
    termo <- (df[[i + 1]](x0) / factorial(i)) * (x - x0)^i
    soma <- soma + termo
  }
  return(soma)
}

'a) y(x) = ex * sen(x);'
f1 <- function(x) { exp(x) * sin(x) }
df1 <- list(
  function(x) { exp(x) * sin(x) },                 # 0ª derivada
  function(x) { exp(x) * (sin(x) + cos(x)) },      # 1ª derivada
  function(x) { 2 * exp(x) * cos(x) },             # 2ª derivada
  function(x) { exp(x) * (cos(x) - sin(x)) },      # 3ª derivada
  function(x) { -2 * exp(x) * sin(x) }             # 4ª derivada
)

'b) y(x) = e-x * cos(x);'

f3 <- function(x) { 2 * exp(x) + exp(-x) }
df3 <- list(
  function(x) { 2 * exp(x) + exp(-x) },            # 0ª derivada
  function(x) { 2 * exp(x) - exp(-x) },            # 1ª derivada
  function(x) { 2 * exp(x) + exp(-x) },            # 2ª derivada
  function(x) { 2 * exp(x) - exp(-x) },            # 3ª derivada
  function(x) { 2 * exp(x) + exp(-x) }             # 4ª derivada
)

'c) Y(x) = 2*ex + e-x;'

f2 <- function(x) { exp(-x) * cos(x) }
df2 <- list(
  function(x) { exp(-x) * cos(x) },                # 0ª derivada
  function(x) { -exp(-x) * (sin(x) + cos(x)) },    # 1ª derivada
  function(x) { -2 * exp(-x) * sin(x) },           # 2ª derivada
  function(x) { exp(-x) * (sin(x) - cos(x)) },     # 3ª derivada
  function(x) { 2 * exp(-x) * cos(x) }             # 4ª derivada
)

# parâmetros
x0 <- 1.1
n <- 4

# Calcular as séries de Taylor
serie_taylor_a <- serie_taylor(f1, df1, x0, x, n)
serie_taylor_b <- serie_taylor(f2, df2, x0, x, n)
serie_taylor_c <- serie_taylor(f3, df3, x0, x, n)

cat("Série de Taylor para y(x) em x =", x0, "até o", n, "º termo:")
print(serie_taylor_a)

cat("Série de Taylor para y(x) em x =", x0, "até o", n, "º termo:")
print(serie_taylor_b)

cat("Série de Taylor para y(x) em x =", x0, "até o", n, "º termo:")
print(serie_taylor_c)

# Calcular as séries de Taylor para os gráficos
x_grafico <- seq(0, 2, length.out = 100)

taylor_f1 <- sapply(x_grafico, function(x) serie_taylor(f1, df1, x0, x, n))
taylor_f2 <- sapply(x_grafico, function(x) serie_taylor(f2, df2, x0, x, n))
taylor_f3 <- sapply(x_grafico, function(x) serie_taylor(f3, df3, x0, x, n))


# Gráficos das funções 
data_funcoes <- data.frame(
  x1 = rep(x_grafico, 3),
  y1 = c(sapply(x_grafico, f1), sapply(x_grafico, f2), sapply(x_grafico, f3)),
  group = rep(c("f1(x)", "f2(x)", "f3(x)"), each = 100)
)


ggplot(data_funcoes, aes(x = x1, y = y1, color = group)) +
  geom_line() +
  labs(title = "Funções",
       x1 = "x",
       y1 = "y") +
  theme_minimal()

# Gráficos das funções e das séries de Taylor
data_series <- data.frame(
  x = rep(x_values, 6),
  y = c(sapply(x_grafico, f1), taylor_f1, sapply(x_grafico, f2), taylor_f2, sapply(x_grafico, f3), taylor_f3),
  group = rep(c("f1(x)", "Taylor f1(x)", "f2(x)", "Taylor f2(x)", "f3(x)", "Taylor f3(x)"), each = 100)
)

ggplot(data_series, aes(x = x, y = y, color = group)) +
  geom_line() +
  labs(title = "Funções e suas séries de Taylor",
       x = "x",
       y = "y") +
  theme_minimal()


'Cálculo de integrais'
#usei função do R de integrais

# a) ∫(0 a 3) dx / (16 + x^2)
a <- integrate(function(x) 1 / (16 + x^2), 0, 3)
cat("Resultado da integral a):", a$value)

# b) ∫(1 a 2) dx / x
b <- integrate(function(x) 1 / x, 1, 2)
cat("Resultado da integral b):", b$value)

# c) ∫(2 a 3) sqrt(1 + x^2) dx
c <- integrate(function(x) sqrt(1 + x^2), 2, 3)
cat("Resultado da integral c):", c$value)

# d) ∫(0 a 2) x * sqrt(4 - x^2) dx
d <- integrate(function(x) x * sqrt(4 - x^2), 0, 2)
cat("Resultado da integral d):", d$value)

# e) ∫(2 a 3) ln(1 + x^2) dx
result_e <- integrate(function(x) log(1 + x^2), 2, 3)
cat("Resultado da integral e):", e$value)

# f) ∫(0 a 3) dx / (16 + x^2) (repetido, mesmo cálculo de a)
f <- a
cat("Resultado da integral f):", f$value)

# g) ∫(0 a 1) dx / (x + 1)
g <- integrate(function(x) 1 / (x + 1), 0, 1)
cat("Resultado da integral g):", g$value)

