'QUESTÃO 1'

'Tabela apresenta a evolução de tais indicadores no Brasil, ao longo do período de 1970 a 1995:'
tabela <- data.frame(
  ano = c(1970, 1975, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995),
  depositos_totais = c(312.0, 381.5, 347.4, 404.2, 402.1, 452.0, 431.7, 582.3, 596.6, 620.8, 513.6, 606.9, 629.0, 602.7, 656.7, 678.5, 637.6, 698.2),
  PIB = c(33027, 105962, 191842, 212187, 222354, 223354, 245104, 273949, 303496, 323736, 335923, 362286, 361909, 376089, 379411, 384591, 395478, 480361),
  populacao = c(93139037, 105279615, 119002706, 121304828, 124132901, 126932107, 129881714, 130964997, 132744121, 135682832, 138506432, 141596301, 146917459, 147489931, 150474909, 153390844, 155608189, 158617875),
  renda_per_capita = c(355, 1006, 1961, 1749, 1791, 1760, 1887, 2092, 2286, 2386, 2425, 2559, 2463, 2550, 2521, 2507, 2541, 3028)
)

View(tabela)

'Função de Depósitos Totais previstos'
funcao_DT <- function(PIB, populacao, renda_per_capita) {
  447.01 + 1.87 * PIB - 0.22 * populacao - 0.75 * renda_per_capita
}

'Valores previstos'
tabela$depositos_previstos <- mapply(funcao_DT, tabela$PIB, tabela$populacao, tabela$renda_per_capita)
View(tabela)

'QUESTÃO 2
Crie um vector com 1000 número aleatórios que seguem distribuição normal com média 500
e desvio padrão 5.'
set.seed(21) # usei a semente 21 para reprodutibilidade
N <- 1000
y <- rnorm(N, 500, 5)
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

'QUESTÃO 3
Calcular o determinante da Matriz dos Coeficientes
7x - 3y – 3z = 7
2x + 4y + z = 0
– 2y - z = 2'

matrizA <- matrix(c(7, -3, -3,
                   2, 4, 1,
                   0, -2, -1),
                  nrow = 3, byrow = TRUE)

coefs <- c(7, 0, 2)

determinante <- det(matrizA)
determinante

#para fins didáticos:  resolver o sistema linear
solucao <- solve(matrizA, coefs)
solucao

'QUESTÃO 4
Para estimar a diferença de tempos médios de vida (em anos) entre fumantes e não
fumantes, foram recolhidas obtidos as seguintes observações. 
Fumantes
52,4 55,0 55,2 55,2 55,5 56,2 57,0 57,4 58,3 58,4 59,2 59,3 59,6 59,7 60,0 60,5 60,6 61,2 61,6
61,9 62,1 62,2 62,4 62,7 63,5 64,1 64,7 64,8 64,9 65,0 65,4 66,0 66,9 69,1 69,2 69,8

Não Fumantes
63,8 65,7 66,2 66,2 66,2 66,8 67,5 67,7 67,9 68,0 68,1 68,3 68,6 68,6 68,7 68,8 68,8 69,2 69,3
69,4 69,5 70,1 70,1 70,2 70,2 70,3 70,4 70,7 70,8 70,8 71,0 71,4 71,5 71,6 72,7 72,7 72,9 73,3
73,3 73,9 74,1 75,8 75,9 77,5'

'a) Obtenha o intervalo de confiança a 95% para tempo médio de vida fumantes e não fumantes;'
fumantes <- c(52.4, 55.0, 55.2, 55.2, 55.5, 56.2, 57.0, 57.4, 58.3, 58.4, 59.2, 59.3, 59.6, 59.7, 60.0, 60.5, 60.6, 61.2, 61.6,
              61.9, 62.1, 62.2, 62.4, 62.7, 63.5, 64.1, 64.7, 64.8, 64.9, 65.0, 65.4, 66.0, 66.9, 69.1, 69.2, 69.8)

nao_fumantes <- c(63.8, 65.7, 66.2, 66.2, 66.2, 66.8, 67.5, 67.7, 67.9, 68.0, 68.1, 68.3, 68.6, 68.6, 68.7, 68.8, 68.8, 69.2, 69.3,
                  69.4, 69.5, 70.1, 70.1, 70.2, 70.2, 70.3, 70.4, 70.7, 70.8, 70.8, 71.0, 71.4, 71.5, 71.6, 72.7, 72.7, 72.9, 73.3, 73.3,
                  73.9, 74.1, 75.8, 75.9, 77.5)

'Função para calcular o intervalo de confiança a 95%'
calc_ic <- function(dados) {
  n <- length(dados)
  media <- mean(dados)
  desvio_padrao <- sd(dados)
  erro_padrao <- desvio_padrao / sqrt(n)
  z <- qnorm(0.975) # valor z para 95% de confiança
  margem_erro <- z * erro_padrao
  intervalo <- c(media - margem_erro, media + margem_erro)
  return(intervalo)
}

ic_fumantes <- calc_ic(fumantes)
ic_fumantes
cat("Intervalo de confiança para fumantes:", ic_fumantes)

ic_nao_fumantes <- calc_ic(nao_fumantes)
cat("Intervalo de confiança para não fumantes:", ic_nao_fumantes)


'b) Por meio do intervalo de confiança é possível dizer que não fumantes vivem mais que
fumantes?'
ic_fumantes[2] - ic_nao_fumantes[1]
print("As evidências estatísticas comprovam que os não fumantes vivem significativamente mais do que fumantes,
      visto que o limite superior do intervalo de confiança do tempo médio de vida dos fumantes é inferior ao intervalo mínimo dos não fumantes.")

'QUESTÃO 5
Escreva uma função (UTILIZAR O FUNCTION EM R) que tem como variáveis
uma série de entradas de capital, a taxa de desconto e o prazo; e retorna o valor
presente da operação;'

'a) ii) Determine o valor presente considerando uma série de entrada nos valores R$ 100, 
$ 200 R$ 300 R$ 400 R$ 500 R$ 600, com taxa de desconto anual de 12% a.a.;'

func_valor_presente <- function(entradas, taxa, prazo) {
  valor_presente <- 0
  for (i in 1:prazo) {
    valor_presente <- valor_presente + entradas[i] / (1 + taxa)^i
  }
  return(valor_presente)
}

'b) iii) Experimente usar a função para diferentes taxas de descontos: 4%, 5% ou 6%;'
entradas <- c(640, 350, 580, 210, 110)
prazo <- length(entradas)

# taxa de desconto anual de 4%
taxa_4 <- 0.04
valor_presente4 <- func_valor_presente(entradas, taxa_4, prazo)
cat("Valor presente com taxa de desconto de 4% a.a.:", valor_presente4)

# taxa de desconto anual de 5%
taxa_5 <- 0.05
valor_presente5 <- func_valor_presente(entradas, taxa_5, prazo)
cat("Valor presente com taxa de desconto de 5% a.a.:", valor_presente5)

# taxa de desconto anual de 6%
taxa_6 <- 0.06
valor_presente6 <- func_valor_presente(entradas, taxa_6, prazo)
cat("Valor presente com taxa de desconto de 6% a.a.:", valor_presente6)

# taxa de desconto anual de 10%
taxa_10 <- 0.1
valor_presente10 <- func_valor_presente(entradas, taxa_10, prazo)
cat("Valor presente com taxa de desconto de 10% a.a.:", valor_presente10)

'QUESTÃO 6
Utilize o comando “for” e elabore o algoritmo para calcular a seguinte expressão definida
pela função F(x), indicada abaixo. (Faça para x= 5) dos 1000 primeiros termos:
F(x) = x – x 2 /(2*2!) – x 3 /(3*3!) + x 4 /(4*4!)+ x 5 /(5*5!)-...,'

fatorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    result <- 1
    for (i in 1:n) {
      result <- result * i
    }
    return(result)
  }
}

# F(x) 
F <- function(x, num) {
  resultado <- 0
  sinal <- 1
  
  for (n in 1:num) {
    termo <- sinal * x^n / (n * fatorial(n))
      if(is.nan(termo)){
        break
      }
    resultado <- resultado + termo
    sinal <- sinal * (-1) 
  }
  return(resultado)
}

x <- 5
num <- 1000

resultado <- F(x, num)
resultado
cat("O valor de F(",x,") com os", num, "primeiros termos é:", resultado)





