#SLIDE 2
library(ggplot2)
data("mtcars")
summary(mtcars)
head(mtcars)

#SLIDE 3

# Inicia o plot
g <- ggplot(mtcars)

# Adicionar pontos (geom_point) e
# vamos mapear variáveis a elementos estéticos dos pontos
# Size = 3 define o tamanho de todos os pontos
g <- g +
  geom_point(aes(x = hp, y = mpg, color = factor(am)),
             size = 3)
g
	
# am - binário, se o carro é automático ou manual

#SLIDE 4

# Altera a escala de cores
g <- g +
  scale_color_manual("Automatic",
                     values = c("red", "blue"),
                     labels = c("No", "Yes"))

# Rótulos (títulos)
g <- g +
  labs(title = 'Relação entre consumo, potência e tipo de câmbio',
       y = 'Consumo',
       x = 'Potência')

g

#Note que o gráfico poderia ser criado com um bloco único de código:

ggplot(mtcars) +
  geom_point(aes(x = hp, y = mpg, color = factor(am)),
             size = 3) +
  scale_color_manual("Automatic",
                     values = c("red", "blue"),
                     labels = c("No", "Yes")) +
  labs(title = 'Relação entre consumo, potência e tipo de câmbio',
       y = 'Consumo',
       x = 'Potência')
       
#SLIDE 6

#criar um gráfico com pontos a partir dos dados mtcars
       
g1 <- ggplot(mtcars, aes(y = mpg, x = disp)) +
              geom_point()

g1
####teste:

ggplot(mtcars, aes(y = mpg, x = disp)) +
  geom_point() +
  geom_smooth()
######

#SLIDE 7

data("iris")
head("iris")
summary(iris)
iris
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point()
  
#SLIDE 8

###é comum haver interesse em alterar-se essas cores, ou seja, alterar-se a escala de cor. Como fazer isso no ggplot2? Podemos usar, por #exemplo, a função scale_color_manual(): 
  
  ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = c("orange", "black", "red"))

#################################
  
#SLIDE 9
  
install.packages("ISLR")  
library(ISLR)
  
## Warning: package 'ISLR' was built under R version 3.5.2

  data(Wage)
head(Wage)
###  	•	Dados salariais na região do Média Atlântico: região dos Estados Unidos que abrange os Estados de Nova Iorque, Nova ##Jérsei, Pensilvânia, Delaware, Maryland, Washington D.C., e às vezes Virginia e West Virginia.

##Dados sobre salários e outros dados para um grupo de 3000 trabalhadores do sexo masculino na região do Médio Atlântico.
ggplot(Wage, aes(x = age, y = wage, color = education)) +
  geom_point() +
  scale_x_continuous("Idade", breaks = seq(0, 80, 5),
                     expand = c(0, 5)) +
  scale_y_continuous("Salário", labels = function(x) paste0("US$ ", x),
                     limits = c(0, 400))
##################################

#SLIDE 10

#Variáveis de Datas
#Quando estamos trabalhando com séries temporais, é comum que datas sejam #associadas a algum eixo do grafico, geralmente ao eixo x. As funções padrão para #controle de escalas dos eixos, para variáveis de datas, são as seguintes:

data(economics) 
head(economics)
summary(economics)
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() 

#	•	Agora, suponha que queremos alterar o gráfico para o formato “Jan/1970”:
#	•	O **scale_ _date** é utilizado para variáveis do tipo Date e *scale_ _datetime* para variáveis do tipo POSIXct. A classe POSIXct acea # #informações relacionadas a tempo/horário e a classe Date aceita apenas dia, mês e ano.
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line() +
  scale_x_date(date_labels = "%b/%Y")
  
 #	•	O %b/%Y é usado para definir-se o formato de data desejado. Para ver a lista de formatos, use help(strptime).
 #################################
 
#SLIDE 11

#Subplots (facet)
	#•	O ggplot2 facilita a criação de subplots nos casos em que se deseja replicar o mesmo gráfico para um conjunto de valores de outra #variável. Por exemplo, criar um gráfico da série temporal de desemprego para cada unidade da federação. As duas principais funções #são facet_wrap() e facet_grid().
	#•	Antes de mais nada, vamos criar um exemplo para o facet_wrap():

data("diamonds")
head(diamonds)
summary(diamonds) 

	#•	Um conjunto de dados contendo os preços e outros atributos de quase 54.000 diamantes.
	#•	carat: peso do diamante (0,2-5,01)
	#•	price: preço em dólares americanos
	#•	cut: qualidade do corte (Justo (Fair), Bom (Good), Muito Bom (Very Good), Premium (Premium), Ideal (Ideal))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

'	•	E se o objetivo for comparar essas relações para diferentes grupos de lapidação?
	•	'

ggplot(diamonds, aes(x = carat, y = price)) +
		geom_point() +
	facet_wrap(~ cut)

##################################

#SLIDE 12

	#•	Já o uso do facet_grid() é indicado para o cruzamento de variáveis. No exemplo abaixo, a relação entre as variáveis price e carat será #“quebrada” para grupos formados pelas variáveis cut e clarity (clarity: uma medida da clareza do diamante (I1 (pior), SI1, SI2, VS1, VS2, #VVS1, VVS2, IF (melhor))):
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(clarity ~ cut)
  
#SLIDE 13

 #Legendas
	#•	Parte das alterações das legendas pode ser feita via theme().
	#•	Essas alterações são gerais para todas as legendas. Se o interesse for em mudanças pontuais na legenda de algum elemento estético, serão utilizadas as funções guides(), guide_legend() e guide_colorbar().
 ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = cut)) +
  geom_point() +
  guides(color = guide_legend(title = "Cor", title.position = "left", keywidth = 5),
         shape = guide_legend(title = "Forma", title.position = "right", override.aes = aes(size = 5)))
 #sem forma
 ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = cut)) +
  geom_point() +
  guides(color = guide_legend(title = "Cor", title.position = "left", keywidth = 5),
         shape = "none")
 
 #SLIDE 14
 
 
  #Gráfico de Dispersão (geom_point())
	#•	O gráfico de dispersão é bastante usado para verificar-se relações entre duas variáveis quantitativas. Para exemplificar, utilizaremos a #base disponível no pacote gapminder. Nesta base, existe uma variável de expectativa de vida e outra de renda per capita.
#Como queremos um gráfico de pontos, o objeto geométrico natural é o geom_point().

install.packages("gapminder")  
require(gapminder)
## Loading required package: gapminder
## Warning: package 'gapminder' was built under R version 3.5.2
data(gapminder)
head(gapminder)
summary(gapminder)

#SLIDE 15

gapminder1 <- subset(gapminder, year==max(year))

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita",
       y = "Expectativa de Vida")



#	•	Note que a expectativa de vida cresce muito rápido para níveis de renda baixos, mas o incremento decresce conforme o nível de renda #aumenta. Esse fato pode ser melhor representado utilizando-se uma escala logarítmica para a variável renda per capita.
#É assim que, usualmente, essa relação é apresentada. Para isso, poderíamos aplicar a função log10() na variável de renda per capita, ou #utilizarmos a função scale_x_log10():

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  scale_x_log10() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")
	
#SLIDE 16

	'	•	Com essa escala, valores incrementados na ordem de dez vezes serão igualmente espaçados. Nesse caso, a relação parece ser mais linear, ou seja, ao aumentarmos a renda dez vezes, espera-se que a expectativa de vida cresça a uma taxa constante.
	•	Vamos mapear a variável continent ao elemento estético color e shape:'

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp, 
             color = continent, shape = continent)) +
  geom_point() + 
  scale_x_log10() +
  scale_color_discrete("Continente") +
  scale_shape_discrete("Continente") +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")

#SLIDE 17


	'•	Automaticamente o ggplot2 criou uma escala para as cores e formatos dos pontos. O usuário pode alterar este mapeamento utilizando as funções *scale_ _*.
	•	Perceba que os formatos de 21 a 24 possuem preenchimento (fill). Assim, no código abaixo definiremos o preenchimento, o tamanho do ponto e a espessura para aqueles formatos que possuem contornos.'

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp, 
             color = continent, shape = continent)) +
  geom_point(fill = "black", size = 3, stroke = 1) + 
  scale_x_log10() +
  scale_color_discrete("Continente") +
  scale_shape_manual("Continente", values = c(19, 21, 22, 23, 24)) +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")
########################################

#SLIDE 18

'Gráficos de Bolhas
	•	O gráfico de bolha é uma extensão natural do gráfico de pontos.
	•	Ele permite observar-se possíveis relações entre as três variáveis. Para este tipo de gráfico, são necessárias três variáveis: duas para indicarem as posições x e y e uma terceira para definir o tamanho do ponto (size). Vamos utilizar a variável pop (população):'

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp, 
             size = pop)) +
  geom_point() + 
  scale_size_continuous("População (milhões)", 
                        labels = function(x) round(x/1000000)) +
  scale_x_log10() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")

#SLIDE 19

'Gráficos de Barras
	•	Os gráficos de barras/colunas são geralmente utilizados para comparações entre categorias (variáveis qualitativas). No ggplot2 podemos usar dois objetos geométricos distintos: geom_bar() e geom_col:'

ggplot(diamonds, aes(x = cut)) +
  geom_bar()

  '	•	Usando o geom_col():'
gapminder2 <- subset(gapminder, continent == "Americas" & year==2007)

ggplot(gapminder2, aes(x = country, y = lifeExp)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Expectativa de vida por país",
       subtitle = "2007",
       x = "País",
       y = "Anos")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#SLIDE 20

'	•	Uma pergunta recorrente é: Como ordenar as barras em ordem crescente/decrescente? Para isso, pode-se utilizar a função reorder() no momento do mapeamento. Fica mais claro com um exemplo:'
ggplot(gapminder2, aes(x = reorder(country, -lifeExp), y = lifeExp)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Expectativa de vida por país",
       subtitle = "2007",
       x = "País",
       y = "Anos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#SLIDE 21

'Gráficos de linhas
Os gráficos de linhas são, geralmente, utilizados para apresentar-se a evolução de uma variável quantitativa em um intervalo de tempo.'

gap4 <- aggregate(lifeExp ~ continent + year, data=gapminder, FUN=mean)

ggplot(gap4, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  labs(title = "Evolução da expectativa de vida por continente",
       x = "Ano",
       y = "Anos de vida",
       color = "Continente")

'	•	É bastante comum que gráficos de linhas apresentem marcações para os períodos em que realmente existem os dados. Para isso, podemos adicionar uma camada de pontos:'

ggplot(gap4, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  geom_point(aes(shape = continent))  +
  labs(title = "Evolução da expectativa de vida por continente",
     x = "Ano",
     y = "Anos de vida",
     color = "Continente",
     shape = "Continente")+
  theme_bw()


#SLIDE 22


'Histogramas e freqpoly
Os histogramas são utilizados para representar-se a distribuição de dados de uma variável quantitativa em intervalos contínuos.
	•	Esses intervalos são chamados de bins. Para cada bin, será apresentada a quantidade de valores que estão naquele intervalo.
	•	A diferença para o geom_freqpoly é que este utiliza linhas para construir polígonos, enquanto o geom_histogram utiliza barras.
	•	Conforme a documentação do ggplot2, o geom_histogram() utiliza os mesmos elementos estéticos do geom_bar(). Já o geom_freqpoly() utiliza os mesmo do geom_line().'

gap5 <- subset(gapminder, year %in% 2007)

ggplot(gap5, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, fill = 'dodgerblue',
                 color = 'black') +
  labs(title = "Distribuição da expectativa vida",
       x = "Anos",
       y = "Frequência") +
  theme_light()

gap5 <- subset(gapminder, year %in% 2007)

 ggplot(gap5, aes(x = lifeExp)) +
  geom_freqpoly(binwidth = 5) +
  labs(title = "Distribuição da expectativa vida",
       x = "Anos",
       y = "Frequência")+
  theme_light()
	
	'•	Transformando em proporção:'

ggplot(gap5, aes(x = lifeExp)) +
  geom_histogram(aes(y = ..count../sum(..count..)),
                 binwidth = 5, fill = 'dodgerblue', color = 'black') +
  labs(title = "Distribuição da expectativa vida",
       x = "Anos",
       y = "Proporção") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()

#SLIDE 23

'Boxplots
	•	O boxplot é uma representação comum para apresentar-se a distribuição de uma variável a partir de seus quantis.
	•	O boxplot também pode ser usado para verificar-se a distribuição de variável para um conjunto de valores de uma segunda variável. Por exemplo: qual é a distribuição da expectativa de vida por ano?'

ggplot(gapminder, aes(x = factor(year), y = lifeExp)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(y = "Anos de vida",
       x = "Ano",
       title = "Distribuição da expectativa de vida por ano") 

#SLIDE 24

#Using R do Departamento de Ecologia da Universidade de São Paulo.

#SLIDE 25
########################################
install.packages("ggplot2")

library(ggplot2)
library(grid)

esaligna <- read.csv("http://ecologia.ib.usp.br/bie5782/lib/exe/fetch.php?media=dados:esaligna.csv")

esaligna

# Criar o plot e salvar no objeto 'p'
p <- ggplot(esaligna, aes(x = dap, y = ht)) + 
  geom_point() +
  xlab("diâmetro na altura do peito (cm)") +
  ylab("altura do tronco (m)")

# Execute 'p' para visualizar:
p

#SLIDE 26

# Boxplot
p_box <- ggplot(esaligna, aes(factor(talhao), dap)) +
  geom_boxplot() +
  xlab("talhao")
  
# Para criar o segundo gráfico, vamos primeiro criar um data.frame com as médias
# e desvios padrão para cada talhão:


dat_por_talhao <- data.frame(talhao = sort(unique(esaligna$talhao)), 
                             media = with(esaligna, tapply(dap, talhao, mean)), 
                             sd = with(esaligna, tapply(dap, talhao, sd)))

dat_por_talhao

#SLIDE 27

# Agora vamos criar o gráfico:
p_media_desvio <- ggplot(dat_por_talhao, aes(factor(talhao), media)) + 
  geom_point(stat = "identity", size = 5) + 
  geom_errorbar(aes(ymin = media - sd, ymax =  media + sd), width = 0.5) +
  xlab("talhao")
##########################





