'Gráficos especiais - 1'
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=4)
c1
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=4)
c2

mydata <- rbind(c1, c2)
mydata
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
mydata
with(mydata,
     plot(x, y, pch=19, main="Scatter Plot with 10.000 Observations"))

with(mydata,
     smoothScatter(x, y, main="Scatter Plot Colored by Smoothed Densities"))

'Gráficos especiais - 2'
install.packages("hexbin")
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins=50)
  plot(bin, main="Hexagonal Binning with 10.000 Observations")
})


'Gráficos especiais - 3'
install.packages("scatterplot3d")
library(scatterplot3d)
mtcars
attach(mtcars)
scatterplot3d(wt, disp, mpg,
              main="Basic 3D Scatter Plot")

'Gráficos especiais - 4'
library(scatterplot3d)
attach(mtcars)    #anexa o dataframe mtcars
scatterplot3d(wt, disp, mpg,
              pch=16,
              highlight.3d=TRUE,
              type="h",
              main="3D Scatter Plot with Vertical Lines")


'SLIDE 2'
library(ggplot2)
data("mtcars")
summary(mtcars)
head(mtcars)
tail(mtcars)

'SLIDE 2
inicia o plot
mpg: Consumo de combustível (milhas por galão).
cyl: Número de cilindros.
disp: Deslocamento do motor (polegadas cúbicas).
hp: Potência do motor (cavalos de potência).
drat: Taxa de eixo traseiro.
wt: Peso do carro.
qsec: Tempo de aceleração de 1/4 de milha.
vs: Motor (0 = V-shaped, 1 = straight).
am: Transmissão (0 = automática, 1 = manual).
gear: Número de marchas.
carb: Número de carburadores.'
g <- ggplot(mtcars)

'adicionar pontos (geom_point) e vamos mapear variáveis a elementos
estéticos dos pontos. Size = 3 define o tamanho de todos os pontos'

g <- g + 
  geom_point(aes(x = hp, y = mpg, color=factor(am)),
             size=3)
g

#alterar a escala de cores
g <- g +
  scale_color_manual("Automatic",
                     values=c("darkred", "darkgreen"),
                     labels=c("No", "Yes"))

#rótulos(títulos)
g <- g + # anterior mais uma qualificação
  labs(title = 'Relação entre consumo, potência e tipo de câmbio',
       y = 'Consumo',
       x = 'Potência')
g

'SLIDE 6
criar um fráfico com pontos a partir dos dados mtcars'

g1 <- ggplot(mtcars, aes(y=mpg, x=disp)) +
  geom_point()

g1

ggplot(mtcars, aes(y = mpg, x = disp)) +
  geom_point() +
  geom_smooth
















