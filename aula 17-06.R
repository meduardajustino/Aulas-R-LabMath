#Ex. 7 - Função demanda partida
# se a função 0<=x<=100; y = 1000-0.20*x
#se a função 101<=x<=200; y2 = 1050-0.21*x
#se a função 201<=x<=300; y3 = 1090-0.22*x

x<- 0:100
y1<- 1000-0.20*x
y1
plot(y1)

x<-101:200
y2<- 1050-0.21*x
y2
plot(y2)

x<-201:300
y3<-1090-0.22*x
y3
plot(y3)

yfinal<- c(y1,y2,y3)
yfinal
plot(yfinal)

#Ex.8 - Elaborar o gráfico da curva de demanda, “quebrada”, sabendo-se que:
#para o caso de considerar o coeficiente angular aleatório com uma distribuição
#normal com b1= média de 20 e desvio padrão de 2; b2 = média de 21 e desvio
#padrão de 2 e b3 = média de 22 e desvio padrão de 2. Com 100 possibilidades de
#simulação.

# aleatorio = coeficiente angular
#rnorm - função de Gauss
aleatorio1<- rnorm(100, 0.20, 0.2) #coeficiente angular da reta com média 20 e desv pad de 2
aleatorio1

aleatorio2<- rnorm(100, .21, .2) #coeficiente angular da reta com média 21 e desv pad de 2
aleatorio2

aleatorio3<- rnorm(100, .22, .2) #coeficiente angular da reta com média 22 e desv pad de 2
aleatorio3


x=1:100
y1 <- (1000-aleatorio1*x)
y1


x=101:200
y2 <- (1050-aleatorio2*x)
y2

x=201:300
y3 <- (1090-aleatorio3*x)
y3

y <- c(y1, y2, y3)
plot(y)


y11<- sort(y1, decreasing = T)

y22<- sort(y2, decreasing = T)

y33<- sort(y3, decreasing = T)

yy <- c(y11,y22,y33)

plot(yy)

hist(aleatorio1, col = 2) # red
hist(aleatorio2, col = 4) # blue
hist(aleatorio3, col = 2) # green


#EX. 9
#Se 0<=x<=500 a demanda é y = 1000;
#Se 500<x<=1.000 a demanda é y = 500;
#Se 1.000<x<=1.500 a demanda é y = 200;
#Se 1.500<x<=2.000 a demanda é y = 100.

y1=0; y2=0; y3= 0; y4 = 0; y=0
  for (i in 1:500){
    #x=1:500
    y1[i] <- 1000
  }

  for(i in 1:500){
    y2[i] = 500 #x=1:500
  }
  
  for(i in 1:500){
    y3[i] = 200 #x=1:500
  }

  for(i in 1:500){
    y4[i] = 100 #x=1:500
  }


y=c(y1,y2,y3,y4)
plot(y);y




