#Ex.5. Crie um vector com 1000 número aleatórios que seguem
#distribuição normal com média 100 e desvio padrão 50. i) Determine a
#diferença entre a média dos primeiros 500 valores para os últimos 500
#valores. iii) Determine a média dos 100 maiores valores e dos 100
#menores valores (use o comando sort( ) para ordenar de forma crescente).

#i)
serie<- rnorm(1000, 100, 50)
serie

serie1<-sort(serie)
serie1
min(serie1)

serie2<- round(serie1, 2)
serie2

diferenca<-mean(serie2[1:500])
mean(serie2[501:1000])
diferenca

# ii)
media_maior <- mean(serie2[1:100])
round(media_maior,2)

media_menor <- mean(serie2[901:1000])
round(media_menor, 2)


#Functions

#Ex.6.
#i) Escreva uma função que tem como variáveis o capital
#inicial, a taxa de juro e o prazo e retorna o capital final;

#ii) Determine o capital final se aplicados R$ 1000 à taxa de juro
#anual de 5% durante 30 anos;

#iii) Experimente usar a função para determinar qual o capital final
#para três situações diferentes (a taxa de juro ser 4%, 5% ou 6%);

#i)
cc <-function(cap.ini, t.juro, prazo){
  
  resultado<-cap.ini*(1+t.juro)^prazo
  
  resultado}

#ii) Executar o comando - testar a função
cc(1000, 0.05, 30)

# ex. para a iii)
cap1 <- c(1000, 2000, 3000)
cc(cap1,0.05,5)

#iii)
cc(1000, c(0.04,0.05,0.06) ,30) 

#laço da função
cc1<- function(cap,t.juro,prazo){
              if(cap>50000) {
                t.juro<-t.juro+0.05}
              else {
                t.juro<-t.juro}
                resultado<-cap*(1+t.juro)^prazo
                resultado
}

cc1(55000, 0.005, 5)

#Função demanda partida
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






























