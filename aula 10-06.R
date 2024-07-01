#Ex.1. Emprestei R$1000 a uma taxa anual de 5%/ano. Quanto dinheiro 
# receberei ao fim de 10 anos (capitalização composta)?

Taxa.de.juro.anual<-0.05

Capital.inicial<-1000

Prazo<-10

Capital.final<-Capital.inicial*(1+Taxa.de.juro.anual)^Prazo

Capital.final

#Ex.2. Um banco personaliza as taxas de juro dos depósitos dos seus
#clientes. i) Crie, para 10 clientes, um hipotético vector de taxas de
#juro, um vector de saldos e calcule os juros a pagar a cada cliente. ii)
#Capitalize esses saldos com a taxa de juro respectiva a 5 anos.

Tx.juros<- c(4.5, 5.1, 4, 3.6, 3, 5, 4.6, 4.8, 3.6, 5)/100

Saldos<- c(10, 150, 45, 20, 100, 75, 15, 67, 9, 2)*1000

Juros <- Saldos*Tx.juros

#ii)
Capital.final<- Saldos*(c(1,1,1,1,1,1,1,1,1,1) + Tx.juros)^5


#Ex.3. Quero saber, em função do prazo, qual é o capital final de
#emprestar R$ 1000 a uma taxa anual de 4%. Experimente 1, 2, 5 e 10
#anos.

Anos <- c(1, 2, 5, 10)

Tx.juro.anual<-0.04

Capital.final <- 1000*(1+Tx.juro.anual)^Anos

Capital.final

#Ex.4. Num investimento, apliquei 1000€ e recebi 250€, 350€, 450€ a
#intervalos de um ano. Sendo que a taxa de desconto é de 4.5% ao ano,
#qual o Valor presente deste investimento?
  
  Cash.flow<-c(-1000, 250, 350, 450)

Taxa.de.desconto<-0.045

Desconto<-(1+Taxa.de.desconto)^-(0:3)

FC<-Cash.flow*Desconto

FC

(VPL<-sum(FC))


#Ex.5. Crie um vector com 1000 número aleatórios que seguem
#distribuição normal com média 100 e desvio padrão 50. i) Determine a
#diferença entre a média dos primeiros 500 valores para os últimos 500
#valores. iii) Determine a média dos 100 maiores valores e dos 100
#menores valores (use o comando sort( ) para ordenar de forma crescente).

#i)
serie<-rnorm(1000, mean=100, sd=50)

#ii)
diferenca<- mean(serie[1:500])-mean(serie[501:1000])

diferenca

#iii)
ordenados<-sort(serie)

media.menores <- mean(ordenados[1:100])

media.maiores<- mean(ordenados[901:1000])

media.menores

media.maiores


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

#ii) Executava o comando
cc(1000, 0.05, 30) #e resultava [1] 4321.942.

#iii)
cc(1000, c(0.04,0.05,0.06) ,30) #e resultava [1] 3243.398 4321.942 5743.491.

#iv) acrescente uma condição de forma que, se o capital for
#maior que R$ 50.000, a taxa de juro é maior em 0.5pp e acrescente na
#saída a taxa de juro. Experimente a função.

#iv)
cc <-function(cap.ini, t.juro, prazo){
  
  if(cap.ini < 50000) tx<- t.juro
  
  else tx<-t.juro+0.005
  
  cap.final<-cap.ini*(1+tx)^prazo
  
  resultado<-c(cap.ini, tx, prazo, cap.final)
  
  names(resultado)<-c("Cap.ini", "Tx.juro", "Prazo", "Cap.final")
  
  resultado}

cc <- function(cap.ini, t.juro, prazo) {
  
  if(cap.ini < 50000) {
    tx <- t.juro
  } else {
    tx <- t.juro + 0.005
  }
  
  cap.final <- cap.ini * (1 + tx)^prazo
  
  resultado <- c(cap.ini, tx, prazo, cap.final)
  
  names(resultado) <- c("Cap.ini", "Tx.juro", "Prazo", "Cap.final")
  
  resultado
}


#ANALISAR O ERRO, ONDE ESTÁ??!!!!!

#Ex.7. Elaborar o gráfico da curva de demanda, “quebrada”,
#sabendo-se que:
  #Se 0<=x<=100 a demanda é y = 1000 – 0.20*x;
  #Se 100<x<=200 a demanda é y = 1050 – 0.21*x;
  #Se 200<x<=300 a demanda é y = 1090 – 0.22*x.

x=1:100

y1<- (1000-0.20*x)

x=101:200

y2<- (1050-0.21*x)

x=201:300

y3<- (1090-0.22*x)

y=c(y1,y2,y3)

plot(y)