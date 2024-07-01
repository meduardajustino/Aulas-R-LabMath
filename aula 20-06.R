# AULA DO DIA 20/06
  
# MATRIZES
matrix(c(1,5,38,400), 2, 2)

matrix(1:6, 2, 3)

matrix(rnorm(9), 3, 3)

matrix(c("a","c","b","j"), 2, 2)

matriz<-matrix(c(1,1,1,2,2,2),2,3)
matriz

colnames(matriz)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matriz)<-c("Linha 1","Linha 2")
matriz

matriz1<-matrix(c(0.1,0.1,0.3,0.4,0.5,0.6),2,3)
colnames(matriz1)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matriz1)<-c("Linha 1","Linha 2")
matriz1

#IPTU
iptu=0.20
iptu
#MATRIZ IPTU
matriziptu=matriz*iptu
colnames(matriziptu)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matriziptu)<-c("Linha 1","Linha 2")
matriziptu

#MATRIZ AGREGADA = Multiplica elemento por elemento de cada matriz e vetor
matriz2=matriz*matriz1*iptu
matriz2
colnames(matriz2)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matriz2)<-c("Linha 1","Linha 2")

#MATRIZ DEMANDA
matrizdemanda<-matrix(c(10,20,25,30,33,40),2,3)
colnames(matrizdemanda)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matrizdemanda)<-c("Linha 1","Linha 2")
matrizdemanda

#MATRIZ PREÇO
matrizpreço<-matrix(c(10,20,30,40,50,60),2,3)
colnames(matrizpreço)<-c("Coluna 1","Coluna 2","Coluna 3")
rownames(matrizpreço)<-c("Linha 1","Linha 2")
matrizpreço

#RECEITA
R=matrizdemanda*matrizpreço
R


# matrix  algumas operações e funções

# A * B <-  multiplicação elemento a elemento
# A %*% B <-  multiplicação matricial
# 
# t(A) <- transposta
# 
# diag(n)  matriz identidade de dimensão n
# 
# solve(A) <- inversa
# solve(A, b) <- devolve o vetor x na equação b = Ax
# 
# rowMeans(A) <-  devolve o vector das médias por linha
# rowSums(A) <- devolve o vector das somas por linha
# colMeans(A) <- devolve o vector das médias por coluna
# colSums(A) <- devolve o vector das somas por colunamatriz e vetor

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



