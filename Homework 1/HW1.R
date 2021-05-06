#=============================================== ITEM 1 ========================================================

#importando as bibliotecas usadas

library(e1071)
library(mlbench)

#importando o data set
data(Glass)
attach(Glass)

#Selecionando as 9 primeiras colunas do dataset Glass
vidro <- Glass[,(1:9)]

preditores <- colnames(vidro)

#Utilizando a fun????o mapply para aplicar a fun????o que queremos no data frame obtemos o vetor da m??dia, do desvio padr??o e da assimetria
media <- mapply(mean, vidro)
desvio_padrao <- mapply(sd, vidro) 
assimetria <- mapply(skewness, vidro)

#Juntando todos os vetores anteriormente 
tabela <- data.frame(rbind(media,desvio_padrao,assimetria))
rownames(tabela) <- c("M??dia", "Desvio Padr??o", "Assimetria") 
tabela

#Plotando os Histogramas

hist(RI, main='Histogram of Refractive Index', xlim=c(1.51,1.535), xlab='Refractive Index')
hist(Na, main='Histogram of Sodium', xlab='Sodium')
hist(Mg, main='Histogram of Magnesium', xlim=c(0,5), xlab='Magnesium')
hist(Al, main='Histogram of Aluminum', xlab='Aluminum')
hist(Si, main='Histogram of Silicon', xlim=c(69,76), xlab='Silicon')
hist(K, main='Histogram of Potassium', xlim=c(0,7), xlab='Potassium')
hist(Ca, main='Histogram of Calcium', xlim=c(4.5,17.5), xlab='Calcium')
hist(Ba, main='Histogram of Barium', xlab='Barium')
hist(Fe, main='Histogram of Iron', xlim=c(0,0.6), xlab='Iron')

#=============================================== ITEM 2 ========================================================

#Medias, desvios padr??o, assimetria e Histograma

#Classe 1
tipo1 <- vidro[(1:70),]
mean2.1 <- rep(0,9)
sd2.1 <- rep(0,9)
skew2.1 <- rep(0,9)
for(x in c(1:9)){
  p <- tipo1[,x]
  mean2.1[x] <- mean(p)
  sd2.1[x] <- sd(p)
  skew2.1[x] <- skewness(p)
  hist(tipo1[,x],xlab = "%",main = paste('Classe 1: Preditor ',x))
}

#Classe 2
tipo2 <- vidro[(71:146),]
mean2.2 <- rep(0,9)
sd2.2 <- rep(0,9)
skew2.2 <- rep(0,9)
for(x in c(1:9)){
  p2 <- tipo2[,x]
  mean2.2[x] <- mean(p2)
  sd2.2[x] <- sd(p2)
  skew2.2[x] <- skewness(p2)
  hist(tipo2[,x],xlab = "%",main = paste('Classe 2: Preditor ',x))
}

#Classe 3
tipo3 <- vidro[(147:163),]
mean2.3 <- rep(0,9)
sd2.3 <- rep(0,9)
skew2.3 <- rep(0,9)
for(x in c(1:9)){
  p3 <- tipo3[,x]
  mean2.3[x] <- mean(p3)
  sd2.3[x] <- sd(p3)
  skew2.3[x] <- skewness(p3)
  hist(tipo3[,x],xlab = "%",main = paste('Classe 3: Preditor ',x))
}

#Classe 5
tipo5 <- vidro[(164:176),]
mean2.5 <- rep(0,9)
sd2.5 <- rep(0,9)
skew2.5 <- rep(0,9)
for(x in c(1:9)){
  p5 <- tipo5[,x]
  mean2.5[x] <- mean(p5)
  sd2.5[x] <- sd(p5)
  skew2.5[x] <- skewness(p5)
  hist(tipo5[,x],xlab = "%",main = paste('Classe 5: Preditor ',x))
}

#Classe 6
tipo6 <- vidro[(177:185),]
mean2.6 <- rep(0,9)
sd2.6 <- rep(0,9)
skew2.6 <- rep(0,9)
for(x in c(1:9)){
  p6 <- tipo6[,x]
  mean2.6[x] <- mean(p6)
  sd2.6[x] <- sd(p6)
  skew2.6[x] <- skewness(p6)
  hist(tipo6[,x],xlab = "%",main = paste('Classe 6: Preditor ',x))
}

#Classe 7
tipo7 <- vidro[(186:214),]
mean2.7 <- rep(0,9)
sd2.7 <- rep(0,9)
skew2.7 <- rep(0,9)
for(x in c(1:9)){
  p7 <- tipo7[,x]
  mean2.7[x] <- mean(p7)
  sd2.7[x] <- sd(p7)
  skew2.7[x] <- skewness(p7)
  hist(tipo7[,x],xlab = "%",main = paste('Classe 7: Preditor ',x))
}

#=============================================== ITEM 3 ========================================================

pairs(Glass[,1:9],col=Glass$Type,pch=20)
glassMatrix <- data.matrix(Glass) #Transformando o data frame em matriz num??rica
glassMatrixCorr <- cor(glassMatrix)  #Criando a matriz de correla????o. 
glassMatrixCorr <- glassMatrixCorr[1:9,1:9] # Mostrando a matriz, retirando a variavel Type
corrplot(gkassMatrixCorr)
#=============================================== ITEM 4 ========================================================

#Principal Component Analysis
#Normaliza????o dos dados
zero <- rep(0,214)
mean.vidro <- matrix(c(zero, zero, zero, zero, zero, zero, zero, zero, zero), ncol = 9)
for(k in c(1,2,3,4,5,6,7,8,9)){
  mean.vidro[,k] <- c(vidro[,k]-mean(vidro[,k]))
}
#Constru????o da matriz de covari??ncia
cov.m <- cov(mean.vidro)
#Extraindo os autovetores e autovalores da matriz de covari??ncia
cov.eig <- eigen(cov.m)
eigenvalues<- data.frame(cbind(cov.eig[["values"]]))
eigenvectors <- cov.eig[["vectors"]]
rownames(eigenvalues) <- c("RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe")
#Escolhendo os preditores com maiores autovalores para serem os componentes principais
pca<-max(eigenvalues)
pca2<-max(eigenvalues[2:9])
RIm <- mean.vidro[,1]
Nam <- mean.vidro[,2]
#Gr??fico dos dados normalizados
plot(RIm,NaM, main = "Gráfico dos dados normalizados", pch=19, col = Glass$Type); abline(h=0,v=0,lty=3)
abline(a=0,b=(eigenvectors[1,1]/eigenvectors[2,1]),col="green")
abline(a=0,b=(eigenvectors[1,2]/eigenvectors[2,2]),col="blue")

feature.vector1 <- as.matrix(eigenvectors[,1],ncol=1)
feature.vector2 <- as.matrix(eigenvectors[,c(1,2)],ncol=2)

final1 <- t(feature.vector1)
final2 <- t(feature.vector2)
plot(final2[1,],final2[2,], main = "Conjunto de dados com PCA", col = Glass$Type ,xlab = "PCA2" ,ylab = "PCA1", ylim = c(-1.5, 1.5));abline(h=0,v=0,lty=3)
abline(a=0,b=(eigenvectors[1,1]/eigenvectors[2,1]),col="green")
abline(a=0,b=(eigenvectors[1,2]/eigenvectors[2,2]),col="blue")

adjust.dataset2 <- t(feature.vector2 %*% final2)
adjust.dataset2[,1] <- adjust.dataset2[,1] + mean(Glass$RI)
adjust.dataset2[,2] <- adjust.dataset2[,2] + mean(Glass$Na)
plot(adjust.dataset2[,1],adjust.dataset2[,2], main = "", col = Glass$Type); abline(h=0,v=0,lty=3)

#Obten????o dos dados finais
zero <- rep(0,214)
mean.vidro <- matrix(c(zero, zero, zero, zero, zero, zero, zero, zero, zero), ncol = 9)
for(k in c(1,2,3,4,5,6,7,8,9)){
  mean.vidro[,k] <- c(vidro[,k]-mean(vidro[,k]))
}
RowDataVector <- t(cov.eig$vectors)
FinalData <- RowDataVector%*%t(mean.vidro)

#An??lise de reten????o de PCA
library("factoextra")
eig.val <- get_eigenvalue(glass.pca)
eig.val
fviz_eig(glass.pca, addlabels = TRUE, ylim = c(0, 30), main = "Scree Plot")
