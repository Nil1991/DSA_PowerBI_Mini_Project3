# Definindo a pasta de trabalho
install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Instalando os pacotes para o projeto
install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages('e1071', dependencies=TRUE)

#Carregando os pacotes
library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

#Carregando o dataset
#Fonte: https://archive.uci.uci.edu/ml/datasets/defaut+of+credit+card+clients
dados_clientes <- read.csv("dados/dataset.csv")

# Visualizando os dados e sua estrutura
#View(dados_clientes)
#str(dados_clientes)
#sumary(dados_clientes)
#dim(dados_clientes)

############## Análise Exploratória, Limpeza e Transformação #########

#Removendo a primeira coluna ID
dados_clientes$ID <- NULL
#dim(dados_clientes)
#View(dados_clientes)

#Renomeando a coluna de target
#colnames(dados_clientes)
colnames(dados_clientes) [24] <- "Inadimplente"
#colnames(dados_clientes)
#View(dados_clientes)

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "valores Missing observados")
dados_clientes <- na.omit(dados_clientes) #omitir valores ausentes

# Renomeando colunas categóricas
#colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
#colnames(dados_clientes)

#Genero (acrescentando label para os valores binários - masculino e feminino)
#summary(dados_clientes$Genero)
dados_clientes$Genero <- cut(dados_clientes$Genero, c(0,1,2), labels = c("Masculino", "Feminino"))
#View(dados_clientes)
#str(dados_clientes$Genero)
#summary(dados_clientes$Genero)

#Escolaridade
#str(dados_clientes$Escolaridade)
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade, c(0,1,2,3,4), labels = c("Pos Graduado", "Graduado", "Ensino Médio", "Outros"))
#View(dados_clientes$Escolaridade)
#str(dados_clientes$Escolaridade)
#View(dados_clientes)

# Convertendo a variável para o tipo fator com faixa etária
#hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade, c(0,30,50,100), labels = c("Jovem", "Adulto", "Idoso"))
#View(dados_clientes)

#Convertendo a varialve que indica pagamentos para o tipo fator
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

#Omitindo valores N/A
dados_clientes <- na.omit(dados_clientes)
#dim(dados_clientes)

#Transformando a variável dependente em fator
dados_clientes$Inadimplente <- as.factor(dados_clientes$Inadimplente)

#Vizualição após as conversões
#str(dados_clientes$Inadimplente)
#View(dados_clientes)

#table(dados_clientes$Inadimplente)

#Plot da distribuição usando ggplot2
qplot(Inadimplente, data = dados_clientes, geom = "bar") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Set seed
set.seed(12345)

#Amostragem estratificada
indice <- createDataPartition(dados_clientes$Inadimplente, p = 0.75, list = FALSE)
#dim(indice)

#Definindo os dados de treinamento como subconjunto do conjunto de dados original 
#com números de índice de linha (conforme identificado acima) e todas as colunas 
dados_treino <- dados_clientes[indice,]
#dim(dados_treino)

#Tudo o que não está no dataset de treinamento está no dataset de teste.
dados_teste <-dados_clientes[-indice,]
#dim(dados_teste)
#dim(dados_treino)

#Construindo a primeira versão do modelo 
modelo_v1 <- randomForest(Inadimplente ~ ., data = dados_treino)
#modelo_v1

#Previsões com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)

#Contruindo a confusion Matrix
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$Inadimplente, positive = "1" )
#cm_v1

#######---- Balanceando--------######

#Instalando o pacote DMwR(porém não está mais disponível no Cran)
install.packages("remotes")
library(remotes) 
remotes::install_github("cran/DMwR")
library(DMwR)

#Aplicando o Smote
set.seed(9560)
dados_treino_bal <- SMOTE(Inadimplente ~ ., data = dados_treino)
#table(dados_treino_bal$Inadimplente)
#prop.table(table(dados_treino_bal$Inadimplente))

#Criando a segunda versão do modelo
modelo_v2 <- randomForest(Inadimplente ~ ., data = dados_treino_bal)
#modelo_v2

#Previsões com o modelo teste
previsoes_v2 <- predict(modelo_v2, dados_teste)
#previsoes_v2

#Confusion Matrix 
?caret::confusionMatrix
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$Inadimplente, positive = "1")
#cm_v2

#Analisando as variáveis mais importantes para as previsões
View(dados_treino)
varImpPlot(modelo_v2)

#Criando o terceiro modelo com base nas variáveis mais importantes 
modelo_v3 <- randomForest(Inadimplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, data = dados_treino_bal)
modelo_v3

#Confusion Matrix modelo 3
previsoes_v3 <- predict(modelo_v3, dados_teste)
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$Inadimplente, positive = "1")
cm_v3

#Salvando o modelo na memória de armazenamento permanente 
saveRDS(modelo_v3, file = "modelo/modelo_v3.rds")

#######################################################################################

#Carregando o modelo
modelo_final <- readRDS("modelo/modelo_v3.rds")

#Previsões com novos clientes 

#Novos dados 

# Dados dos clientes 
PAY_0 <- c(0, 0, 0)
PAY_2 <- c(0, 0, 0)
PAY_3 <- c(1, 0, 0)
PAY_AMT1 <- c(1100, 1000, 1200)
PAY_AMT2 <- c(1500, 1300, 1150)
PAY_5 <- c(0, 0, 0)
BILL_AMT1 <- c(350, 450, 280)

#Concatena em um dataframe
novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

#Convertendo o tipo de dados 
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))
str(novos_clientes)
str(dados_treino_bal)

#Previsões
previsoes_novo_clientes <- predict(modelo_final, novos_clientes)
table(previsoes_novo_clientes)
previsoes_novo_clientes





