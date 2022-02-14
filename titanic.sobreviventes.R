
---
title: "Titanic.sobreviventes"
author: "Renata Augusto Dantas"
---
  
# Estatistica de sobreviventes do famoso naufrágio (titanic).
# Para isso iremos utilizar o pacote tidyverse para nos auxiliar nesta analise.
---
install.packages("tidyverse")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("gplot")
library("dplyr")
library("ggplot2")
library("randomForest")
library("readr")

# Carregando os datasets que iremos utilizar.

dados_treino <- read_csv('C:/Users/Renata.BEMVINDO/OneDrive/Documentos/Kaggle/train.csv')
dados_teste <- read_csv('C:/Users/Renata.BEMVINDO/OneDrive/Documentos/Kaggle/test.csv')

# Visualização de dados dos dois banco de dados.

str(dados_treino)
str(dados_teste)

# Dados de treino: 891 observações com 12 variáveis.
# Dados de teste: 418 observações com 11 variáveis.

# Variáveis que não utilizaremos apresentam como a sigla NA.

print("Quantidade de dados NA na variável Age:")
sum(is.na(dados_treino$Age))

# Foram identificados 177 NA no dataset, 
# Iremos colocar um registro para distinguir o que é treino e teste.

dados_treino$lsTrainSet <- TRUE
dados_teste$lsTrainSet <- FALSE

# Criação da observação survived no dataset teste para darmos continuidade de unificar os dados.

dados_teste$Survived <- NA

# Junção dos dois dataset.

titanic.sobreviventes <- rbind(dados_treino, dados_teste)

# Dados em true e false da distinção da base de dados.

table(titanic.sobreviventes$lsTrainSet)

# Campo de embarked, filtrar a base e fazer um replace com ´s´.
# Realocação dos resultados onde consta local não encontrado local.

table(titanic.sobreviventes$Embarked, useNA = "ifany")
miss <- is.na(titanic.sobreviventes$Embarked)
titanic.sobreviventes[miss, "Embarked"] <- "Other"
titanic.sobreviventes[titanic.sobreviventes$Embarked == '', "Embarked"] <- 's'

# Idades não declaradas seram apresentadas como true.

table(is.na(titanic.sobreviventes$Age))

# Mediana dos registros de idades validos.

age.median <- median(titanic.sobreviventes$Age, na.rm = TRUE)

# Preenchimento dos dados de idade pela mediana encontrada.

titanic.sobreviventes[is.na(titanic.sobreviventes$Age), "Age"] <- age.median
table(is.na(titanic.sobreviventes$Age))

# Campo Fare, encontrar registro não identificados.

table(is.na(titanic.sobreviventes$Fare))

# Pegar este registro e atribuir a uma variavel.

fare.median <- median(titanic.sobreviventes$Fare, na.rm = TRUE)

# Preencher a informação faltante com a média.

titanic.sobreviventes[is.na(titanic.sobreviventes$Fare), "Fare"] <- fare.median
table(is.na(titanic.sobreviventes$Fare))

# Categorizar Pclass utilizando a variavel qualitativa.

titanic.sobreviventes$Pclass <- as.factor(titanic.sobreviventes$Pclass)
titanic.sobreviventes$Sex <- as.factor(titanic.sobreviventes$Sex)
titanic.sobreviventes$Embarked <- as.factor(titanic.sobreviventes$Embarked)
dados_treino <- titanic.sobreviventes[titanic.sobreviventes$lsTrainSet == TRUE,]
dados_teste <- titanic.sobreviventes[titanic.sobreviventes$lsTrainSet == FALSE,]
dados_treino$Survived <- as.factor(dados_treino$Survived)

# Contrução da previsão de sobreviventes.

survived.equation <- " Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Formulas adicionadas em outros objetos.

survived.formula <- as.formula(survived.equation)

# Criação do modelo criado a partir da string de armazenamento em survived.equation.

Resultado_final <- randomForest(formula = survived.formula, data = dados_treino, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(dados_teste))
survived <- predict(Resultado_final, newdata = dados_teste)
PassengerId <- dados_teste$PassengerId

# Finalização da analise de sobreviventes do titanic.

output.df <- as.data.frame(PassengerId)
output.df$survived <- survived

write.csv(output.df, file = "Kaggle_titanic.sobreviventes.csv", row.names = FALSE)

# Vamos ver algumas visualizações em gráficos para termos uma visão mais detalhada do acontecimento.

# Em barras:Quantidade de passageiros por classe.

ggplot(dados_teste,aes(Pclass)) + 
  geom_bar(aes(fill = factor(Pclass)), alpha = 1) +
  ggtitle("Quantidade de passageiros por classe") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Em barras:Taxas de Sobrevivência.

ggplot (dados_teste, aes(x = survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passageiros",
       title = "Taxas de Sobrevivência ")

#Em histograma:Taxas de Sobrevivência por Idade.

ggplot (dados_teste, aes(x = Age, fill = survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passageiros",
               x = "Idade",
               title = "Taxas de Sobrevivência por Idade")

# Em barras: Taxa de sobrevivência por Sexo.

ggplot (dados_teste, aes(x = Sex, fill = survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passageiros",
               title = "Taxas de Sobrevivência  por Sexo")

# Em barras: Taxas de Sobrevivência por Classe.

ggplot (dados_teste, aes(x = Pclass, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passageiros",
               title = "Taxas de Sobrevivência por Classe")

# Em barras: Taxas de Sobrevivência por Classe e Sexo.

ggplot (dados_teste, aes(x = Sex, fill = survived)) + 
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y = "Passageiros",
               title = "Taxas de Sobrevivência por Classe e Sexo")

# Em hitograma: Distribuição por Idade.

ggplot (dados_teste, aes(x = Age)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Contagem de Passageiros",
               x = "Idade",
               title = "Distribuição por Idade")

# Neste utilizamos outra base de dados com o campo survived preenchido.

# Em quadro de dados:Taxas de Sobrevivência por Idade.

ggplot (dados_treino, aes(x = Survived, y = Age)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Idade",
               x = "Sobreviventes",
               title = "Taxas de Sobrevivência por Idade")

# Apenas uma demonstração para ver que o gráfico,
# Nesta base de dados o survived não está com o resultado.

# Em densidade e probabilidade: Taxas de Sobrevivência por Idade, Classe e Sexo.
ggplot (dados_teste, aes(x = Age, fill = survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density() +
  labs(y = "Idade",
               x = "Sobreviveu",
               title = "Taxas de Sobrevivência por Idade, Classe e Sexo")

# Em histograma: Taxas de Sobrevivência por Idade, Classe e Sexo.

ggplot (dados_teste, aes(x = Age, fill = survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Idade",
               x = "Sobreviveu",
               title = "Taxas de Sobrevivência por Idade, Classe e Sexo")

