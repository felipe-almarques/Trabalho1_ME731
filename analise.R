########################################################################## 
####                          Aplicação PCA                           ####
##########################################################################

## dados disponivel em:
# https://www.kaggle.com/datasets/mansoordaku/ckdisease
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(ggcorrplot)
library(xtable)
library(googlesheets4)

#### Lendo os dados ####

dados <- read_csv("kidney_disease.csv")

glimpse(dados)
dados <- dados %>% 
  select(age, bp, bgr, bu, sc, sod, pot, hemo, pcv, wc, rc)
dados$pcv <- as.numeric(dados$pcv)
dados$wc <- as.numeric(dados$wc)
dados$rc <- as.numeric(dados$rc)

# Contagem de NA's
sum(colSums(is.na(dados))) # 636 valores faltantes

dados <- dados %>% 
  drop_na()


## graficando a matriz de correlação ##
round(cor(dados), 3) %>% xtable()

ggcorrplot(cor(dados)) +
  labs(title = "Correlação entre as variáveis")

#### Calculo das Componentes Principais ####
dados_pca <- PCA(dados, scale.unit = T, graph = F)

summary(dados_pca)

round(princomp(scale(dados))$loadings[,1:5],3) # autovetores
round(dados_pca$eig[1:11],3) # autovalores
round(dados_pca$eig[23:33],3) # cumulative perc. of total variance


## Plotando os autovalores
# Scree-plot
fviz_eig(dados_pca, addlabels = TRUE, ylim = c(0,50)) +
  labs(x = "Número de dimensões", y = "Porcentagem de variação representada") +
  theme_bw()

# Variable correlation plot
fviz_pca_var(dados_pca, col.var = "black") +
  labs(x = "Dimensão 1 (42.4%)", y = "Dimensão 2 (11.7%)",
       title = "Correlação das variáveis") 

