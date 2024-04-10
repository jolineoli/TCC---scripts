# INICIANDO ANALISE FATORIAL PCA DO TCC
# INTALANDO PACOTES
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl") # importar arquivo Excel
installed.packages()
install.packages(tidyverse)
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregar o dataset


#inicio

library(readxl)
clientes <- read_excel("BankChurnersalterado3.xlsx", 
                                    col_types = c("numeric", "text", "numeric", 
                                                  "text", "numeric", "text", "text", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "numeric", "numeric", "text", "numeric", 
                                                  "text", "text", "numeric", "numeric", 
                                                  "text", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))


summary(clientes[,21:29])


pearson <- rcorr(as.matrix(clientes[,21:29]), type="pearson")

corr_coef <- pearson$r

corr_sig <- round(pearson$P, 5)

ggplotly(
  clientes[,21:29] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

chart.Correlation(clientes[,21:29], histogram = TRUE, pch = "+")

#TESTE 
cortest.bartlett(clientes[,21:29])

#FATORIAL
fatorial <- principal(clientes[,21:29],
                      nfactors = length(clientes[,21:29]),
                      rotate = "none",
                      scores = TRUE)
fatorial


#AUTOVALORES
eigenvalues <- round(fatorial$values, 5)
eigenvalues

#SOMA
round(sum(eigenvalues), 2)

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")
#SCORES FATORIAIS
scores_fatoriais <- as.data.frame(fatorial$weights)

#visualização dos scores
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

#fatores
fatores <- as.data.frame(fatorial$scores)
View(fatores)

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades 
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

#aplicar o critério de kaiser
k <- sum(eigenvalues > 1)
print(k)

#fatorial para os componentes pricipais
fatorial2 <- principal(clientes[,21:29],
                      nfactors = k,
                      rotate = "none",
                      scores = TRUE)
fatorial2

#comunalidade 2
comunalidades2 <- as.data.frame(unclass(fatorial2$communality)) %>%
  rename(comunalidades = 1)

#visualização das comunalidades 2
round(comunalidades2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Loading plot com as cargas dos 4 fatores
cargas_fatoriais[, 1:4] %>%
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()

# Adicionando os fatores extraídos no banco de dados original
clientes <- bind_cols(clientes,
                           "fator 1" = fatores$PC1, 
                           "fator 2" = fatores$PC2,
                           "fator 3" = fatores$PC3,
                           "fator 4" = fatores$PC4)

# Criação de um ranking Critério da soma ponderada e ordenamento)
clientes$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
  fatores$PC2 * variancia_compartilhada$PC2[2] + fatores$PC3 * variancia_compartilhada$PC3[2] +
  fatores$PC4 * variancia_compartilhada$PC4[2]

# Visualizando o ranking final
clientes %>%
  arrange(desc(ranking)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 17)
