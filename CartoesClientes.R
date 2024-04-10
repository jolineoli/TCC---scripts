#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



#REGRESSÃO LINEAR SIMPLES                         #
#                     EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS               #
################################################################################

#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
load(file = "ClientesCartoes.RData")

#                OBSERVANDO OS DADOS CARREGADOS DO DATASET BankChuners

BankChurners %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

Simples <- select(BankChurners, 3,15)

#visualização
glimpse(Simples) 

#Estatísticas univariadas
summary(Simples)

#Renomear

Simples <-rename(Simples, Idade = Customer_Age, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples, aes(x = Idade, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Idade",
         y = "Uso_Rotativo",
         title = paste("R²:",
                       round(((cor(Simples$Idade, Simples$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples2 <- select(BankChurners, 5,15)

#visualização
glimpse(Simples2) 

#Estatísticas univariadas
summary(Simples2)

Simples2 <-rename(Simples2, Dependentes = Dependent_count, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples2, aes(x = Dependentes, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Dependentes",
         y = "Uso_Rotativo",
         title = paste("R²:",
                       round(((cor(Simples2$Dependentes, Simples2$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#Verificamos que o gráfico não condiz com uma regressão linear pois não foi possível verificar uma correlação.

Simples3 <- select(BankChurners, 10,15)

#visualização
glimpse(Simples3) 

#Estatísticas univariadas
summary(Simples3)

Simples3 <-rename(Simples3, Meses_de_relacionamento = Months_on_book, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples3, aes(x = Meses_de_relacionamento, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Dependetes",
         y = "Total_Revolving_Bal",
         title = paste("R²:",
                       round(((cor(Simples3$Meses_de_relacionamento, Simples3$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples4 <- select(BankChurners, 11,15)

#visualização
glimpse(Simples4) 

#Estatísticas univariadas
summary(Simples4)

Simples4 <-rename(Simples4, numero_produtos = Total_Relationship_Count, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples4, aes(x = numero_produtos, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "numero_produtos",
         y = "Total_Revolving_Bal",
         title = paste("R²:",
                       round(((cor(Simples4$numero_produtos, Simples4$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples5 <- select(BankChurners, 13,15)

#visualização
glimpse(Simples5) 

#Estatísticas univariadas
summary(Simples5)

Simples5 <-rename(Simples5, Contatos = Contacts_Count_12_mon, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples5, aes(x = Contatos, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Contatos",
         y = "Total_Revolving_Bal",
         title = paste("R²:",
                       round(((cor(Simples5$Contatos, Simples5$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples6 <- select(BankChurners, 14,15)

#visualização
glimpse(Simples6) 

#Estatísticas univariadas
summary(Simples6)

Simples6 <-rename(Simples6, Limite_de_credito = Credit_Limit, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples6, aes(x = Limite_de_credito, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Limite_de_credito",
         y = "Total_Revolving",
         title = paste("R²:",
                       round(((cor(Simples6$Limite_de_credito, Simples6$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples7 <- select(BankChurners, 16,15)

#visualização
glimpse(Simples7) 

#Estatísticas univariadas
summary(Simples7)

Simples7 <-rename(Simples7, media_abertura_credito = Avg_Open_To_Buy, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples7, aes(x = media_abertura_credito, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "media_abertura_credito",
         y = "Uso_Rotativo",
         title = paste("R²:",
                       round(((cor(Simples7$media_abertura_credito, Simples7$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

Simples8 <- select(BankChurners, 18,15)

#visualização
glimpse(Simples8) 

#Estatísticas univariadas
summary(Simples8)

Simples8 <-rename(Simples8, Total_transações = Total_Trans_Amt, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples8, aes(x = Total_transações, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Total_transações_valor",
         y = "Uso_Rotativo",
         title = paste("R²:",
                       round(((cor(Simples8$Total_transações, Simples8$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)



Simples9 <- select(BankChurners, 19,15)

#visualização
glimpse(Simples9) 

#Estatísticas univariadas
summary(Simples9)

Simples9 <-rename(Simples9, Total_transações_cartao = Total_Trans_Ct, Uso_Rotativo = Total_Revolving_Bal)

#gráfico de dispersão com a funçao
ggplotly(
  ggplot(Simples9, aes(x = Total_transações_cartao, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Total_transações",
         y = "Uso_Rotativo",
         title = paste("R²:",
                       round(((cor(Simples9$Total_transações_cartao, Simples9$Uso_Rotativo))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#Estimando o modelo
modelo_simples9 <- lm(formula = Uso_Rotativo ~ Total_transações_cartao,
              data = Simples9)

#Observando os parâmetros do modelo_simples9
summary(modelo_simples9)

#Outras maneiras de apresentar os outputs do modelo
#função 'summ' do pacote 'jtools'
summ(modelo_simples9, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_simples9, scale = F, digits = 4)

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_simples9, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
Simples9$yhat <- modelo_simples9$fitted.values
Simples9$erro <- modelo_simples9$residuals

#Visualizando a base de dados com as variáveis yhat e erro
Simples9 %>%
  select(Total_transações_cartao, Uso_Rotativo, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Gráfico didático para visualizar o conceito de R²
ggplotly(
  ggplot(Simples9, aes(x = Total_transações_cartao, y = Uso_Rotativo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Ychapéu - Ymédio", x = Total_transações_cartao, xend = Total_transações_cartao,
                     y = yhat, yend = mean(Uso_Rotativo)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Erro = Y - Ychapéu", x = Total_transações_cartao, xend = Total_transações_cartao,
                     y = Uso_Rotativo, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Transações",
         y = "Uso_rotativo") +
    scale_color_manual("Legenda:",
                       values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)

#Cálculo manual do R²
R2 <- (sum((Simples9$yhat - mean(Simples9$Uso_Rotativo))^2))/
  ((sum((Simples9$yhat - mean(Simples9$Uso_Rotativo))^2)) + (sum((Simples9$erro)^2)))

round(R2, digits = 4)

#coeficiente de ajuste (R²) é a correlação ao quadrado
cor(Simples9[1:2])

#Modelo auxiliar para mostrar R² igual a 100% (para fins didáticos)
#note que aqui o yhat é a variável dependente
modelo_auxiliar <- lm(formula = yhat ~ Uso_Rotativo,
                      data = Simples9)
summary(modelo_auxiliar)

#Gráfico mostrando o perfect fit
my_plot <-
  ggplot(Simples9, aes(x = Total_transações_cartao, y = yhat)) +
  geom_point(color = "#39568CFF", size = 5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  labs(x = "Transações",
       y = "Rotativo") +
  scale_color_manual("Legenda:",
                     values = "grey50") +
  theme_cowplot()
my_plot

#Com figuras JPEG e PNG
ggdraw() + #funções 'ggdraw', 'draw_image' e 'draw_plot' do pacote 'cowplot'
  draw_image("https://cdn.pixabay.com/photo/2021/12/14/16/32/harry-potter-6870854_960_720.png",
             x = 0.075, y = -0.15, scale = .44) +
  draw_image("https://img.freepik.com/fotos-premium/agulha-de-trico-isolada_93675-25968.jpg?w=1380",
             x = -0.235, y = 0.25, scale = .37) +
  draw_plot(my_plot)


#                            REGRESSÃO LINEAR MÚLTIPLA                         #
#                                                                              #
################################################################################

multiplaclientes <-select(BankChurners,3,13,14,15,16,18,19)

#Estatísticas univariadas
summary(multiplaclientes)

#trocar de nomes
multiplaclientes <-rename(multiplaclientes, Idade = Customer_Age, Contatos = Contacts_Count_12_mon, 
                          Limite_de_credito = Credit_Limit, Uso_Rotativo = Total_Revolving_Bal, 
                          Media_Abertura_Credito = Avg_Open_To_Buy,Total_Transações_Valor = Total_Trans_Amt, 
                          Total_Trans_Cartão = Total_Trans_Ct)

#vamos incluir as variaveis gênero que será dummizada

multiplaclientes <-select(BankChurners,3,4,13,14,15,16,18,19)

multiplaclientes_dummies <- dummy_columns(.data = multiplaclientes,
                                   select_columns = "Gender",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

multiplaclientes_dummies <-rename(multiplaclientes_dummies,Idade = Customer_Age, 
                                  Contatos = Contacts_Count_12_mon, 
                                  Limite_de_credito = Credit_Limit, 
                                  Uso_Rotativo = Total_Revolving_Bal, 
                                  Media_Abertura_Credito = Avg_Open_To_Buy,
                                  Total_Transações_Valor = Total_Trans_Amt, 
                                  Total_Trans_Cartão = Total_Trans_Ct,
                                  Genero = Gender_M)

################################################################################
#                             ESTUDO DAS CORRELAÇÕES                           #
################################################################################
#Visualizando a base de dados
multiplaclientes_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 24)


#A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
multiplaclientes_dummies %>%
  correlation(method = "pearson") %>%
  plot()

#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation(multiplaclientes_dummies, histogram = TRUE)

#    ESTIMANDO UM MODELO MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS     #
################################################################################
#Estimando a Regressão Múltipla
modelo_multiplaclientes_dummies <- lm(formula = Uso_Rotativo ~ . - Media_Abertura_Credito, 
                                      data = multiplaclientes_dummies)

#Parâmetros do modelo
summary(modelo_multiplaclientes_dummies)
confint(modelo_multiplaclientes_dummies, level = 0.95) # siginificância de 5%


#Outro modo de apresentar os outputs do modelo - função 'summ' do pacote 'jtools'
summ(modelo_multiplaclientes_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_multiplaclientes_dummies, scale = F, digits = 5)


#Salvando os fitted values na base de dados
multiplaclientes_dummies$UsoRotativofit <- modelo_multiplaclientes_dummies$fitted.values

#Gráfico 3D com scatter e fitted values
scatter3d(Uso_Rotativo ~ Genero + Limite_de_credito,
          data = multiplaclientes_dummies,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

#                            PROCEDIMENTO STEPWISE                             #
################################################################################
#Aplicando o procedimento Stepwise, temos o seguinte código:
step_clientescartoes <- step(modelo_multiplaclientes_dummies, k = 3.841459)

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F), 7)

summary(step_clientescartoes)
#Este procedimento no R removeu as variáveis. 
#forma funcional linear!


extract_eq(step_clientescartoes, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

export_summs(step_clientescartoes, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(step_clientescartoes, level = 0.95) # siginificância 5%
plot_summs(step_clientescartoes, colors = "#440154FF") #função 'plot_summs' do pacote 'ggstance'

#Parâmetros padronizados
plot_summs(step_clientescartoes, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_clientescartoes, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_multiplaclientes_dummies, step_clientescartoes, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

##############################################################################
#Vamos fazer a analise incluido variáveis que serão dummizadas

multiplaclientes2 <-select(BankChurners,3,4,6,7,8,9,13,14,15,16,18,19)

#PROCEDIMENTO N-1 DUMMIES                            #
################################################################################
#Dummizando a variável regiao. O código abaixo, automaticamente, fará: a) o
#estabelecimento de dummies que representarão cada uma das regiões da base de 
#dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
#de referência a dummy mais frequente.
multiplaclientes_dummies2 <- dummy_columns(.data = multiplaclientes2,
                                   select_columns = "Gender",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

multiplaclientes_dummies2 <- dummy_columns(.data = multiplaclientes_dummies2,
                                           select_columns = "Education_Level",
                                           remove_selected_columns = T,
                                           remove_most_frequent_dummy = F)

multiplaclientes_dummies2 <- dummy_columns(.data = multiplaclientes_dummies2,
                                           select_columns = "Marital_Status",
                                           remove_selected_columns = T,
                                           remove_most_frequent_dummy = F)

multiplaclientes_dummies2 <- dummy_columns(.data = multiplaclientes_dummies2,
                                           select_columns = "Income_Category",
                                           remove_selected_columns = T,
                                           remove_most_frequent_dummy = F)

multiplaclientes_dummies2 <- dummy_columns(.data = multiplaclientes_dummies2,
                                           select_columns = "Card_Category",
                                           remove_selected_columns = T,
                                           remove_most_frequent_dummy = F)

#Vamos Renomear as variáveis

multiplaclientes_dummies2 <-rename(multiplaclientes_dummies2,Idade = Customer_Age, 
                                  Contatos = Contacts_Count_12_mon, 
                                  Limite_de_credito = Credit_Limit, 
                                  Uso_Rotativo = Total_Revolving_Bal, 
                                  Media_Abertura_Credito = Avg_Open_To_Buy,
                                  Total_Transações_Valor = Total_Trans_Amt, 
                                  Total_Trans_Cartão = Total_Trans_Ct,
                                  Genero = Gender_M,
                                  Superior = Education_Level_College,
                                  Doutorado = Education_Level_Doctorate,
                                  Ensino_Medio = "Education_Level_High School",
                                  Graduado = Education_Level_Graduate,
                                  Nao_Escolarizado = Education_Level_Uneducated,
                                  Pos_Graduado = "Education_Level_Post-Graduate",
                                  Ecolaridade_desconhecida = Education_Level_Unknown,
                                  Divorciado = Marital_Status_Divorced,
                                  Casado = Marital_Status_Married,
                                  Solteiro = Marital_Status_Single,
                                  Estado_Marital_Desconhecido = Marital_Status_Unknown,
                                  Renda_Menos40k = "Income_Category_Less than $40K",
                                  Renda_Entre_60_80mil = "Income_Category_$60K - $80K",
                                  Renda_Entre_80_120mil = "Income_Category_$80K - $120K",
                                  Renda_Entre_40_60mil = "Income_Category_$40K - $60K",
                                  Renda_Mais_de_120mil = "Income_Category_$120K +",
                                  Renda_Desconhecida = "Income_Category_Unknown",
                                  Cartao_Blue = Card_Category_Blue,
                                  Cartao_Platinum = Card_Category_Platinum,
                                  Cartao_Gold = Card_Category_Gold,
                                  Cartao_Silver = Card_Category_Silver)

 #Amém, quase Desisti!
   
#Visualizando a base de dados dummizada
multiplaclientes_dummies2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO                       #
################################################################################
#Modelagem com todas as variáveis
modelo_multiplaclientes_dummies2 <- lm( Uso_Rotativo ~ . - Media_Abertura_Credito 
                                        , data = multiplaclientes_dummies2)

#Parâmetros do modelo_corrupcao_dummies
summary(modelo_multiplaclientes_dummies2)

#Vamos remodelar retirando as variaveis com NA
modelo_multiplaclientes_dummies2 <- lm( Uso_Rotativo ~ . - Media_Abertura_Credito
                                        -Ecolaridade_desconhecida
                                        -Estado_Marital_Desconhecido
                                        -Renda_Desconhecida
                                        -Cartao_Silver,
                                        data = multiplaclientes_dummies2)


#Parâmetros do modelo
summary(modelo_multiplaclientes_dummies2)
confint(modelo_multiplaclientes_dummies2, level = 0.95) # siginificância de 5%


#Outro modo de apresentar os outputs do modelo - função 'summ' do pacote 'jtools'
summ(modelo_multiplaclientes_dummies2, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_multiplaclientes_dummies2, scale = F, digits = 5)


#Salvando os fitted values na base de dados
multiplaclientes_dummies2$UsoRotativofit <- modelo_multiplaclientes_dummies2$fitted.values


#Gráfico 3D com scatter e fitted values
scatter3d(Uso_Rotativo ~ Contatos + Limite_de_credito,
          data = multiplaclientes_dummies2,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))


#fim


#           REGRESSÃO NÃO LINEAR SIMPLES E TRANSFORMAÇÃO DE BOX-COX            #
#                 EXEMPLO 04 - CARREGAMENTO DA BASE DE DADOS                   #
################################################################################


#Estatísticas univariadas
summary(Simples6)

#Gráfico de dispersão
ggplotly(
  Simples6 %>% 
    ggplot() +
    geom_point(aes(x = Limite_de_credito, y = Uso_Rotativo),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "Limite",
         y = "Uso_Rotativo") +
    theme_bw()
)



#Gráfico de dispersão com ajustes (fits) linear e não linear
ggplotly(
  Simples6 %>% 
    ggplot() +
    geom_point(aes(x = Limite_de_credito, y = Uso_Rotativo),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = Limite_de_credito, y = Uso_Rotativo),
                method = "lm", formula = y ~ x,
                color = "#FDE725FF", se = F) +
    geom_smooth(aes(x = Limite_de_credito, y = Uso_Rotativo),
                method = "loess", formula = y ~ x,
                color = "#440154FF", se = F) +
    labs(x = "Limite",
         y = "Rotativo") +
    theme_bw()
)

#Estimação do modelo OLS linear
modelo_linear <- lm(formula = Uso_Rotativo ~ Limite_de_credito,
                    data = Simples6)

summary(modelo_linear)

################################################################################
#        TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                             SHAPIRO-FRANCIA                                  #
################################################################################
#Shapiro-Wilk: n <= 30
## shapiro.test(modelo_linear$residuals)

#Shapiro-Francia: n > 30
sf.test(modelo_linear$residuals) #função 'sf.test' do pacote 'nortest'


#faremos outro teste de normalidade, o shapiro-francia só suporta 5000
#observaçoes, Faremos o Anderson-Darling

ad.test(rnorm(5001))

ad.test(runif(5001))

#Não deu certo

#Vamos criar uma nova função Shapito-Francia
#Quebramos o código e aumentamos o limite de observaçoes

sf.teste2 <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 5000000)) 
    stop("sample size must be between 5 and 5000000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia MBA USP TCC JOLINE", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

sf.teste2(modelo_linear$residuals)

#Histograma dos resíduos do modelo OLS linear
Simples6 %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Não dá para fazer o historiograma porque não temos os resíduo od Shapiro.

#Visualização do comportamento dos resíduos em função dos fitted values do
#do modelo linear, com destaque para as distribuições das variáveis
#(pacote 'ggside')
bebes %>%
  ggplot(aes(x = modelo_linear$fitted.values, y = modelo_linear$residuals)) +
  geom_point(color = "#FDE725FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Resíduos") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

#            PROCEDIMENTO STEPWISE no modelo_multiplaclientes_dummies2        #
################################################################################
#Aplicando o procedimento Stepwise, temos o seguinte código:
step_clientes_dummies2 <- step(modelo_multiplaclientes_dummies2, k = 3.841459)

#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F), 7)

summary(step_clientes_dummies2)
#Este procedimento no R incluiu uma variável apenas, a dummy "casado". 
#Visualizamos as variáveis restantes na summary
#Este é o nosso modelo
#forma funcional linear!

export_summs(step_clientes_dummies2, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(step_clientes_dummies2, level = 0.95) # siginificância 5%
plot_summs(step_clientes_dummies2, colors = "#440154FF") #função 'plot_summs' do pacote 'ggstance'

#Parâmetros padronizados
plot_summs(step_clientes_dummies2, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_clientes_dummies2, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_multiplaclientes_dummies2, step_clientes_dummies2, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))

#Equação do modelo 

extract_eq(step_clientes_dummies2, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

