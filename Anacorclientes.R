# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse",
             "dplyr",
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando a base de dados
clientesbanco <- read_csv("BankChurners.csv")

#CLIENTNUM

Client number. Unique identifier for the customer holding the account

#Attrition_FlagInternal 

Internal event (customer activity) variable - if the account is closed then 1 else 0

#Customer_Age

Demographic variable - Customers Age in Years

#Gender

Demographic variable - M=Male, F=Female

#Dependent_count

Demographic variable - Number of dependents

#Education_Level

Demographic variable - Educational Qualification of the account holder (example: high school, college graduate, etc.)

#Marital_Status

Demographic variable - Married, Single, Divorced, Unknown

#Income_Category

Demographic variable - Annual Income Category of the account holder (< $40K, $40K - 60K, $60K - $80K, $80K-$120K, > $120K, Unknown)

#Card_Category

Product Variable - Type of Card (Blue, Silver, Gold, Platinum)

#Months_on_book

Period of relationship with bank

#Total_Relationship_Count

Total no. of products held by the customer

#Months_Inactive_12_mon

No. of months inactive in the last 12 months

#Contacts_Count_12_mon

No. of Contacts in the last 12 months

#Credit_Limit

Credit Limit on the Credit Card

#Total_Revolving_Bal

Total Revolving Balance on the Credit Card. Saldo rotativo total no cartao de credito.

#Avg_Open_To_Buy

Open to Buy Credit Line (Average of last 12 months)

#Total_Amt_Chng_Q4_Q1

Change in Transaction Amount (Q4 over Q1)

#Total_Trans_Amt

Total Transaction Amount (Last 12 months)

#Total_Trans_Ct

Total Transaction Count (Last 12 months)

#Total_Ct_Chng_Q4_Q1

Change in Transaction Count (Q4 over Q1)

#Avg_Utilization_Ratio

Average Card Utilization Ratio

#Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1

Naive Bayes

#Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2Naive Bayes 

## Algumas variáveis são qualitativas e outras quantitativas

# Vamos categorizar as variáveis quanti (por critério estatístico)

clientesbanco <- clientesbanco %>%
  mutate(Categ_Customer_Age = case_when(Customer_Age <= quantile(Customer_Age, 0.25, na.rm = T) ~ "jovens",
                                 Customer_Age > quantile(Customer_Age, 0.25, na.rm = T) & Customer_Age <= quantile(Customer_Age, 0.5, na.rm = T) ~ "adultos",
                                 Customer_Age > quantile(Customer_Age, 0.50, na.rm = T) & Customer_Age <= quantile(Customer_Age, 0.75, na.rm = T) ~ "adultos_maduros",
                                 Customer_Age > quantile(Customer_Age, 0.75, na.rm = T) ~ "Idosos"))

quartis não foram adequados a essa variável

pacotes <- c("DescTools")

library(DescTools)

clientesbanco <- transform(clientesbanco,
                            faixa_etaria = cut(Customer_Age, breaks = c(min(Customer_Age),25,40,65,max(Customer_Age)), right = F, include.lowest = T))

#criando uma coluna faixa etaria com niveis de idade 
clientesbanco <- clientesbanco %>%  
  mutate(faixa_etaria_descritiva = case_when(Customer_Age <25 ~ "jovem", 
                                  between(Customer_Age, 26,40) ~ "adulto", 
                                  between(Customer_Age, 41,65)~ "meia_idade", 
                                  Customer_Age >65 ~ "idoso")) 
head(clientesbanco) 

#cliando uma coluna faixa de uso do rotativo
clientesbanco <- clientesbanco %>%
  transform(limites_credito = cut(Credit_Limit, breaks = c(min(Credit_limit),5000,10000,20000,30000,max(Credit_Limit)), right = T, include.lowest = T))

não funciona, usaremos a formula dos quartis 

clientesbanco <- clientesbanco %>%
  mutate(Categ_Total_Revolving_Bal = case_when(Total_Revolving_Bal <= quantile(Total_Revolving_Bal, na.rm = T) ~ "poucou_ou_nenhum_uso",
                                    Total_Revolving_Bal > quantile(Total_Revolving_Bal, 0.25, na.rm = T) & Total_Revolving_Bal <= quantile(Total_Revolving_Bal, 0.50, na.rm = T) ~ "uso_medio",
                                    Total_Revolving_Bal > quantile(Total_Revolving_Bal, 0.50, na.rm = T) & Total_Revolving_Bal <= quantile(Total_Revolving_Bal, 0.75, na.rm = T) ~ "uso_alto",
                                    Total_Revolving_Bal > quantile(Total_Revolving_Bal,0.75, na.rm = T) ~ "uso_muito_alto"))

# Vamos remover as variáveis que não utilizaremos (quantitativas)
clientesbanco <- clientesbanco %>% 
  select(-CLIENTNUM, -Attrition_Flag, -Customer_Age, -Dependent_count, -Months_on_book, -Months_Inactive_12_mon, -Total_Relationship_Count, -Contacts_Count_12_mon, -Credit_Limit, -Total_Revolving_Bal, -Avg_Open_To_Buy, -Avg_Utilization_Ratio, -Total_Trans_Amt, -Total_Amt_Chng_Q4_Q1, -Total_Trans_Ct, -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2, -Total_Ct_Chng_Q4_Q1, -faixa_etaria)

# A função para a criação da ACM pede que sejam utilizados "fatores"
clientesbanco <- as.data.frame(unclass(clientesbanco), stringsAsFactors=TRUE)

# Tabelas de contingência (todas apresentam associação com alguma variável?)
sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$Gender,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$Education_Level,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$Marital_Status,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$Income_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$Card_Category,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = clientesbanco$Categ_Total_Revolving_Bal,
         var.col = clientesbanco$faixa_etaria_descritiva,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#exclusao da variavel por possur coluna NULL
clientesbanco <- clientesbanco %>% 
  select(-faixa_etaria)


# Vamos gerar a ACM
ACM <- dudi.acm(clientesbanco, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(clientesbanco,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Poderíamos fazer o mapa com as coordenadas obtidas por meio da matriz de Burt

# Consolidando as coordenadas-padrão obtidas por meio da matriz de Burt
df_ACM_B <- data.frame(ACM$co, Variável = rep(names(quant_categorias),
                                              quant_categorias))

# Plotando o mapa perceptual
df_ACM_B %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# É possível obter as coordenadas das observações
df_coord_obs <- ACM$li

# Plotando o mapa perceptual
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = clientesbanco$Total_Revolving_Bal)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Doença Cardíaca") +
  theme_bw()

# Fim!
