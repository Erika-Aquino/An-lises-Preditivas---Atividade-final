# ============================================================
# Disciplina: Análises Preditivas
# Curso: MBA em Engenharia de Dados e Big Data
# Instituição: Universidade de São Paulo (USP)
# ============================================================

# Aluna: Érika Carvalho de Aquino

# ============================================================
# Considere uma idustria que fabrica sorvetes e você está resposavel por fazer as estimativas de vendas da área financeira
# OBJETIVO: Estimar qual será o volume de vendas (em R$) de  sorvete no dia 30/out/2023

# Preparando o ambiente R
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(psych)     
library(summarytools)  
library(tidyr)


# ============================================================
##### 1) Criação de variaveis
# ============================================================
###### 1.1) Crie uma variavel chamada: Dia_da_semana e popule-na com (dom, seg, ter, etc)

# Converter Date para Date (removendo hora) e criar Dia_da_semana
bd <- bd %>%
  mutate(
    Data = as.Date(Date),  # Cria coluna apenas com a data (sem hora)
    Dia_da_semana = weekdays(Data)
  ) %>%
  mutate(
    Dia_da_semana = case_when(
      Dia_da_semana == "domingo"  ~ "dom",
      Dia_da_semana == "segunda-feira" ~ "seg",
      Dia_da_semana == "terça-feira"   ~ "ter",
      Dia_da_semana == "quarta-feira"  ~ "qua",
      Dia_da_semana == "quinta-feira"  ~ "qui",
      Dia_da_semana == "sexta-feira"   ~ "sex",
      Dia_da_semana == "sábado"        ~ "sáb",
      TRUE ~ Dia_da_semana
    )
  )
head(bd)

###### 1.2) Crie uma variavel chamada: Mês_venda e popule-na com (jan,fev,mar,abr, etc)

# Criar a variável Mês_venda com nomes abreviados em português
bd <- bd %>%
  mutate(
    Mês_venda = month(Data, label = TRUE, abbr = TRUE, locale = "pt_BR.UTF-8")
  )
head(bd$Mês_venda)

# ============================================================
##### 2) Façam as análises univariadas
# ============================================================
###### 2.1) Análises descritivas

# Ver estrutura do banco 
str(bd)

# Estatísticas descritivas para variáveis numéricas
# Selecionar apenas variáveis numéricas
numericas <- bd %>% select(where(is.numeric))

# Descritivas padrão
summary(numericas)

# Tabelas de frequência para variáveis categóricas
# Frequência para Dia_da_semana
table(bd$Dia_da_semana)
prop.table(table(bd$Dia_da_semana)) * 100

# Frequência para Mês_venda
table(bd$Mês_venda)
prop.table(table(bd$Mês_venda)) * 100

# Verificar valores ausentes
colSums(is.na(bd))

# Gerar relatório detalhado
view(dfSummary(bd))
 
###### 2.2)  analise graficas: grafico de barras, histogramas, etc
# Histograma da Temperatura
ggplot(bd, aes(x = `Temperatura (graus celsius)`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribuição das temperaturas registradas",
    x = "Temperatura (°C)",
    y = "Frequência"
  ) +
  theme_minimal()

# Histograma do Total de Vendas
ggplot(bd, aes(x = `Total das vendas (R$)`)) +
  geom_histogram(binwidth = 1000, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribuição do total diário de vendas (R$)",
    x = "Total das vendas (R$)",
    y = "Frequência"
  ) +
  theme_minimal()

# Gráfico de barras - Vendas médias por dia da semana
bd %>%
  group_by(Dia_da_semana) %>%
  summarise(venda_media = mean(`Total das vendas (R$)`, na.rm = TRUE)) %>%
  ggplot(aes(x = Dia_da_semana, y = venda_media)) +
  geom_col(fill = "orange") +
  labs(
    title = "Vendas médias (R$) por dia da semana",
    x = "Dia da semana",
    y = "Média de vendas (R$)"
  ) +
  theme_minimal()




# ============================================================
##### 3) Façam as análises bi-variadas
# ============================================================
###### 3.1)  analise graficas bi variadas: Grafico de dispersão (Vendas(y) x Temperatura (x))


## Análise biavariada "Total de vendas" x Temperatura
# Filtrar NAs do que será usado no gráfico
bd_scatter <- bd %>%
  select(`Total das vendas (R$)`, `Temperatura (graus celsius)`, Mês_venda, Dia_da_semana) %>%
  tidyr::drop_na()

# Correlação (para referência no relatório)
cor_pearson <- cor(bd_scatter$`Total das vendas (R$)`,
                   bd_scatter$`Temperatura (graus celsius)`,
                   use = "complete.obs", method = "pearson")
cor_pearson

# Dispersão com suavização com Ajuste linear simples sobreposto
ggplot(bd_scatter,
       aes(x = `Temperatura (graus celsius)`, y = `Total das vendas (R$)`)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Vendas (R$) vs. Temperatura (°C) – ajuste linear",
       x = "Temperatura (°C)", y = "Total das vendas (R$)") +
  theme_minimal()


## Análise bivariada "Dia da semana" x "Total de vendas"
# Boxplot: Vendas por dia da semana
ggplot(bd_scatter, aes(x = Dia_da_semana, y = `Total das vendas (R$)`)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(title = "Vendas (R$) por dia da semana",
       x = "Dia da semana", y = "Total das vendas (R$)") +
  theme_minimal()



## Análise bivariada "Mês venda" x "Total de vendas"
# Filtrar variáveis necessárias e remover NAs
base_mes <- bd %>%
  select(Mês_venda, `Total das vendas (R$)`) %>%
  drop_na()

# --- Gráfico: Boxplot das vendas por mês
ggplot(base_mes, aes(x = Mês_venda, y = `Total das vendas (R$)`)) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.3) +
  labs(title = "Distribuição das vendas (R$) por mês",
       x = "Mês", y = "Total das vendas (R$)") +
  theme_minimal()


# --- Gráfico: Linha da média de vendas ao longo dos meses
ggplot(media_mes, aes(x = Mês_venda, y = venda_media, group = 1)) +
  geom_line(color = "darkblue") +
  geom_point(color = "blue", size = 2) +
  labs(title = "Tendência da média mensal de vendas",
       x = "Mês", y = "Vendas médias (R$)") +
  theme_minimal()


## Evolução das vendas ao longo do tempo
# --- Gráfico: Linha com todas as vendas (dia a dia)
ggplot(bd, aes(x = Data, y = `Total das vendas (R$)`)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  labs(title = "Evolução diária das vendas ao longo do tempo",
       x = "Data", y = "Total das vendas (R$)") +
  theme_minimal()

# --- Gráfico 2: Média mensal de vendas ao longo dos anos
bd_mensal <- bd %>%
  mutate(Ano_mes = floor_date(Data, unit = "month")) %>%
  group_by(Ano_mes) %>%
  summarise(venda_media = mean(`Total das vendas (R$)`, na.rm = TRUE), .groups = "drop")

ggplot(bd_mensal, aes(x = Ano_mes, y = venda_media)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 1.5) +
  labs(title = "Média mensal de vendas ao longo dos anos",
       x = "Ano-Mês", y = "Vendas médias (R$)") +
  theme_minimal()


###### 3.2) calcule a correlação entre as variaveis Temperatura e total de vendas (R$)
# Calcular correlação de Pearson
cor.test(bd$`Temperatura (graus celsius)`,
         bd$`Total das vendas (R$)`,
         method = "pearson",
         use = "complete.obs")

# ============================================================
##### 4) definam a equacao através de uma Regressão Linear
# ============================================================
# Ajustar modelo de regressão linear
modelo_rl <- lm(`Total das vendas (R$)` ~ `Temperatura (graus celsius)`, data = bd)

# Ver o resumo do modelo
summary(modelo_rl)

# Ajustar o modelo
modelo_rl <- lm(`Total das vendas (R$)` ~ `Temperatura (graus celsius)`, data = bd)

# Extrair coeficientes
intercepto <- round(coef(modelo_rl)[1], 2)
inclinação <- round(coef(modelo_rl)[2], 2)
r2 <- round(summary(modelo_rl)$r.squared, 3)

# Montar equação como texto
equacao <- paste0("Vendas = ", intercepto, " + ", inclinação, " × Temperatura\nR² = ", r2)

# Gráfico com equação no canto
ggplot(bd, aes(x = `Temperatura (graus celsius)`, y = `Total das vendas (R$)`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = equacao,
           hjust = 1.1, vjust = -0.5, size = 4, color = "black") +
  labs(title = "Regressão linear: Vendas vs Temperatura",
       x = "Temperatura (°C)",
       y = "Total das vendas (R$)") +
  theme_minimal()


# ============================================================
##### 5) Prevejam qual será o total a ser faturado com as vendas (em R$) na data de 30/out/23, sabendo que a previsao do tempo aponta para 18o C
# ============================================================
# Cálculo direto a partir da equação
previsao <- 18129.4 + 866.19 * 18
previsao
