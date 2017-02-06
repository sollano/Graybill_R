

# Teste com a base de dados 1
library(readxl)
library(forestr)

#Ler os dados
dados <- read_excel("graybill_dados.xlsx", sheet = "graybill")

#Teste padrão (tabela com resultados, e alpha = 0.05)
FdeGraybill(dados, Y1 = "Y1", Yj = "Yj")
FdeGraybill_(dados, Y1 = "Y1", Yj = "Yj")

#Teste com Tabela numerica
FdeGraybill(dados, Y1 = "Y1", Yj = "Yj", Tab = 1)

#Teste com alpha 0.1
FdeGraybill(dados, Y1 = "Y1", Yj = "Yj", alpha = 0.1)

# teste por grupo

library(dplyr)

dados %>%
  group_by(TALHAO) %>%
  do(FdeGraybill(., "Y1", "Yj", Tab = 1))

