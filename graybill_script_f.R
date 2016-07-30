### Script criado por: Sollano Rabelo Braga e Marcio Leles R. de Oliveira

### Aplicando o teste F(H0) de Graybill e graficos de analise 

## F(H0) = ((beta - theta)'*(Y1'Y1)*(beta - theta))/(2*QMRes)

# INSTRUCOES:

# Abra o arquivo dados.xslx que acompanha este script;
# Insira os valores Padrao na coluna Y1 e os valores propostos na coluna Yj;
# o alpha por padrao esta em 0.05. Para muda-lo, altere a linha 91
# Obs: E recomendado criar um novo projeto em uma pasta dedicada,
# Caso se esteja usando o R Studio.

# Este tutorial esta dividido em topicos, para melhor visualizacao, utilize:
# ctrl + o : fecha todas as foldings (topicos)
# ctrl + shift + o : abre todas as foldings (topicos)
# ou command + o e command + shift + o, caso utilize o Mac

# 0) Baixar e Carregar os pacotes necessarios ####

# faz-se o download das bibliotecas necessa?rias, caso ainda nao estejam 
# instaladas na maquina. Este passo deve ser feito apenas uma vez. 
# Obs: Remover "#" dos pacotes que nao estiverem instalados

# instala-se o pacote readxl, para poder importar dados provindos do excel, 
# sem anecessidade de conversao.

#install.packages("readxl", dependencies = T)

# instala-se o pacote ggplot2, para a criacao dos graficos.
#install.pachages("ggplot2", dependencies = T)
#install.packages("GridExtra")
#install.packages("grid")

# Carregar os Pacotes 

library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)

# 1) Importar os dados ####

# obs: colocar a planilha "dados_graybill.xlsx" na pasta do diretorio de trabalho
dados <- read_excel("graybill_dados.xlsx", 1)

# 2) Calculo do FH0, F Critico e p-valor ####

# Ajusta-se um modelo Linear Simples composto do valor proposto como y,
# e do valor padrao como x
fit <- lm(Yj ~ Y1, dados)



# Calcula-se o Quadrado medio do residuo,  obtido pela razao entre 
# a soma de quadrados de residuos e os graus de liberdade do resíduo da regressao
QMRes <- sum(residuals(fit)^2)/fit$df.residual

# Criacao da matrix 2x1 beta - theta, subtraindo os coeficientes da regressao
# pela matriz theta [0 1]
beta_theta <- coef(fit) - c(0,1)

# Calculo da Matriz Y1'Y1
# esta matriz e composta por n, Somatorio de Y1, 
# Somatorio de Y1, e o somatorio quadratico de Y1
# Utilizamos a funcao length para calcular n, e a funcao sum para calcular os somatorios
# concatena-se n e Somatorio de Y1 em um vetor, e
# Somatorio de Y1 e somatorio quadratico de Y1 em outro
# e uni-se os dois com a funcao cbind
Y1linha_Y1 <- cbind(c(length(dados$Y1), sum(dados$Y1)), 
                    c(sum(dados$Y1), sum(dados$Y1^2)))

# Calculo de FH0
# Fazemos a multiplicacao da matriz beta_theta transposta,
# pela matriz Y1'Y1, e multiplicamos isto pela matriz beta_theta;
# entao dividimos tudo isto por 2*Quadrado medio do residuo
# Arredondamos este calculo para 4 casas decimais com a funcao round()
FH0 <- round(
  (t(beta_theta)%*%Y1linha_Y1%*%beta_theta)
              /(2*QMRes),
  4)

# Definicao do alpha
alpha <- 0.05

# Calculo do F Critico utilizando os graus de liberdade do residuo,
# e arredondado para 4 casas decimais
Ftab <- round(
  qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = FALSE),
  4)

# Calculo do p-valor arredondado para 6 casas decimais
pvalor <- signif(
  pf(FH0,1,fit$df.residual,lower=F),
  4)

# 3) Teste de Hipotese ####

# Logica condicional que cria um objeto denominado resultado
# com fator "*" caso FH0 > 0, e caso contrario, "ns"
if(FH0 > Ftab)
  {Resultado <- "*"}else
    (Resultado <- "ns")
# Logica condicional semelhante a anterior
if(FH0 > Ftab)
{Conclusao <- "Yj e estatisticamente diferente de Y1, para o alpha estabelecido"}else
    {Conclusao <- "Yj e estatisticamente igual a Y1, para o alpha estabelecido"}

# 4) Tabela de Resultados Simples ####

Tab_Res_Simp <- data.frame("F_H0"    = FH0, 
                           "F_crit"  = Ftab, 
                           "P_valor" = pvalor, 
                           "Alpha"   = alpha,
                           "Teste"   = Resultado) 

# 5) Tabela de Resultados com medidas de tendencia ####

# Criacao de um dataframe composto pelas variaveis geradas
# Esta tabela nao possui fatores, apenas dados numericos
Tab_Res_Med <- data.frame(Resultado = rbind(
  mean(dados$Y1), 
  mean(dados$Yj), 
  var(dados$Y1), 
  var(dados$Yj), 
  sd(dados$Y1), 
  sd(dados$Yj), 
  length(dados$Y1), 
  2, 
  fit$df.residual, 
  Ftab, 
  FH0, 
  alpha, 
  pvalor))

rownames(Tab_Res_Med) <- c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "alpha",  "p-valor")

# 6) Tabela de Resultados completa ####

# Criacao de um dataframe composto pelas variaveis geradas
# Este dataframe possui fatores que indicam o resultado do teste
aux1 <- c(round(mean(dados$Y1),2), round(var(dados$Y1),2), round(sd(dados$Y1),2),  length(dados$Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
aux2 <- c(round(mean(dados$Yj),2), round(var(dados$Yj),2), round(sd(dados$Yj),2), length(dados$Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
Tab_Res_Comp <- data.frame("Valor_Padrao" = aux1,"Valor_Proposto" = aux2)
rownames(Tab_Res_Comp) <- c("Media", "Variancia", "Desvio_Padrao", "Observacoes", "g.l.", "F_Critico", "F_H0", "Alpha", "P-valor" ,"Teste", "Conclusao")

# 7) Grafico de dispersao ####

# Gera-se um grafico de dispersao Y1 x Yj, para avaliacao de tendencia
graph <- ggplot(data = dados, aes(x = Y1, y = Yj)) +
  geom_point(aes(), size = 3) +
  labs(x="Valor Padrão", 
       y="Valor Proposto", 
       title = "Comparação \n (Método proposto e alternativo)") +
  geom_smooth(method="lm", colour="red") +
  theme(axis.title=element_text(size=12, face= "bold" ), 
        plot.title=element_text(size=16,face="bold") ) +
  coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)),
                  ylim = c(0, max(dados$Yj + 0.3)))

# grafico alternativo com informacoes do ajuste

graph2 <- ggplot(data = dados, aes(x = Y1, y = Yj)) + 
  geom_point(aes(), size = 3) + 
  labs(x="Valor Padrão", 
       y="Valor Proposto", 
       title = paste("Adj R2 = ",round(summary(fit)$adj.r.squared, 6),
                     "Intercept =",
                     round(fit$coef[[1]],6),
                     "\n"," Slope =",
                     round(fit$coef[[2]],6), " P =",
                     signif(summary(fit)$coef[2,4],4))) + 
  geom_smooth(method="lm", colour="red") + 
  theme(axis.title=element_text(size=12, face= "bold" ), 
        plot.title=element_text(size=9) ) + 
  coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)), 
                  ylim = c(0, max(dados$Yj + 0.3)))



# 8) Resultados ####

graph
Tab_Res_Simp
Tab_Res_Med
Tab_Res_Comp

# 9) Exportar os resultados ####

#Obs: a funcao "file.choose()" so funciona no S.O. Windows
# caso necessario, pode-se especificar o caminho desejado na funcao

# Grafico
ggsave(filename = file.choose(), plot = graph)

# Tabela com resultados
write.csv(Tab_Res_Comp, file.choose())

#Tabela Numerica
write.csv(Tab_Res_Med, file.choose())

