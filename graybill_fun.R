

# Criar a funcao

FdeGraybill <- function(Y1, Yj, alpha = 0.05, Tab = 3) {
  
    fit <- lm(Yj ~ Y1)
    QMRes <- sum(residuals(fit)^2)/fit$df.residual
    beta_theta <- coef(fit) - c(0,1)
    Y1linha_Y1 <- cbind(c(length(Y1), sum(Y1)), c(sum(Y1), sum(Y1^2)))
    FH0 <- round((t(beta_theta)%*%Y1linha_Y1%*%beta_theta)/(2*QMRes),4)
   
     Ftab <- round(qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = FALSE),4)
    pvalor <- round(pf(FH0,1,fit$df.residual,lower=F),6)
   
    if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")  
    if(FH0 > Ftab){Conclusao <- "Yj e estatisticamente diferente de Y1, para o alpha estabelecido"}else{Conclusao <- "Yj e estatisticamente igual a Y1, para o alpha estabelecido"}
    
    Tab_Res_Simp <-  data.frame("F_H0"    = FH0, 
                                "F_crit"  = Ftab, 
                                "P_valor" = pvalor, 
                                "Alpha"   = alpha,
                                "Teste"   = Resultado) 
    
  Tab_Res_Med <- data.frame(Resultado = rbind(mean(Y1), mean(Yj), var(Y1), var(Yj), sd(Y1), sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, alpha, pvalor))
  rownames(Tab_Res_Med) <- c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "alpha",  "p valor")

  aux1 <- c(round(mean(Y1),2), round(var(Y1),2), round(sd(Y1),2),  length(Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  aux2 <- c(round(mean(Yj),2), round(var(Yj),2), round(sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
  
  Tab_Res_Comp <- data.frame("Valor_Padrao" = aux1,"Valor_Proposto" = aux2)
  rownames(Tab_Res_Comp) <- c("Media", "Variancia", "Desvio_Padrao", "Observacoes", "g.l.", "F_Critico", "F_H0", "Alpha", "P-valor" ,"Teste", "Conclusao")
  
  
  if(Tab==1)
    {
    return(Tab_Res_Simp)
    } 
  else if(Tab==2)
    {
      return(Tab_Res_Med)
      }
  else(return(Tab_Res_Comp))
}

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(size = 3) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

# Teste com a base de dados 1
library(readxl)

#Ler os dados
dados <- read_excel("graybill_dados.xlsx", sheet = "graybill")

#Teste padrão (tabela com resultados, e alpha = 0.05)
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj)

#Teste com Tabela numerica
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj, Tab = 1)

#Teste com alpha 0.1
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj, alpha = 0.1)

# teste por grupo

library(dplyr)

dados %>%
  group_by(TALHAO) %>%
  do(FdeGraybill(.$Y1, .$Yj, Tab = 1))

