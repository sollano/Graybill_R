

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
    
    Tab_Res_Simp <- cbind(as.data.frame(FH0), as.data.frame(Ftab), as.data.frame(pvalor), as.data.frame(alpha),Resultado)
  colnames(Tab_Res_Simp) <- c("F_H0","F Crit", "P-valor", "Alpha", "Teste")
  
  Tab_Res_Med <- as.data.frame(rbind(mean(Y1), mean(Yj), var(Y1), var(Yj), sd(Y1), sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, alpha, pvalor))
  rownames(Tab_Res_Med) <- c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "alpha",  "p valor")
  colnames(Tab_Res_Med) <- c("Resultado")
  
  aux1 <- c(round(mean(Y1),2), round(var(Y1),2), round(sd(Y1),2),  length(Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  aux2 <- c(round(mean(Yj),2), round(var(Yj),2), round(sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
  Tab_Res_Comp <- as.data.frame(cbind(aux1, aux2))
  rownames(Tab_Res_Comp) <- c("Media", "Variancia", "Desvio_Padrao", "Observacoes", "g.l.", "F_Critico", "F_H0",  "Alpha", "P-valor", "Teste", "Conclusao")
  colnames(Tab_Res_Comp) <- c("Valor_Padrao", "Valor_Proposto")
  
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

GraybillTest <- function(Y1, Yj, Tabela_Num = 0, alpha = 0.05) {
  n <- length(Y1)
  
  Sum_Y1 <- sum(Y1)
  Sum_Y1_Quad <- sum(Y1^2)
  Mean_Y1 <- mean(Y1)
  Var_Y1 <- var(Y1)
  Sd_Y1 <- sqrt(Var_Y1)
  
  Sum_Yj <- sum(Yj)
  Sum_Yj_Quad <- sum(Yj^2)
  Mean_Yj <- mean(Yj)
  Var_Yj <- var(Yj)
  Sd_Yj <- sqrt(Var_Yj)
  
  n <- length(Y1)
  Sum_Y1Yj <- sum(Y1 * Yj)
  c <- Sum_Yj^2/n
  
  b1 <- cov(Y1, Yj)/var(Y1)
  b0 <- Mean_Yj-(b1*Mean_Y1)
  
  yJlinha <- t(Yj)
  yJlinha_yJ <- yJlinha %*% Yj
  
  beta_linha <- cbind(b0,b1)
  betalinha_y1linha_yJ <- beta_linha %*% (t(cbind(rep(1, times=n), Y1)) %*% Yj)
  
  SST <- yJlinha_yJ - c
  SSR <- betalinha_y1linha_yJ - c
  SSE <- SST - SSR
  MSE <- SSE/(n-2)
  SY1Yj <- sqrt(MSE)
  r_Quad <- SSR/SST
  
  GL1 <- 1
  GL2 <- n -1 -1
  
  beta <- rbind(b0,b1)
  theta <- rbind(0,1)
  beta_theta <- beta - theta
  beta_theta_linha <- t(beta_theta)
  
  Y1linha_Y1 <- t(cbind(rep(1, times=n), Y1)) %*% cbind(rep(1, times=n), Y1)
  beta_theta_linha__Y1linha_Y1<- beta_theta_linha %*% Y1linha_Y1
  beta_theta_linha__Y1linha_Y1__beta_theta_linha <- beta_theta_linha__Y1linha_Y1 %*% beta_theta
  FH0 <- round(beta_theta_linha__Y1linha_Y1__beta_theta_linha/(2*MSE),4)
  pvalor <- round(pf(FH0,GL1,GL2,lower=F),6)
  
  Ftab <- round (qf(p=alpha, df1=2, df2=GL2, lower.tail = FALSE),4)
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")
  if(FH0 > Ftab){Conclusao <- "Yj e estatisticamente diferente de Y1, para o alpha estabelecido"}else{Conclusao <- "Yj e estatisticamente igual a Y1, para o alpha estabelecido"}
  
  Tab_Results_Num_graybill <- rbind(Mean_Y1, Mean_Yj, Var_Y1, Var_Yj, Sd_Y1, Sd_Yj, n, 2, GL2, Ftab, FH0, alpha, pvalor)
  rownames(Tab_Results_Num_graybill) <- c("Media Y1", "Media Yj", "Variancia Y1", "Variancia Yj", "Desvio Padrao Y1", "Desvio Padrao Yj", "Observacoes", "g.l 1", "g.l 2", "Ftab", "F(H0)", "alpha",  "p valor")
  colnames(Tab_Results_Num_graybill) <- c("Resultado")
  
  colY1 <- c(round(Mean_Y1,4), round(Var_Y1,4), round(Sd_Y1,4), n, 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  colYj <- c(round(Mean_Yj,4), round(Var_Yj,4), round(Sd_Yj,4), n, GL2, " ", " ", " ", " ", " ", " ")
  Tab_Results_graybill <- cbind(colY1, colYj)
  rownames(Tab_Results_graybill) <- c("Media", "Variancia", "Desvio Padrao", "Observacoes", "g.l.", "F Critico", "F(H0)", "P-valor", "Alpha","Teste", "Conclusao")
  colnames(Tab_Results_graybill) <- c("V. Padrão (Y1)", "V. Proposto (Yj)")
  
  if(Tabela_Num == 0){return(as.data.frame(Tab_Results_graybill))}else(return(as.data.frame(Tab_Results_Num_graybill)))
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
library(xlsx)

#Ler os dados
dados <- read.xlsx("dados_graybill.xlsx", sheetName = "graybill", header = TRUE)

#Teste padrão (tabela com resultados, e alpha = 0.05)
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj)

#Teste com Tabela numerica
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj, Tab = 1)

#Teste com alpha - 0.01
FdeGraybill(Y1 = dados$Y1, Yj = dados$Yj, alpha = 0.1)

# teste por grupo

library(dplyr)

dados %>%
  group_by(TALHAO) %>%
  do(FdeGraybill(.$Y1, .$Yj, Tab = 1))



# Teste com a base de dados 2
library(xlsx)

critcub <- read.xlsx("critxcub_wo_outlier.xlsx", sheetName = "crit", header = TRUE)

FdeGraybill(Y1 = critcub$vol_cubagem, Yj = critcub$vol_criterium)
FdeGraybill(Y1 = critcub$vol_cubagem, Yj = critcub$vol_criterium, Tab= 1)
FdeGraybill(Y1 = critcub$vol_cubagem, Yj = critcub$vol_criterium, alpha = 0.1)

