library(shiny)
library(ggplot2)
library(DT)


shinyServer( function(input, output,session) {
  
  outVar <- reactive({
    
    if(input$Load == 0){return()}
    inFile <- input$file1
    if(is.null(inFile)){return(NULL)}
    
    mydata <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote=input$quote)
    
    names(mydata)
  })  
  
  observe({
    #updateCheckboxGroupInput(session, "columns", choices = outVar())
    updateSelectizeInput(session,"columns",choices = outVar())
    })
  
  newData <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    input$Load
    raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    
    subset_data <- raw_data

    if(input$subset)
    {
    subset_data <- raw_data[, input$columns]
    colnames(subset_data) <- c("Y1", "Yj")  }
    
    subset_data
    
  })
 
  output$data <- renderDataTable({
    
  data <- newData()
    
  datatable(data)
   
  })
  
  output$tabgraybill <- renderTable({
    
    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
      dados <- newData()
    
    
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
    
    aux1 <- c(round(mean(dados$Y1),2), round(var(dados$Y1),2), round(sd(dados$Y1),2),  length(dados$Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
    aux2 <- c(round(mean(dados$Yj),2), round(var(dados$Yj),2), round(sd(dados$Yj),2), length(dados$Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
    Tab_Res_Comp <- as.data.frame(cbind(aux1, aux2))
    rownames(Tab_Res_Comp) <- c("Media", "Variancia", "Des. Padrao", "Observacoes", "g.l.", "F Critico", "F(H0)", "Alpha", "P-valor" ,"Teste", "Conclusao")
    colnames(Tab_Res_Comp) <- c("Valor Padrao", "Valor Proposto")
    
    Tab_Res_Comp
    
  })
  
  output$plot <- renderPlot({
    
   # inFile <- input$file1
    
   # if (is.null(inFile))
     # return(NULL)
    
   # dados <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec, quote=input$quote)
    
    dados <- newData()
    

    
    ggplot(data = dados, aes(x = Y1, y = Yj)) +
      geom_point(aes(), size = 3) +
      labs(x="Valor Padrao", 
           y="Valor Proposto", 
           title = "Comparacao \n (Metodo proposto e alternativo)") +
      geom_smooth(method="lm", colour="red") +
      theme(axis.title=element_text(size=12, face= "bold" ), 
            plot.title=element_text(size=16,face="bold") ) +
      coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)),
                      ylim = c(0, max(dados$Yj + 0.3)))
    
  })
  
  output$formula <- renderUI({
    
    withMathJax(
      h3("$$ F(H_{0})=\\frac{(\\hat{\\beta}-\\theta)'(Y'_{1} Y_{1})(\\hat{\\beta}-\\theta)}{2QMRes}  \\sim F_\\alpha (2,n-2 \\phantom{1}g.l.) $$")
    )
    
  })
  
  
  })
  

  
