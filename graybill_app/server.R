library(shiny)
library(ggplot2)
library(DT)
library(xlsx)
library(xlsxjars)

shinyServer( function(input, output,session) { # como estamos usando reactive, cria-se session
  
  outVar <- reactive({ # iremos salvar os nomes das variaveis do objeto carregado em uma funcao reativa
    
    if(input$Load == 0){return()} # se o botao load nao for pressionado, retornar nada
    inFile <- input$file1 
    if(is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    
    # Carregar o arquivo com base em input
    if(input$excel==F)
    {
      mydata <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote=input$quote)
      
    }else {
      mydata <- read.xlsx(inFile$datapath, 1)
      
    }

    names(mydata) # nomes das variaveis do arquivo carregado
  })  
  
  observe({ # Com observe iremos atualizar a lista de variaveis em selectizeInput

      updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "columns", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    })
  
  newData <- reactive({ # Criamos uma nova funcao reactive. este sera o objeto filtrado, utilizado nos calculos
    
    if(input$Load==0){return()} # se o botao load nao for pressionado(==0), retornar nada
    else(inFile <- input$file1) # caso contrario, salvar o caminho do arquivo carregado em inFile

    # input$file1 sera NULL inicialmente. apos o usuario selecionar
    # e upar um arquivo, ele sera um data frame com as colunas
    # 'size', 'type', e 'datapath' . A coluna 'datapath' 
    # ira conter os nomes dos arquivos locais onde o dado pode ser encontrado
    
    if (is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    else if(input$excel == F)
      {
      raw_data <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec,quote='"')
    } else {raw_data <- read.xlsx(inFile$datapath, 1)  }
    # Carregamos o arquivo em um objeto

    subset_data <- raw_data # Criamos uma copia do arquivo
    # este sera mostrado enquanto o usuario nao seleciona colunas com input$columns

    if(input$subset) # se o botao input#subset for apertado
    {
    subset_data <- raw_data[, input$columns] # filtar colunas com base em input$columns
    colnames(subset_data) <- c("Y1", "Yj")  # renomear variaveis
    }
    
    subset_data # tabela final a ser mostrada. 
    # Se o botao input$columns nao for pressionado, mostra os arquivos inalterados
    # caso contrario, este se torna filtrado, e o arquivo se altera
    
  })
 
  output$data <- renderDataTable({ # renderizamos uma DT::DataTable
    
   # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
  data <- newData() 
    
  datatable(data) # Criamos uma DT::datatable com base no objeto
  
  # Este arquivo e reativo, e ira se alterar caso o usuario
  # aperte o botao input$columns
  
  })
  
  output$tabgraybill <- renderTable({ # rendereizamos uma tabela normal
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    
    dados <- newData() 
    
    if(is.null(dados)){return(NULL)} # se o arquivo nao for carregado, retornar null
    # evita mensagens de erro caso o arquivo nao esteja carregado ainda
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
    # O resto deste script se baseia no nome das colunas, Yj e Y1
    # ou seja, equanto o usuario nao apertar input$columns,
    # este codigo nao ira funcionar
    
    # Calculo do FH0, F Critico e p-valor ####
    
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
      qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = F),
      4)
    
    # Calculo do p-valor arredondado para 6 casas decimais
    pvalor <- signif(
      pf(FH0,df1=2,df2=fit$df.residual, lower.tail = F),
      4)
    
    # Teste de Hipotese ####
    
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
    
    Tab_Res_Comp <- data.frame("Valor Padrao" = aux1,"Valor Proposto" = aux2)
    rownames(Tab_Res_Comp) <- c("Media", "Variancia", "Desvio Padrao", "Observacoes", "g.l.", "F Critico", "F(H0)", "Alpha", "P-valor" ,"Teste", "Conclusao")
    
    
   Tab_Res_Comp
    
  })
  
  output$plot <- renderPlot({ # Renderizamos um grafico
    
  
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    
    dados <- newData()
    
    if(is.null(dados)){return(NULL)} # se o arquivo nao for carregado, retornar null
    # evita mensagens de erro cas o o arquivo nao esteja carregado ainda
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
    # O resto deste script se baseia no nome das colunas, Yj e Y1
    # ou seja, equanto o usuario nao apertar input$columns,
    # este codigo nao ira funcionar
    
    # utilizando o pacote ggplot2, renderizamos um grafico de dispersao simples
    
    ggplot(data = dados, aes(x = Y1, y = Yj)) + # dados e variaveis utilizadas
      geom_point(aes(), size = 3) + # grafico de dispersao
      labs(x="Valor Padrao", # titulo eixo x
           y="Valor Proposto", # titulo eixo y
           title = "Comparacao \n (Metodo proposto e alternativo)") + # titulo do grafico
      geom_smooth(method="lm", colour="red") + # linha do ajuste
      theme(axis.title=element_text(size=12, face= "bold" ),  # tamanho da letra e tipo da letra dos eixos
            plot.title=element_text(size=16,face="bold") ) + # tamanho da letra e tipo da letra do titulo
      coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)), # alteracao da escala
                      ylim = c(0, max(dados$Yj + 0.3)))
    
  })
  
  output$formula <- renderUI({ 
    
    # renderizamos a formula do teste, apenas para efeito visual
    # utilizamos a funcao withMathJax, e a liguagem LaTeX.
    withMathJax(
      h3("$$ F(H_{0})=\\frac{(\\hat{\\beta}-\\theta)'(Y'_{1} Y_{1})(\\hat{\\beta}-\\theta)}{2QMRes}  \\sim F_\\alpha (2,n-2 \\phantom{1}g.l.) $$")
    )
    
  })
  
  
  })
  

  
