library(shiny)
library(ggplot2)
library(DT)
library(xlsx)
library(xlsxjars)
library(plotly)

FdeGraybill_ <- function(df, Y1, Yj, alpha = 0.05, Tab = 3) {
  
  Y1 <- df[[Y1]]
  Yj <- df[[Yj]]
  
  fit <- lm(Yj ~ Y1)
  QMRes <- sum(residuals(fit)^2)/fit$df.residual
  beta_theta <- coef(fit) - c(0,1)
  Y1linha_Y1 <- cbind(c(length(Y1), sum(Y1)), c(sum(Y1), sum(Y1^2)))
  FH0 <- round((t(beta_theta)%*%Y1linha_Y1%*%beta_theta)/(2*QMRes),4)
  
  Ftab <- round(qf(p=alpha, df1=2, df2=fit$df.residual, lower.tail = FALSE),4)
  pvalor <- signif(pf(FH0,2,fit$df.residual,lower=F),4)
  
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")  
  if(FH0 > Ftab){Conclusao <- "V. Proposto é estatisticamente diferente de V.Padrão, para o n. de significância estabelecido"}else{Conclusao <- "V. Proposto é estatisticamente igual a V. Padrão, para o n. de significância estabelecido"}
  
  Tab_Res_Simp <-  data.frame("F_H0"    = FH0, 
                              "F_crit"  = Ftab, 
                              "P_valor" = pvalor, 
                              "Alpha"   = alpha,
                              "Teste"   = Resultado) 
  
  Tab_Res_Med <- data.frame(Resultado = rbind(mean(Y1), mean(Yj), var(Y1), var(Yj), sd(Y1), sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, alpha, pvalor),
                            row.names = c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "alpha",  "p valor") )
  
  aux1 <- c(round(mean(Y1),2), round(var(Y1),2), round(sd(Y1),2),  length(Y1), 2, Ftab, FH0, alpha, pvalor, Resultado, Conclusao)
  aux2 <- c(round(mean(Yj),2), round(var(Yj),2), round(sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
  
  Tab_Res_Comp <- data.frame("Valor Padrão" = aux1,
                             "Valor Proposto" = aux2, 
                             row.names = c("Média", "Variância", "Desvio Padrão", "Observações", "grau de liberdade", "F-Crítico", "F(H0)", "Nível de significância", "P-valor" ,"Teste", "Conclusão") )
  
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

shinyServer( function(input, output,session) { # como estamos usando reactive, cria-se session
  
  outVar <- reactive({ # iremos salvar os nomes das variaveis do objeto carregado em uma funcao reativa
    
    if(input$Load == 0){return()} # se o botao load nao for pressionado, retornar nada
    inFile <- input$file1 
    if(is.null(inFile)){return(NULL)} # se o arquivo nao for carregado, retornar null
    
    # Carregar o arquivo com base em input
    if(input$excel==F)
    {
      mydata <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec)
      
    }else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."));
      mydata <- readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), 1)       
    }

    names(mydata) # nomes das variaveis do arquivo carregado
  })  
  
  observe({ # Com observe iremos atualizar a lista de variaveis em selectizeInput

      updateSelectizeInput( # funcao que atualiza um SelectizeInput
      session, # sessao
      "columns", # Id do SelecizeInput que sera atualizado
      choices = outVar()) # lista de opcoes. No caso, nomes das variaveis do arquivo carregado pelo usuario
    })
  
  observe({ # este observe muda a tab selecionada para dados
    # caso o usuário carregue os dados (clicando no action button Load)
    if (input$Load) updateTabsetPanel(session, "tabs", selected = "Dados")
     
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
    } else {
      file.copy(inFile$datapath,
                paste(inFile$datapath, "xlsx", sep="."));
      raw_data <- readxl::read_excel(paste(inFile$datapath, "xlsx", sep="."), 1)       
    }
    # Carregamos o arquivo em um objeto

    subset_data <- raw_data # Criamos uma copia do arquivo
    # este sera mostrado enquanto o usuario nao seleciona colunas com input$columns

    if(input$run) # se o botao input#subset for apertado
    {
    subset_data <- raw_data[, input$columns] # filtar colunas com base em input$columns
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
  
  tabgraybill <- reactive({ # renderizamos uma tabela normal
    
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    if(is.null(newData() ) ){return()}
    dados <- newData() 
    
    x <- FdeGraybill_(dados, input$columns[1], input$columns[2], alpha = input$alpha)
    
    x
    
  })
  
  output$tablegraybill <-renderDataTable({ 
    
    x <- tabgraybill() 
    
    datatable(x, options = list(searching = FALSE,
                                          paging=FALSE ) )
    
    })

  graph <- reactive({ # Renderizamos um grafico
    
  
    # salvamos a funcao newData, que contem o arquivo carregado pelo usuario em um objeto
    
    dados <- newData()
    
    if(is.null(dados)){return()} # se o arquivo nao for carregado, retornar null
    # evita mensagens de erro cas o o arquivo nao esteja carregado ainda
    
    dados <- dados[,input$columns] # filtrar dataframe
    names(dados) <- c("Y1", "Yj") # renomear dataframe
    
    # Este arquivo e reativo, e ira se alterar caso o usuario
    # aperte o botao input$columns
    
    # O resto deste script se baseia no nome das colunas, Yj e Y1
    # ou seja, equanto o usuario nao apertar input$columns,
    # este codigo nao ira funcionar
    
    # utilizando o pacote ggplot2, renderizamos um grafico de dispersao simples
    
   graph <- ggplot(data = dados, aes(x = Y1, y = Yj)) + # dados e variaveis utilizadas
      geom_point(size = 3) + # grafico de dispersao
      labs(x="Valor Padrão", # titulo eixo x
           y="Valor Proposto", # titulo eixo y
           title = "Comparação \n (Valor Proposto x Padrão)") + # titulo do grafico
      geom_smooth(method="lm", colour="red",se=F) + # linha do ajuste
      theme(axis.title=element_text(size=12, face= "bold" ),  # tamanho da letra e tipo da letra dos eixos
            plot.title=element_text(size=16,face="bold", hjust = 0.5) ) #+ # tamanho da letra e tipo da letra do titulo
    #  coord_cartesian(xlim = c(0, max(dados$Y1 + 0.3)), # alteracao da escala
                     # ylim = c(0, max(dados$Yj + 0.3)))
    
   graph
   
  })
  
  output$plot <- plotly::renderPlotly({ 
    
   if(is.null(graph()) ) return(NULL)
    
     g <- graph()
     
     plotly::ggplotly(g)
    
   
    
  })
  
  output$formula <- renderUI({ 
    
    # renderizamos a formula do teste, apenas para efeito visual
    # utilizamos a funcao withMathJax, e a liguagem LaTeX.
    withMathJax(
      h3("$$ F(H_{0})=\\frac{(\\hat{\\beta}-\\theta)'(Y'_{1} Y_{1})(\\hat{\\beta}-\\theta)}{2QMRes}  \\sim F_\\alpha (2,n-2 \\phantom{1}g.l.) $$")
    )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      
        "f_graybill_teste.xlsx"
      
    },
    
    content = function(file) {

      
        xlsx::write.xlsx2(as.data.frame( tabgraybill() ), file, row.names = F)
      
      
      
      
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      
      
      "grafico_teste_f_graybill.png"
      
    },
    
    content = function(file) {
      
      
      ggsave(file, graph(), width = 12, height = 6)
      
      
      
      
    }
  )
  
  
  
  
  })
  

  
