library(shiny)
library(DT)
#library(shinythemes)

shinyUI(
  
  fluidPage(
  
  #theme = shinytheme("flatly"),
  theme = "bootstrap.css",
  
  titlePanel("Teste F de Graybill"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        inputId = "file1",
        
        label = "Selecione o arquivo .csv",
        
        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      
      radioButtons(inputId='sep', 
                   label='Separador',
                   choices=c(Virgula=',', "Ponto e Virgula"=';', Tab='\t'),
                   selected=';'),
      
      radioButtons(inputId='dec', 
                   label='Decimal',
                   choices=c(Ponto=".", Virgula=","),
                   selected=","),
      
     # radioButtons(inputId='quote',label='Aspas',choices=c(Nenhuma='',Simples="'",Dupla= '"'), selected='"'),
      
     # tags$hr(),
      
      actionButton("Load", "Carregue o arquivo"),
      
      #checkboxGroupInput('columns', "Selecione as variaveis:", ""),
      
     helpText("As colunas devem ser obrigatoriamente nomeadas 'Y1' e 'Yj', para valores Padrao e Proposto, respectivamente."),
     
     helpText("Para filtrar as colunas,  selecione as variaveis e clique no botao abaixo:"),
     
     selectizeInput(
       'columns', "selecione as variaveis:", choices = "",
       multiple = TRUE, options = list(maxItems = 2)  ),

     actionButton("subset", "Filtrar"),

     helpText("Para renomear as colunas, clique no botao abaixo:"),

     actionButton("rename", "Renomear"),
      
      width = 3 ), 
    
    mainPanel(
      
      withMathJax(),
      uiOutput("formula"),
      
      tabsetPanel(
        tabPanel("Dados", dataTableOutput("data")),
        tabPanel("Grafico", plotOutput("plot")), 
        tabPanel("Resultado", tableOutput("tabgraybill"))
      )
    )
  )
  
  
  
)

)
