library(shiny)
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
      
      radioButtons(inputId='quote', 
                   label='Aspas',
                   choices=c(Nenhuma='',Simples="'",Dupla= '"'),
                   selected='"'),
      
     # tags$hr(),
      
      actionButton("Load", "Carregue o arquivo"),
      
      checkboxGroupInput('columns', "Selecione as variaveis:", ""),
      
      helpText("As colunas devem ser obrigatoriamente nomeadas 'Y1' e 'Yj', para valores Padrao e Proposto, respectivamente."),
      
      helpText("Para renomear as colunas, clique em um dos botoes abaixo:"),
      
      actionButton("rename", "Y1 Yj"),
      
      actionButton("changeorder", "Yj Y1"),
      
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
