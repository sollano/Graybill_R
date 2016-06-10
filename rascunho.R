

checkboxGroupInput("show_vars", "Variable:", choices = names(raw_data), selected = names(raw_data))


server = function(input, output) {
  
  data1 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      colnames(my_data) <-c("Dose","Response")
      my_data$ResponseLog <- log10(my_data$Response + 1)
      my_data$ResponseSqRoot <- sqrt(my_data$Response + 1)
    })
    my_data
  })
  
  
  
  
  output$my_output_data <- renderTable({data1()},include.rownames=FALSE)  
  
}




if(input$changeorder == F)
{names(dados) <- c("Y1, Yj")}
else(names(dados) <- c("Yj, Y1") )


