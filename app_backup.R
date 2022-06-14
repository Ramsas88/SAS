ui <- fluidPage(
  h1("File copy app",align= "center"),
  
  
  fluidRow(column(4,
          textInput("copy_from", "Enter the Path copy from", "D:/R/CDISC_Study/outputs1")),
  
  
          column(4,offset=4,
          textInput("copy_to", "Enter the Path copy to", "D:/R/CDISC_Study/outputs")),
  ),
  
  
 fluidRow(column(4, textInput("patt1","files Select. Ex: .pdf,.rtf",".pdf")),
          column(4, offset=4,textInput("patt2","files delete. Ex: .pdf,.rtf",".pdf"))
   
 ),
  
#output  
  
  fluidRow(column(4,
          verbatimTextOutput("from")),
  
  column(4,offset=4,
          verbatimTextOutput("to"))
  ),

fluidRow(column(4,
                 actionButton("goButton1", "Copy",icon=icon("copy"))),

         column(4,offset=4,
                 actionButton("goButton2", "Delete",icon=icon("trash")))
)

)


server <- function(input, output,session) {
  
  #val <- reactive({input$patt})
  
  
  output$from <- renderPrint({ list.files(input$copy_from,pattern=input$patt1)})
  
  observeEvent(input$goButton1, {
   
  # file_copy(copy_from=input$copy_from,
  #             copy_to= input$copy_to,
  #             files=c(list.files(input$copy_from,pattern=input$patt1))
  #          )
    # file.copy(from="D:/R/CDISC_Study/outputs1/14-4.01.rtf",
    #           to="D:/R/CDISC_Study/outputs/",copy.date=T)
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
 
  })
    
  output$to <- renderPrint({ list.files(input$copy_to,pattern=input$patt2) })
  #print(renderPrint({input$submitButton}))
}
shinyApp(ui, server)

