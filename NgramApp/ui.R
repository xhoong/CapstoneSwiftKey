# This is the minimum viable product for the app.
# Add error handling code, and your own enhancements

library(shiny)

textareaInput <- function(id, label, value, rows=20, cols=35, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

shinyUI(fluidPage(theme = "bootstrap.css",#get more bootstrap themes from http://bootswatch.com/
  # Set the page title
  titlePanel("Data Science Capstone: Word Predictor Using Back-Off and Generalized Linear Interpolation"),
  
  sidebarPanel(
    textareaInput("entry","Text Area Entry", "Capstone SwiftKey project with word prediction analysis ", rows = 5, cols = 35),
    sliderInput("show", "Predicted words to show:", 1, 10, 3, step=1),
    submitButton("SUBMIT"),
    br()
    
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Instruction", 
                         h4("Usage"),
                         p("You can input a sentence in the top left panel for next word prediction. When ready, press SUBMIT. You will see:"),
                         span(h4(textOutput('sent')),style = "color:blue"),
                         p('as the input and'),
                         verbatimTextOutput('words'),
                         p("as predicted words sorted by most probable interpolated term probability.",
                            "Table below show the complete NGRAM terms and it's properties,",
                            "tf (ngram frequency), tp (ngram Markov Chain Probability in log),",
                            "glmtp (generalized language model term probability)"),
                         span(h4(fluidRow(
                           column(3,
                                  tableOutput('table')
                           )
                         )),style = "color:blue")
                        ),
                tabPanel("Source")
               
                )
               
                
    ))
)