# This is the minimum viable product for the app.
# Add error handling code, and your own enhancements

library(shiny)
source('ngram_model.R')
loadNgram()

lastWord <- function (term) {
  words <- unlist(strsplit(term, " "))
  gsub(" ", '', words[length(words)])
}

shinyServer(function(input, output) {
  dataInput <- reactive({
    predict(input$entry, input$show)
  })
  output$words <- renderText({paste0(sapply(dataInput()$term, lastWord), collapse = ', ')})
  output$table <- renderTable(dataInput(), digits = 5, auto = T)
  
  output$sent <- renderText({
    input$entry
  })
})