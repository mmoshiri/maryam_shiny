
server <- function(session, input, output){
  
  wordclouddata<-observeEvent(input$go, {
    abstracts = getAbstracts(searchTerms = input$searchTerm,
                             mindate = input$dateRange[1],
                             maxdate = input$dateRange[2],
                             retmax = input$absFreq)
    frequencies = getWordFrequency(abstractsx)
    
    # save frequencies to new object
    
    without_stopwords <- getFilteredWordFreqency(frequencies, filterList)
    
    finalOutput <- wordcloud2(frequencies, color = "random-light", minSize= "15" , backgroundColor = "gray")
    output$cloud <- renderWordcloud2({ 
      wordcloud2(filteredWords, color = "random-light", minSize= "15" , backgroundColor = "gray")
    })  
  })
}
# use the functions you defined
# use input$... to get your function arguments
# use filterWords, imported through global

# PUT THIS IN SERVER!
#... stuff to get frequencies

filtered <- frequencies
for(filterName in input$filter){
  # filterList <- .....
  filtered <- getFilteredWordFreqency(filtered, filterList)
}





server <- function(session, input, output){

  cloud<-observeEvent(input$go, {
  abstracts = getAbstracts(searchTerms = input$searchTerm,
                           mindate = input$dateRange[1],
                           maxdate = input$dateRange[2],
                           retmax = input$absFreq)
 # frequencies = getWordFrequency(abstractsx)
  
  # save frequencies to new object

  #without_stopwords <- getFilteredWordFreqency(frequencies, filterList)
  
  finalOutput <- wordcloud2(frequencies, color = "random-light", minSize= "15" , backgroundColor = "gray")
  output$cloud <- renderWordcloud2({ 
    wordcloud2(filteredWords, color = "random-light", minSize= "15" , backgroundColor = "gray")
  })  
  })
}
    # use the functions you defined
    # use input$... to get your function arguments
    # use filterWords, imported through global

# PUT THIS IN SERVER!
#... stuff to get frequencies
   
#filtered <- frequencies
#for(filterName in input$filter){
 # filterList <- .....
 # filtered <- getFilteredWordFreqency(filtered, filterList)
#}
 
 
  

