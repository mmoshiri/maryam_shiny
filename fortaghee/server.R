
server <- function(session, input, output){

mycloud<-observeEvent(input$go, {
abstracts = getAbstracts(searchTerms = input$searchTerms,
                           mindate = input$dateRange[1],
                           maxdate = input$dateRange[2],
                           retmax = input$absFreq)
#frequencies = getWordFrequency(abstracts)
#medicalfilteredWords = getFilteredWordFreqency(frequencies, filterList)
medicalfilteredWords = getFilteredWordFreqency(frequencies = getWordFrequency(abstracts),
                                               filterList = input$filtermedical)
pureofstopword = purefiltered(medicalfilteredWords,
                             stop_words = input$filterstopwords)
})

MC<-mycloud()

output$cloud<-renderWordcloud2({
 wordcloud2 (data= MC , color = "random-light", minSize= "15" , backgroundColor = "gray")
 })
}



  #finalOutput <- wordcloud2(puredfiltered, color = "random-light", minSize= "15" , backgroundColor = "gray")
  
#output$cloud <- reactive({

#})  
  #changed by myself#frequencies = getWordFrequency(abstractsx)
  # save frequencies to new object
    # use the functions you defined
    # use input$... to get your function arguments
    # use filterWords, imported through global
 #output$cloud<- renderWordcloud2({ 
  #wordcloud2(filteredWords, color = "random-light", minSize= "15" , backgroundColor = "gray")
 #})
# PUT THIS IN SERVER!
#... stuff to get frequencies
#filtered <- frequencies
#for(filterName in input$filter){
# filterList <- .....
# getFilteredWordFreqency <- getFilteredWordFreqency(filtered, filterList)
#}
 


