setwd("C:/Users/A   S    U    S/Desktop/shiny/maryam_shiny")
library(shiny)
library(RISmed)
library(dplyr)
library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud2)

ui<-fluidPage(
  titlePanel("Word Cloud Plot"),
  sidebarLayout(
    sidebarPanel(
    textInput("searchTerms", label = h3("Enter Your Metabolite"), value = "enter metabolite..."),
    br(),
    actionButton("Updates", "Update"),
    hr(),
    #dateRangeInput('dateRange',label = 'Date range input: yyyy-mm-dd',start = Sys.Date() - 2, end = Sys.Date() + 2),
    sliderInput(inputId = "dateRange", label = "Date range input:",min = 2000, max = 2020,value = c(2010, 2020)),

    sliderInput("AbsFreq", "AbsFrequency:", min = 1,  max = 4000, value = 1000),
    actionButton("Updates", "Update"),
    hr(),
   #textAreaInput("text", "Enter text", rows = 10),
  ), 
  mainPanel(
    verbatimTextOutput("dateRange"),
    wordcloud2Output(outputId = "cloud")
  )
)
)

#set filterList CSV 
# ===== FIND PUBMED STOPWORDS ======
save("filterList", file = "filterList.csv")
write.csv(filterList, row.names=F)
filterList<-read.csv(file="filterList.csv", header=TRUE, sep=",") 
# ==== SEARCH A METABOLITE TERM =====
searchTerms = c("aspirin")
SUMMARYx <- EUtilsSummary(paste0(searchTerms, collapse="+"),type = "esearch", db = "pubmed",
                          datetype = "edat")
# Get Download(Fetch)
MyDownloadsx <- EUtilsGet(SUMMARYx, type = "efetch", db = "pubmed")
#Make Data.frame of MyDownloads
abstractsx <- data.frame(title = MyDownloadsx@ArticleTitle,
                         abstract = MyDownloadsx@AbstractText,
                         journal = MyDownloadsx@Title,
                         DOI = MyDownloadsx@PMID,
                         year = MyDownloadsx@YearPubmed)
#Character files of Abstracs
#Constract a charaterized variable for abstract of abstracts data.frame
abstractsx <- abstractsx %>% mutate(abstract = as.character(abstract))
#abstractsx$abstract <- as.character(abstractsx$abstract) # alternative for above line
#Split a column into tokens using the tokenizers package
CorpusofMyCloudx <- unique(abstractsx %>% unnest_tokens(word, abstract)) %>%
  anti_join(stop_words) %>% count(word, sort = TRUE)

Wordx<-CorpusofMyCloudx$word
limittedvaluesx <- gsub("^\\d+$", "", Wordx)

filterWords <- unique(c(filterList$word, as.character(1:100), "p", searchTerms))

#CorpusofMyCloudx%>% anti_join(limittedvalues2)
filteredWords <- CorpusofMyCloudx[!(CorpusofMyCloudx$word %in% filterWords),]

server<-function(session, input, output){
reactive(filteredWords)
output$cloud <- renderWordcloud2({ 
wordcloud2(data=filteredWords,color = "random-light", minSize= "15" , backgroundColor = "gray")
})
}



shinyApp(ui = ui, server = server)
