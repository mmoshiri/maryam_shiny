setwd("C:/Users/A   S    U    S/Desktop/shiny/maryam_shiny/fortaghee")

library(shiny)
library(RISmed)
library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud2)
library(data.table)

#set filterList CSV 
# ===== FIND PUBMED STOPWORDS ======

# write.csv(filterList, row.names=F)
#SUMMARYmain <- EUtilsSummary("study+research",type = "esearch", db = "pubmed",
                           #datetype = "edat",retmax = 500, mindate = 2000,
                           #maxdate = 2019)
#Ids <- QueryId(SUMMARYmain)
# Get Download(Fetch)
#MyDownloadsmain <- EUtilsGet(SUMMARYmain, type = "efetch", db = "pubmed")

#Make Data.frame of MyDownloadsmain
#abstractsmain <- data.frame(title = MyDownloadsmain@ArticleTitle,
                            #abstract = MyDownloadsmain@AbstractText,
                            #journal = MyDownloadsmain@Title,
                            #DOI = MyDownloadsmain@PMID,
                            #year = MyDownloadsmain@YearPubmed)
#Character files of Abstracs
#Constract a charaterized variable for abstract of abstractsmain data.frame
#abstractsmain <- abstractsmain %>% mutate(abstract = as.character(abstract))
#Split a column into tokens using the tokenizers package and pre-processing steps
#CorpusofMyCloudmain <- abstractsmain %>% unnest_tokens(word, abstract)
#CorpusofMyCloudmain <- unique(CorpusofMyCloudmain) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
#topWords = 500
#filterList <- CorpusofMyCloudmain[order(CorpusofMyCloudmain$n, decreasing = TRUE)[1:topWords],]
#save("filterList", file = "filterList.csv")

filterList <- data.table::fread("filterList.csv")

getAbstracts <- function(searchTerms, retmax=500, mindate=2000, maxdate=2019){
searchTerms = strsplit(searchTerms, " ")[[1]]
  # ==== SEARCH A METABOLITE TERM =====
SUMMARYx <- EUtilsSummary(paste0(searchTerms, collapse="+"),type = "esearch", db = "pubmed",
                          datetype = "edat",retmax = 500, 
                          mindate = 2000, maxdate = 2019)
Idsx <- QueryId(SUMMARYx)
  # Get Download(Fetch)
MyDownloadsx <- EUtilsGet(SUMMARYx, type = "efetch", db = "pubmed")
  #Make Data.frame of MyDownloads
abstractsx <- data.frame(title = MyDownloadsx@ArticleTitle,
                           abstract = MyDownloadsx@AbstractText,
                           journal = MyDownloadsx@Title,
                           DOI = MyDownloadsx@PMID,
                           year = MyDownloadsx@YearPubmed)
  #Constract a charaterized variable for abstract of abstracts data.frame
abstractsx <- abstractsx %>% mutate(abstract = as.character(abstract))
  #abstractsx$abstract <- as.character(abstractsx$abstract) # alternative for above line
return(abstractsx)
}
getAbstracts<-getAbstracts("warfarin")

getWordFrequency <- function(abstractsx){
  #Split a column into tokens using the tokenizers package
  CorpusofMyCloudx <- unique(abstractsx %>% unnest_tokens(word, abstract)) %>% count(word, sort = TRUE)
  CorpusofMyCloudx$word <- gsub("^\\d+$", "", CorpusofMyCloudx$word)
  return(CorpusofMyCloudx)
}

frequencies <- getWordFrequency(getAbstracts)
# frequencies <- getWordFrequency(abstractsx)#? or abstracts# why is it problem??
getFilteredWordFreqency <- function(frequencies, filterList){
  filterWords <- unique(filterList$word)
  filteredWords <- frequencies[!(frequencies$word %in% filterWords),]
  return(filteredWords)
}

#purefiltered = medicalfilteredWords %>% anti_join(stop_words)
purefiltered<-function(medicalfilteredWords, stop_words){
  medicalfilteredWords %>% dplyr::anti_join(stop_words)
  return(puredfiltered)
}


#filter_storage <- list(
 # medical = filterList,
 # numbers = data.table(word = 1:100),
#  stopwords = tidytext::stop_words
#)

#filter_storage[["medical"]]
#filter_storage[["numbers"]]
#filter_storage[["stopwords"]]


#Joanna
#if you want the medical words
#filter_you_want = "medical"
#filter_storage[[filter_you_want]]



#for(filterName in c("medical", "numbers","stopwords")){
#  words2remove = filter_storage[[filterName]]
#  print(head(words2remove))
#}




