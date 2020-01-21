# Don't hardcode this - a good one is the following command:
library(rstudioapi)    
# script_path = rstudioapi::getActiveDocumentContext()$path
# script_dir = dirname(script_path)
# setwd(script_dir)
#setwd("C:/Users/A   S    U    S/Desktop/shiny/maryam_shiny/uiserverglobal")

library(shiny)
library(RISmed)
library(dplyr)
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

filterList <- fread("filterList.csv")

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

#wordfrequency of whole abstract #frequencies = getWordFrequency(abstracts)#it should be # from here
getWordFrequency <- function(abstractsx){
  #Split a column into tokens using the tokenizers package
  CorpusofMyCloudx <- unique(abstractsx %>% 
                               unnest_tokens(word, abstract)) %>% count(word, sort = TRUE)
  CorpusofMyCloudx$word <- gsub("^\\d+$", "", CorpusofMyCloudx$word)
  return(CorpusofMyCloudx)
}

# frequencies <- getWordFrequency(abstractsx)#? or abstracts# why is it problem???????????????????
#filterList are medicalwords
#filterWords are summerized and uniqued of filterList
#getFilteredWordFreqency is filterd from medicalwords
getFilteredWordFreqency <- function(frequencies, filterList){
  filterWords <- unique(filterList$word)
  filteredWords <- frequencies[!(frequencies$word %in% filterWords),]
  return(filteredWords)
}

filter_storage <- list(
  medical = filterList,
  stopwords = tidytext::stop_words,
  metabolomics = data.table::data.table(word=c("metabolism", "metabolic", 
                                               "metabolomic", "metabolomics",
                                               "biochemical", "mass", "spectrometry", 
                                               "nmr", "direct", "infusion"))
)


