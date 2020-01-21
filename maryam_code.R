library(RISmed)

library(tidytext)
library(dplyr)
library(stringr)
library(wordcloud2)

# ===== FIND PUBMED STOPWORDS ======

SUMMARYmain <- EUtilsSummary("study+research",type = "esearch", db = "pubmed",
                             datetype = "edat",retmax = 10000, mindate = 2000,
                             maxdate = 2019)
Ids <- QueryId(SUMMARYmain)

# Get Download(Fetch)
MyDownloadsmain <- EUtilsGet(SUMMARYmain, type = "efetch", db = "pubmed")

#Make Data.frame of MyDownloadsmain
abstractsmain <- data.frame(title = MyDownloadsmain@ArticleTitle,
                            abstract = MyDownloadsmain@AbstractText,
                            journal = MyDownloadsmain@Title,
                            DOI = MyDownloadsmain@PMID,
                            year = MyDownloadsmain@YearPubmed)

#Character files of Abstracs
#Constract a charaterized variable for abstract of abstractsmain data.frame
abstractsmain <- abstractsmain %>% mutate(abstract = as.character(abstract))

#Split a column into tokens using the tokenizers package and pre-processing steps
CorpusofMyCloudA <- abstractsmain %>% unnest_tokens(word, abstract)
CorpusofMyCloudmain <- unique(CorpusofMyCloudA) %>% anti_join(stop_words) %>% count(word, sort = TRUE)

topWords = 500
filterList <- CorpusofMyCloudmain[order(CorpusofMyCloudmain$n, decreasing = TRUE)[1:topWords],]

# ==== SEARCH A METABOLITE TERM =====

#Get summary of NCBI EUtils query
searchTerms = ("warfarine")
SUMMARYx <- EUtilsSummary(paste0(searchTerms, collapse="+"),type = "esearch", db = "pubmed",
                          datetype = "edat",retmax = 2000, mindate = 2000,maxdate = 2019)
Idsx <- QueryId(SUMMARYx)

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
abstractsx$abstract <- as.character(abstractsx$abstract) # alternative for above line

#Without english stop words
#Split a column into tokens using the tokenizers package
CorpusofMyCloudx <- unique(abstractsx %>% unnest_tokens(word, abstract)) %>%
                    anti_join(stop_words) %>% count(word, sort = TRUE)

Wordx<-CorpusofMyCloudx$word
limittedvaluesx <- gsub("^\\d+$", "", Wordx)

filterWords <- unique(c(filterList$word, as.character(1:100), "p", searchTerms))

#CorpusofMyCloudx%>% anti_join(limittedvalues2)
filteredWords <- CorpusofMyCloudx[!(limittedvaluesx$word %in% filterWords),]

wordcloud2(filteredWords, color = "random-light", minSize= "15" , backgroundColor = "gray")
