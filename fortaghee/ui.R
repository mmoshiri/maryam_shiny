ui <- fluidPage(
  titlePanel("Word Cloud Plot"),
  sidebarLayout(
    sidebarPanel(
      helpText("Type a metabolite below and search PubMed to find documents that contain that word in the text."),
      textInput("searchTerms", label = h3("Enter your search terms"), value = "enter your search terms"),
      hr(),
      #dateRangeInput('dateRange',label = 'Date range input: yyyy-mm-dd',start = Sys.Date() - 2, end = Sys.Date() + 2),
      helpText("You can specify the start and end years of your search, use the format YYYY"),
      sliderInput(inputId = "dateRange", label = "Date range input:",min = 2000, max = 2019,value = c(2010, 2020)),
      sliderInput("absFreq", "AbsFrequency:", min = 1,  max = 4000, value = 500),
      #selectizeInput("filter","Filter",choices = c("medical","numbers","stopwords"),multiple = T),
      selectizeInput("filterstopwords","FilterStowprds","Filter for Stop Words",multiple = F),
      selectizeInput("filtermedical","FilterMedical","Filter for Medical Words",multiple = F),
      #selectizeInput("filternumbers","FilterNumbers","Filter for Numbers",multiple = F),
      actionButton("go", "Go! :)"),
      hr(),
    ), 
    mainPanel(
      verbatimTextOutput("absFreq"),
     verbatimTextOutput("filter"),
      verbatimTextOutput("dateRange"),
      wordcloud2Output(outputId = "cloud")
  
    )
  )
)

