library(shiny)
library(shinydashboard)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

#The main part of shinyUI
shinyUI(dashboardPage(
    skin = "blue",
    dashboardHeader(title ="Text Sentiment Analysis - Assignment 2, Yao Chen",
                    titleWidth = 500
                    ),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem("Introduction", tabName = "introduction", icon = icon("info")),
            menuItem("About Lexicons", tabName = "lexicon", icon = icon("book"),
                     menuItem("Lexicon Info", tabName = "lexicon_info"),
                     menuItem("HarryPotter with different lexicons",tabName = "HP_with_lexicons")
            ),
            menuItem("Analysis", tabName = "analysis", icon = icon("autoprefixer"),
               menuItem("Basic Sentiment Analysis", tabName = "basic"),
               menuItem("Explore More", tabName = "more") 
            ),
            menuItem("References", tabName = "references", icon = icon("book"))
            )
            
        ),
    dashboardBody(
        # Also add some custom CSS to make the title background area the same
        # color as the rest of the header.
        tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
        tabItems(
            # First main tab content
            tabItem(
              tabName = "introduction",
              h2("Introduction of Text Sentiment"),
              br(),
              fluidRow(
                  box(width = 8,
                      title = "What is Text Sentiment?", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      p("Sentiment analysis is a type of text mining which aims to determine the 
                      opinion and subjectivity of its content. When applied to lyrics, the results can
                      be representative of not only the artist's attitudes, but can also reveal 
                      pervasive, cultural influences."
                        ,style = "font-family: 'calibri'; font-size:18pt")
                  )
              ),
              fluidRow(
                  box(width = 8,
                    title = "What is Text Sentiment used for?", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,collapsed = TRUE,
                    p("The applications of sentiment analysis are broad and powerful. The ability to 
                    extract insights from social data is a practice that is being widely adopted by 
                    organisations across the world."
                    ,style = "font-family: 'calibri';font-size:18pt"),
                    p("Shifts in sentiment on social media have been shown to correlate with shifts in
                      the stock market."
                      ,style = "font-family: 'calibri';font-size:18pt"),
                    p("The Obama administration used sentiment analysis to gauge public opinion to 
                    policy announcements and campaign messages ahead of 2012 presidential election. "
                      ,style = "font-family: 'calibri';font-size:18pt"),
                    p("It can also be an essential part of your market research and customer service 
                    approach. Not only can you see what people think of your own products or services,
                    you can see what they think about your competitors too. "
                      ,style = "font-family: 'calibri';font-size:18pt"),
                    p("Basically, we can use sentiment analysis to track the sentiment plot in one of
                      our favourite novels or songs."
                      ,style = "font-family: 'calibri';font-size:18pt")
                  )                  
              ),
              fluidRow(
                  box(width = 8,
                      title = "Methods to do Text Sentiment", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      p("There are different methods used for sentiment analysis, including training a
                      known dataset, creating your own classifiers with rules, and using predefined 
                      lexical dictionaries (lexicons). In this ShinyApp, we will use the lexicon-based
                      approach."
                        ,style = "font-family: 'calibri';font-size:18pt")
                  )
              ),
              fluidRow(
                  box(width = 8,
                      title = "Steps to do Text Sentiment", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,collapsed = TRUE,
                      p("-Which lexicon should you use to determine how many positive and negative 
                        words are in the word_tb dataset?"
                        ,style = "font-family: 'calibri';font-size:18pt"),
                      p("-How many positive and negative words are in the dataset? What are they?"
                        ,style = "font-family: 'calibri';font-size:18pt"),
                      p("-Can you get word counts for each category of sentiment type for each author
                        ?"
                        ,style = "font-family: 'calibri';font-size:18pt")
                  )
              ),
              fluidRow(
                box(width = 12,
                p("In this ShinyApp, I try to use lexicon to analyse the sentiment of HarryPotter, 
                which is one of my favourate novel series. The basic motivation behind this sentiment 
                analysis is to assess how positive or negative text is, based on a dictionary of words
                that have been previously classified as positive or negative."
                  ,style = "font-family: 'calibri';font-size:18pt")
                )
              )
            ),
           # Second main tab content
            tabItem(tabName = "lexicon_info",
                    h2("Lexicons Info"),
                    fluidRow(
                      box(width = 12,
                      p("In this ShinyApp I use the Lexicon in tidytext package, including a dataset
                      called sentiments which provides several distinct lexicons. These lexicons 
                      are dictionaries of words with an assigned sentiment category or value.tidytext
                      provides three general purpose lexicons:"
                        ,style = "font-family: 'calibri';font-size:16pt"),
                      p(strong("-AFINN:"), "assigns words with a score that runs between -5 and 5,
                      with negative scores indicating negative sentiment and positive scores 
                      indicating positive sentiment."
                        ,style = "font-family: 'calibri';font-size:16pt"),
                      p(strong("-Bing:"), "assigns words into positive and negative categories."
                        ,style = "font-family: 'calibri';font-size:16pt"),
                      p(strong("-NRC:"), "assigns words into one or more of the following ten 
                      categories: positive, negative, anger, anticipation, disgust, fear, joy, sadness
                        , surprise, and trust."
                        ,style = "font-family: 'calibri';font-size:16pt")
                    )
                    ),
                    fluidRow(
                        box(title = "Afinn",width = 4, status = "primary",solidHeader = TRUE,
                            verbatimTextOutput('afinn')),
                        box(title = "Bing",width = 4,status = "primary",solidHeader = TRUE,
                            verbatimTextOutput('bing')),
                        box(title = "NRC",width = 4,status = "primary",solidHeader = TRUE,
                            verbatimTextOutput('nrc'))
                    ),
                    fluidRow(
                      box(width = 12,status = "warning",
                          p("Note: these sentiment lexicons were constructed via either crowdsourcing 
                          (using, for example, Amazon Mechanical Turk) or by the labor of one of the
                          authors, and were validated using some combination of crowdsourcing again, 
                          restaurant or movie reviews, or Twitter data. Given this information, we may
                          hesitate to apply these sentiment lexicons to styles of text dramatically 
                          different from what they were validated on, such as narrative fiction from 
                          200 years ago. While it is true that using these sentiment lexicons with, 
                          for example, Jane Austen's novels may give us less accurate results than 
                          with tweets sent by a contemporary writer, we still can measure the 
                          sentiment content for words that are shared across the lexicon and the text
                          ."
                          ,style = "font-family: 'calibri';font-size:16pt")
                      ) 
                    )
            ),
           tabItem(tabName = "HP_with_lexicons",
                   h2("The sentiment of Harry Potter novels measured by three lexicons"),
                   br(),
                   tabsetPanel(
                     tabPanel("HP words found in Lexicons",
                     box(width = 12,
                     p("Generally speaking, based on a lexicon analysis, we would better choose a 
                     suitable lexicon. First of all, we should figure out how many words there are 
                     both in the novel and in the lexicon:"
                       ,style = "font-family: 'calibri';font-size:16pt"),  
                     tableOutput("lexicon_hp"),
                     p("As we can see, of the 24475 distinct words from HP, the Bing and NRC lexicon 
                     has more records than AFINN. Notice the sum of the match ratios is low. No 
                     lexicon could have all words, nor should they. Many words are considered 
                     neutral and would not have an associated sentiment. So there is an associated 
                     fear that exists in the novel but is not captured in sentiment analysis using 
                     typical lexicons."
                     ,style = "font-family: 'calibri';font-size:16pt")
                     )
                   ),
                   tabPanel("HP sentiment by Lexicons",
                     box(width = 12,
                         p("Secondly, we can see how the three lexicons work on the novels through 
                         sentiment tends plots in each novel of HarryPotter. This plot calculate the 
                         sentiment score of every 500 words(assume that every page contain 500 words),
                         for AFINN, the score is the sum of the 500 words sentiment score; for Bing 
                         and NRC(only choose 'negative' and 'positive'), the score is the difference 
                         between amount of positve and amount of negative words, i.e. amount of 
                         positive - amount of negative. Therefore, the sentiment of one page is more 
                         positive the score (y-axis) is higher. "
                           ,style = "font-family: 'calibri';font-size:16pt"),
                       plotOutput("lexicon_diff"),
                   p("This output also allows us to compare across novels. First, you get a good sense
                   of differences in book lengths - Order of the Pheonix is much longer than Philosoph
                   er's Stone. Second, you can compare how books differ in their sentiment (both 
                   direction and magnitude) across a series."
                     ,style = "font-family: 'calibri';font-size:16pt"),
                   p("The three different lexicons for calculating sentiment give results that have 
                   fairly similar relative trajectories through the novels. We see similar dips and 
                   peaks in sentiment at about the same places in the novel, but the absolute values 
                   are significantly different. In some instances, it apears the AFINN lexicon finds 
                   more positive sentiments than the NRC lexicon."
                     ,style = "font-family: 'calibri';font-size:16pt"),
                   p("Considering these results, I used Bing in the following examples."
                     ,style = "font-family: 'calibri';font-size:16pt")
                     )
                   )
                   )
           ),
           #third main tab content
           tabItem(tabName = "basic",
                   titlePanel("Basic Sentiment Analysis"),
                     fluidRow(
                       box(width = 12,
                           p("Basic sentiment analysis is created on the word sentiment. We firstly 
                           split the text into words and give each word a sentiment, then we can find 
                           the most positive word or most negative word. Furthermore, we can analyse 
                           the sentiment of a whole sentence of text through some algorithm of the 
                           words."
                             ,style = "font-family: 'calibri';font-size:16pt"))
                     ),
                     fluidRow(
                         box(width = 12,
                             column(3,
                                    radioButtons("book_title","Please select a book",choices = titles, 
                                          selected = "Philosopher's Stone"
                                          )
                                    ),
                             column(3,
                                    imageOutput("image")
                                    )
                         ),
                         box(width = 12,
                             checkboxInput("stop_words", label = "Delete stop-words", value = TRUE),
                             p("A stop word is a commonly used word (such as 'the', 'a', 'an', 'in') 
                                that some times need to be ignored. Here I used the stop-words list in the 
                                package. "
                               ,style = "font-family: 'calibri';font-size:14pt"),
                             tabsetPanel(
                               tabPanel("Word Count",
                                        plotOutput("word_count"),
                                        p("This two graphs show the most positive words and negative 
                                           words in the chosen novel. For example, in Philosopher's Stone,
                                           the leading two negative word is dark and fell, if we have read 
                                           the novel or see the movie, we can assume that they may be used 
                                           most in describing the tiny room where Harry Potter found the 
                                           magic stone."
                                          ,style = "font-family: 'calibri';font-size:14pt")
                               ),
                               tabPanel("Word cloud",
                                        plotOutput("word_cloud"),
                                        p("This wordcloud shows integrate the words most used in the 
                                           novel, in which red represent the positive words while blue 
                                           represent the negative words."
                                          ,style = "font-family: 'calibri';font-size:14pt")
                               ),
                               tabPanel("Plot Development",
                                        plotOutput("plot_development"),
                                        p("This sentiment thermal map through each chapter indicates how
                                           the sentiment changes during the process of plot development. 
                                           Notice, this algorithem is different from the one in 'Explore 
                                           More' part. This map used the word sentiment analysis to   
                                           calculate the sentiment score of a sentence."
                                          ,style = "font-family: 'calibri';font-size:14pt")
                               )
                         )
                       )
                     )
           ),
           tabItem(tabName = "more",
                   h2("sentence sentiment"),
                   br(),
                   fluidRow(
                     box(width = 12,
                      p("Lots of useful work can be done by tokenizing at the word level, but 
                      sometimes it is useful or necessary to look at different units of text. For 
                      example, 'I am not happy', in a common word sentitment analysis, we probably 
                      give it a positive score because of the word 'happy', but we ignore there is a 
                      'not', so in most cases we would want to figure out the sentiment of this whole 
                      sentence. Fortunately, now we have many tools to do it and 'sentimentr' package 
                      is one of them.Sentimentr attempts to take into account valence shifters (i.e.,
                      negators, amplifiers (intensifiers), de-amplifiers (downtoners), and adversative
                      conjunctions) while maintaining speed. "
                        ,style = "font-family: 'calibri';font-size:16pt")
                     )
                   ),
                   fluidRow(
                     box(width = 3,
                     selectInput("book_sentence","Please select a book",choices = titles, 
                                 selected = "Philosopher's Stone")
                     )
                   ),
                   fluidRow( 
                     box(
                     plotOutput("chapter_sentence"),
                     p("This plot shows the sentiment of sentence distribution of each chapter, the 
                     red dot is the general sentiment of a chapter. For example, in Philosopher's 
                     Stone, the most pistive chapter is the 5th and the most negative moods apeared in
                     the 4th chapter, this is an interesting point that J. K. Rowling set a roller 
                     coaster here."
                       ,style = "font-family: 'calibri';font-size:14pt")
                     ),
                     box(
                     plotOutput("plot_sentence"),
                     p("This plot can evidently show the development of sentiment in the reading 
                       process. "
                       ,style = "font-family: 'calibri';font-size:14pt")   
                     )
                   )
           ),
           #references
           tabItem(tabName = "references",
                   h2("Thanks to these references"),
                   br(),
                   p("1 ",
                     a("Harry Potter Sentiment Analysis for Beginners"
                       ,href = "https://rstudio-pubs-static.s3.amazonaws.com/300624_8260952d1f0346969e65f41a97006bf5.html")
                     ,style = "font-family: 'calibri';font-size:14pt"),
                   p("2 ",
                     a("Understanding Sentiment Analysis: What It Is & Why It's Used"
                       ,href = "https://www.brandwatch.com/blog/understanding-sentiment-analysis/")
                     ,style = "font-family: 'calibri';font-size:14pt"),
                   p("3 ",
                     a("Tidy Sentiment Analysis in R"
                       ,href = "https://www.datacamp.com/community/tutorials/sentiment-analysis-R")
                     ,style = "font-family: 'calibri';font-size:14pt"),
                   p("4 ",
                     a("sentimentr"
                       ,href = "https://cran.r-project.org/web/packages/sentimentr/readme/README.html"
                       )
                     ,style = "font-family: 'calibri';font-size:14pt")
             
           )
        )
    )
  )
)
