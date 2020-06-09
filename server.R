library(shiny)
library(wordcloud)
library(devtools)
library(tidyverse)      
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(textdata)
library(knitr)
library(kableExtra)
library(sentimentr)
if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")
}
devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

##definite my own table style
my_kable_styling <- function(dat, caption) {
    kable(dat, "html", escape = FALSE, caption = caption) %>%
        kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                      full_width = FALSE, position = "left")
}

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

##Each book is an array in which each value in the array is a chapter 
series <- tibble()
for(i in seq_along(titles)) {
    ##divide the books into chapter
    clean <- tibble(chapter = seq_along(books[[i]]),
                    text = books[[i]]) %>%
             unnest_tokens(word, text) %>%
             ##Here we tokenize each chapter into words
             mutate(book = titles[i]) %>%
             select(book, everything())
    
    series <- rbind(series, clean)
}
# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$afinn <- renderPrint({
        get_sentiments("afinn")
    })
    
    output$bing <- renderPrint({
        get_sentiments("bing")
    })
    
    output$nrc <- renderPrint({
        get_sentiments("nrc")
    })
    
    output$lexicon_hp <- function(){
        new_sentiments <- bind_rows(get_sentiments("afinn") %>% #From the tidytext package
                                        mutate(sentiment = ifelse(value >= 0, "positive",
                                                                  ifelse(value < 0,
                                                                         "negative", "neutral"))) %>%
                                        mutate(lexicon = "AFINN")%>%
                                        select(word, sentiment, lexicon),
                                    get_sentiments("bing")%>%
                                        mutate(lexicon = "Bing"),
                                    get_sentiments("nrc")%>%
                                        mutate(lexicon = "NRC")
        )%>%
            group_by(lexicon) %>%
            mutate(words_in_lexicon = n_distinct(word)) %>%
            ungroup()
        
        series %>%
            mutate(words_in_novels = n_distinct(word)) %>%
            inner_join(new_sentiments) %>%
            group_by(lexicon, words_in_novels, words_in_lexicon) %>%
            summarise(lex_match_words = n_distinct(word)) %>%
            ungroup() %>%
            mutate(total_match_words = sum(lex_match_words), #Not used but good to have
                   match_ratio = lex_match_words / words_in_novels) %>%
            select(lexicon, lex_match_words,  words_in_novels, match_ratio)%>%
            my_kable_styling(caption = "HP Word Found In Lexicons")
    }
    
    output$lexicon_diff <- renderPlot({
        afinn <- series %>%
            group_by(book) %>% 
            mutate(word_count = 1:n(),
                   index = word_count %/% 500 + 1) %>% 
            inner_join(get_sentiments("afinn")) %>%
            group_by(book, index) %>%
            summarise(sentiment = sum(value)) %>%
            mutate(method = "AFINN")
        
        bing_and_nrc <- bind_rows(series %>%
                                      group_by(book) %>% 
                                      mutate(word_count = 1:n(),
                                             index = word_count %/% 500 + 1) %>% 
                                      inner_join(get_sentiments("bing")) %>%
                                      mutate(method = "Bing"),
                                  series %>%
                                      group_by(book) %>% 
                                      mutate(word_count = 1:n(),
                                             index = word_count %/% 500 + 1) %>%
                                      inner_join(get_sentiments("nrc") %>%
                                                     filter(sentiment %in% c("positive", "negative"))) %>%
                                      mutate(method = "NRC")) %>%
            count(book, method, index = index , sentiment) %>%
            ungroup() %>%
            spread(sentiment, n, fill = 0) %>%
            mutate(sentiment = positive - negative) %>%
            select(book, index, method, sentiment)
        
        #now have an estimate of the net sentiment (positive - negative) in each chunk of the novel text for each sentiment lexicon
        bind_rows(afinn, 
                  bing_and_nrc) %>%
            ungroup() %>%
            mutate(book = factor(book, levels = titles)) %>%
            ggplot(aes(index, sentiment, fill = method)) +
            geom_bar(alpha = 1, stat = "identity", show.legend = FALSE) +
            facet_grid(book ~ method) +
            theme(strip.text.x = element_text(size = 14))
    })
    
    output$word_count <- renderPlot({
        bing_word_counts <- series[series$book==input$book_title,] %>%
            {if(input$stop_words) anti_join(., stop_words) else .}%>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            ungroup()
        bing_word_counts %>%
            group_by(sentiment) %>%
            top_n(10) %>%
            ggplot(aes(reorder(word, n), n, fill = sentiment)) +
            geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment", x = NULL) +
            coord_flip()
    })
    
    output$word_cloud <- renderPlot({
        series[series$book==input$book_title,] %>%
            {if(input$stop_words) anti_join(., stop_words) else .}%>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 0) %>%
            comparison.cloud(colors = c( "#00BFC4","#F8766D"),
                             max.words = 100,title.size = 1.5)
    })
    
    output$plot_development <- renderPlot({
        book_index = match(input$book_title,titles)
        book_sentences <- tibble(chapter = 1:length(books[[book_index]]),
                               text = books[[book_index]]) %>% 
            unnest_tokens(sentence, text, token = "sentences")
        
        book_sent <- book_sentences %>%
            group_by(chapter) %>%
            mutate(sentence_num = 1:n(),
                   index = round(sentence_num / n(), 2)) %>%
            unnest_tokens(word, sentence) %>%
            inner_join(get_sentiments("afinn")) %>%
            group_by(chapter, index) %>%
            summarise(sentiment = sum(value, na.rm = TRUE)) %>%
            arrange(desc(sentiment))
        
        ggplot(book_sent, aes(index, factor(chapter, levels = sort(unique(chapter), decreasing = TRUE)), fill = sentiment)) +
            geom_tile(color = "white") +
            scale_fill_gradient2() +
            scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
            scale_y_discrete(expand = c(0, 0)) +
            labs(x = "Chapter Progression", y = "Chapter") +
            ggtitle(paste("Sentiment of", input$book_title),
                    subtitle = "Summary of the net sentiment score as you progress through each chapter") +
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = "top")
    })
    
    tb <- reactive({tibble(chapter = seq_along(books[[match(input$book_sentence,titles)]]),
                text = books[[match(input$book_sentence,titles)]])
        })
    output$chapter_sentence <- renderPlot({
             tb() %>%
                sentimentr::get_sentences(text) %$%
            sentimentr::sentiment_by(text,list(chapter))%>%
                plot(ordered=FALSE)+ ggtitle("Sentiment distribution of chapters")
    })
    
    output$plot_sentence <- renderPlot({
        tb() %>%
            sentimentr::get_sentences(text) %$%
            sentimentr::sentiment_by(text,list(chapter))%>%
            sentimentr::uncombine()%>%
            plot()+ ggtitle("Sentiment development along with plot")
    })
       
    
    #output$hl_sentence <- renderPrint({
    #    sentence_class <- with(
    #                        series_select(),
    #                        sentiment_by(series_select()$text, series_select()$chapter)
    #    )
    #    sentence_class %>% highlight()
    #   
    #})


})
