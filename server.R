library(tidyverse)
library(shiny)
library(shinydashboard)
library(wordcloud2)
library(textstem)
library(harrypotter)
library(htmlwidgets)
library(tidytext)


nrc <- read.csv("nrc.csv")


philosophers_stone <- harrypotter::philosophers_stone
philosophers_stone <- data.frame(book = "philosophers_stone",
                                 chapter = seq_along(philosophers_stone),
                                 text = philosophers_stone)

chamber_of_secrets <- harrypotter::chamber_of_secrets
chamber_of_secrets <- data.frame(book = "chamber_of_secrets",
                                 chapter = seq_along(chamber_of_secrets),
                                 text = chamber_of_secrets)

prisoner_of_azkaban <- harrypotter::prisoner_of_azkaban
prisoner_of_azkaban <- data.frame(book = "prisoner_of_azkaban",
                                  chapter = seq_along(prisoner_of_azkaban),
                                  text = prisoner_of_azkaban)

goblet_of_fire <- harrypotter::goblet_of_fire
goblet_of_fire <- data.frame(book = "goblet_of_fire",
                             chapter = seq_along(goblet_of_fire),
                             text = goblet_of_fire)

order_of_the_phoenix <- harrypotter::order_of_the_phoenix
order_of_the_phoenix <- data.frame(book = "order_of_the_phoenix",
                                   chapter = seq_along(order_of_the_phoenix),
                                   text = order_of_the_phoenix)

half_blood_prince <- harrypotter::half_blood_prince
half_blood_prince <- data.frame(book = "half_blood_prince",
                                chapter = seq_along(half_blood_prince),
                                text = half_blood_prince)

deathly_hallows <- harrypotter::deathly_hallows
deathly_hallows <- data.frame(book = "deathly_hallows",
                              chapter = seq_along(deathly_hallows),
                              text = deathly_hallows)

Harry_Potter_TOTAL <- union_all(philosophers_stone,chamber_of_secrets) %>%
  union_all(prisoner_of_azkaban) %>%
  union_all(goblet_of_fire) %>%
  union_all(order_of_the_phoenix) %>%
  union_all(half_blood_prince) %>%
  union_all(deathly_hallows)






function(input,output,session) {
  
 wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                          fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                          minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                          rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                          widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
 {
   if ("table" %in% class(data)) {
     dataOut = data.frame(name = names(data), freq = as.vector(data))
   }
   else {
     data = as.data.frame(data)
     dataOut = data[, 1:2]
     names(dataOut) = c("name", "freq")
   }
   if (!is.null(figPath)) {
     if (!file.exists(figPath)) {
       stop("cannot find fig in the figPath")
     }
     spPath = strsplit(figPath, "\\.")[[1]]
     len = length(spPath)
     figClass = spPath[len]
     if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
       stop("file should be a jpeg, jpg, png, bmp or gif file!")
     }
     base64 = base64enc::base64encode(figPath)
     base64 = paste0("data:image/", figClass, ";base64,", 
                     base64)
   }
   else {
     base64 = NULL
   }
   weightFactor = size * 180/max(dataOut$freq)
   settings <- list(word = dataOut$name, freq = dataOut$freq, 
                    fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                    minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                    gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                    shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                    ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
   chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                     width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                             browser.padding = 0, browser.fill = TRUE))
   chart
 }
 
 

 
 
 data_title <- reactive({
   
   Harry_Potter_TOTAL %>%
     filter(book == input$book) %>% 
     unnest_tokens(output = word,
                   input = text,
                   token = "words") %>%
     mutate(word = lemmatize_strings(word)) %>%
     anti_join(tidytext::stop_words)
   
   
 }) 
 
 
 
 
 output$specific_title_wordcloud <- renderWordcloud2({
   
  df_wordcloud2 <- data_title() %>%
    count(word, sort = T) %>% 
    mutate(rank = row_number(desc(n))) %>% 
    filter(rank >= input$range[1] & rank <= input$range[2]) %>%
    select(word,n)
   
   
  wordcloud2a(data = df_wordcloud2,
              size = 1,
              gridSize = 2) 
 }) 
 
  
 output$plt_title <- renderPlot({
 
     data_title() %>% 
     count(word, sort = T) %>% 
     mutate(rank = row_number(desc(n))) %>% 
     filter(rank >= 1 & rank <= 20) %>%
     ggplot(aes(x = reorder(word,n), y = n)) +
     geom_bar(stat = "identity", fill = "darkred") +
     theme_minimal() +
     coord_flip() +
     geom_text(aes(label = n),hjust = 1.2, color = "white",size = 3.5,vjust = 0.5) +
     labs(x = "",y = "") 
   })
 
 
 
 output$sentiment_line <- renderPlot({
   
   data_title() %>% 
     count(chapter,word, sort = T) %>%
     bind_tf_idf(term = word, document = chapter, n = n) %>% 
     select(chapter,word, tf_idf) %>% 
     inner_join(nrc, by = c("word" = "word")) %>%
     filter(!word %in% c("harry","boy","stone","tree","moody","black")) %>%
     group_by(chapter,sentiment) %>%
     summarise(tf_idf = sum(tf_idf)) %>%
     ungroup() %>% 
     ggplot(aes(x = chapter, y = tf_idf, color = sentiment)) +
     geom_point() + geom_line() +
     scale_x_continuous(breaks = c(seq(1,40,1))) +
     facet_wrap(.~sentiment, scales = "fixed", ncol = 2) +
     labs(x = "Chapters",y = "") +
     theme_minimal() +
     theme(strip.background = element_rect(fill = "darkred"),
           strip.text = element_text(colour = "white", face = "bold"),
           legend.position = "none")
   
  
 })
 
 
 output$sentiment_top_words <- renderPlot({
   
   
   data_title() %>% 
     count(word, sort = T) %>%
     inner_join(nrc, by = c("word" = "word")) %>%
     filter(!word %in% c("harry","boy","stone","tree","moody","black")) %>%
     group_by(sentiment) %>%
     top_n(10,n) %>%
     ungroup() %>% 
     mutate(word = reorder(word,n)) %>%
     ggplot(aes(x = word, y = n, fill = sentiment)) +
     geom_col(show.legend = F) +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 90),
           strip.background = element_rect(fill = "darkred"),
           strip.text = element_text(color = "white", face = "bold")) +
     labs(x = "",y = "") +
     coord_flip() +
     facet_wrap(.~sentiment, scales = "free_y",ncol = 4) +
     geom_text(aes(label = n), size = 3,hjust = 1)
   
 })
   
 

 
 
}