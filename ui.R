library(shiny)
library(shinydashboard)
library(shinycssloaders)

dashboardPage(skin = "red",
  
  dashboardHeader(title = h4(HTML("Harry Potter Text Mining Analysis"))),
  
  dashboardSidebar(
    br(),

    radioButtons(inputId = "book", label = "Book",
                 choiceValues = c("philosophers_stone","chamber_of_secrets",
                             "prisoner_of_azkaban","goblet_of_fire",
                             "order_of_the_phoenix","half_blood_prince", "deathly_hallows"),
                 choiceNames  = c("philosophers stone","chamber of secrets",
                                 "prisoner of azkaban","goblet of fire",
                                 "order of the phoenix","half blood prince", "deathly hallows"),
                 selected = "philosophers_stone", width = "400px"),
    br(),
    sliderInput(inputId = "range",
                label = "Range of the most frequent words for Word Cloud",
                min = 1, max = 500, step = 1, value = c(0,100), sep = "", round = TRUE, ticks = FALSE),
    
    
    br(),
    br(),
    div(style="display:inline-block;width:100%;text-align: center;",
        submitButton(text = "Apply", width = "100px",icon("paper-plane"))),
    br(),
    br()
  ),
  
  dashboardBody(

    
               br(),
               fluidRow(column(width = 8,
               box(withSpinner(wordcloud2::wordcloud2Output("specific_title_wordcloud")),
                   width = 12, title = "Word Cloud - the most frequent words",
                   status = "primary",height = "460px")),
                      column(width = 4,
                             box(withSpinner(plotOutput("plt_title")),
                                 width = 16,
                                 title = "20 the most frequent words",
                                 status = "primary"))),
               fluidRow(column(width = 6,
                               box(withSpinner(plotOutput("sentiment_line")),
                                   width = 12,
                                   title = "Sentiment Analysis (NRC) - (TF-IDF)",
                                   status = "primary")),
                        column(width = 6,
                               box(withSpinner(plotOutput("sentiment_top_words")),
                                   width = 14,
                                   title = "NRC sentiment divided into top words",
                                   status = "primary"))
    ))
  
  

  
  
)


