library(shiny)
shinyUI(navbarPage("Book Recommender System",
   tabPanel("Search Engine",
            h3("We can help you find a book"),
            h4("This search engine to find other books that others enjoyed!"),
            fluidRow("1. Search for a book title you've read."),
            fluidRow("2. Press [Submit] to get some recommended books!"),
            br(),
            textInput('sentenceInputVar', 'Search: (Example: "Da Vinci", "The Notebook")'),
            actionButton('submitButton', 'Submit'),
            br(),
            br(),
            br(),
            textOutput("oRecommendationsFor"),
            textOutput("oUserInput"),
            br(),
            tableOutput("oResults"),
            textOutput("oSimExplanation")
   )
        
    )
)