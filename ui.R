library(shinydashboard)

# Define UI for application that draws a histogram
dash_header <- dashboardHeader(
    title = "Exploravec(tor)"
)

corpus_tf_idf_tab <- tabItem(
    tabName = "corpus_tf_idf",
    h2("TF-IDF of the corpus documents"),
    box(
        selectizeInput("tfidf_stoplist", choices = NULL, selected = NULL, multiple = TRUE, label = "Eliminate tokens from TF-IDF"),
        p("Add tokens to exluce from the TF-IDF calculations.")
    ),
    box(dataTableOutput("document_metadata"),
        title = "Document TF-IDF",
        width = 12)
)

termsovertime_tab <- tabItem(
    tabName = "termsovertime",
    h2("Terms over time"),
    list(
    box(
        selectizeInput("wordchart_tokens", choices = NULL, selected = NULL, multiple = TRUE, label = "Pick tokens to plot over time"),
        p("Begin typing to generate token suggestions. Click on a token and press \"Delete\" to remove it from the list.")
    ),
    box(
        plotOutput("termsovertime_chart"),
        width = 12,
        height = 650
    ),
    box(
        dataTableOutput("termsovertime_metadata"),
        title = "Documents with these tokens",
        width = 12
    )
    )
)

corpus_selector <- selectInput("corpus_menu", choices = NULL, selected = 1, multiple = FALSE, label = "Corpus")

corpus_data <- div(
    p("Number of docs: ", textOutput("corpus_size", inline = TRUE))
)

dash_sidebar <- dashboardSidebar(
    corpus_selector,
    corpus_data,
    sidebarMenu(
        menuItem("Historical Change", tabName = "termsovertime", icon = icon("chart-line")),
        menuItem("TF-IDF", tabName = "corpus_tf_idf", icon = icon("sort-amount-down"))
    )
)

dash_body <- dashboardBody(
    tabItems(
        termsovertime_tab,
        corpus_tf_idf_tab
    )
)

dashboardPage(
    dash_header,
    dash_sidebar,
    dash_body,
    title = "AI and Ethics Corpus Explorer"
)
