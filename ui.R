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

bookworm_tab <- tabItem(
    tabName = "bookworm",
    h2("Terms over time"),
    list(
    box(
        selectizeInput("wordchart_tokens", choices = NULL, selected = NULL, multiple = TRUE, label = "Pick tokens to plot over time")
    ),
    box(
        plotOutput("bookworm_chart"),
        width = 12
    )
    )
)

corpus_selector <- selectInput("corpus_menu", choices = NULL, selected = 1, multiple = FALSE, label = "Corpus")

dash_sidebar <- dashboardSidebar(
    corpus_selector,
    sidebarMenu(
        menuItem("Bookworm", tabName = "bookworm", icon = icon("chart-line")),
        menuItem("TF-IDF", tabName = "corpus_tf_idf", icon = icon("sort-amount-down"))
    )
)

dash_body <- dashboardBody(
    tabItems(
        bookworm_tab,
        corpus_tf_idf_tab
    )
)

dashboardPage(
    dash_header,
    dash_sidebar,
    dash_body,
    title = "AI and Ethics Corpus Explorer"
)
