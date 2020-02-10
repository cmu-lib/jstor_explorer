library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("Corpus TF-IDF"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("document_metadata")
        )
    )
)

