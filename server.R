library(shiny)
library(tidytext)
library(glue)
library(lubridate)

function(input, output, session) {

  available_corpora <- reactive({
    collect(corpora)
  })

  corpus_tokens <- reactive({
    return(st$get("corpus-1-tidy"))
  })

  corpus_metadata <- reactive({
    corpus_document %>%
      filter(corpus_id == 1) %>%
      inner_join(document_metadata, by = "document_id") %>%
      arrange(date, parent_title, item_title) %>%
      collect()
  })

  token_choices <- reactive({
    toks <- corpus_tokens() %>%
      distinct(gram) %>%
      pull(gram)
  })

  # Update token selectize menus based on selected corpora tokens
  observe({
    x <- token_choices()
    updateSelectizeInput(session, "tfidf_stoplist",
                         choices = x,
                         server = TRUE)
    updateSelectizeInput(session, "wordchart_tokens",
                         choices = x,
                         selected = c("regulation", "big data"),
                         server = TRUE)
  })

  # TF-IDF ----

  refined_tf_idf_tokens <- reactive({
    corpus_tokens() %>%
      filter(!(gram %in% input$tfidf_stoplist))
  })

  corpus_tfidf <- reactive({
    refined_tf_idf_tokens() %>%
      bind_tf_idf(gram, document_id, n) %>%
      group_by(document_id) %>%
      filter(row_number(desc(tf_idf)) <= 10) %>%
      arrange(desc(tf_idf)) %>%
      summarize(top_terms = str_c(gram, collapse = ", "))
  })

  # corpus_decade_tfidf <- reactive({
  #   ethics_and_ai_tokens %>%
  #     left_join(document_metadata %>% filter(language == "eng") %>% select(document_id, year), by = "document_id", copy = TRUE) %>%
  #     group_by(gram) %>%
  #     filter(sum(n) > 10) %>%
  #     group_by(year, gram) %>%
  #     summarize(nn = sum(n)) %>%
  #     bind_tf_idf(gram, year, nn) %>%
  #     group_by(year) %>%
  #     filter(row_number(desc(tf_idf)) <= 10) %>%
  #     arrange(desc(tf_idf)) %>%
  #     summarize(top_terms = str_c(gram, collapse = ", "))
  # })

  output$document_metadata <- renderDataTable({
    corpus_tfidf() %>%
      inner_join(corpus_metadata(), by = "document_id") %>%
      select(doc_type, item_title, parent_title, date, url, top_terms) %>%
      mutate(
        date = ymd(date),
        url = glue("<a href={url}>{url}</a>")
        )
  }, escape = FALSE)

  # Bookworm ----

  bookworm_tokens <- reactive({
    corpus_document %>%
      filter(corpus_id == 1) %>%
      left_join(document_metadata, by = "document_id") %>%
      select(document_id, year) %>%
      collect()
    })

  bookworm_data <- reactive({
    corpus_tokens() %>%
      left_join(bookworm_tokens(), by = "document_id") %>%
      group_by(year) %>%
      mutate(total_docs = n_distinct(document_id)) %>%
      ungroup() %>%
      filter(gram %in% input$wordchart_tokens) %>%
      group_by(year, gram) %>%
      summarize(
        percent_total = n() / first(total_docs)
      )
  })

  output$bookworm_chart <- renderPlot({
    bookworm_data() %>%
      ggplot(aes(x = year, y = percent_total)) +
      geom_line(aes(color = gram)) +
      xlim(2014, 2020) +
      theme_minimal()
  })
}
