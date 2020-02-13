library(shiny)
library(tidytext)
library(glue)
library(lubridate)

function(input, output, session) {

  # corpora ----

  available_corpora <- reactive({
    collect(corpora)
  })

  observe({
    x <- set_names(available_corpora()$corpus_id, nm = available_corpora()$label)
    updateSelectInput(session, "corpus_menu",
                      choices = x,
                      selected = 1)
  })

  corpus_tokens <- reactive({
    req(input$corpus_menu)
    corpus_key <- corpus_rdata %>%
      filter(corpus_id == local(input$corpus_menu), object_type == "tidy-tokens") %>%
      collect() %>%
      pull(storr_key)
    st$get(corpus_key)
  })

  # Document IDs that must be kept in
  inclusive_filtered_corpus <- reactive({
    if (is.null(input$corpus_include)) {
      corpus_tokens() %>%
        distinct(document_id) %>%
        pull(document_id)
    } else {
      corpus_tokens() %>%
        group_by(document_id) %>%
        filter(all(input$corpus_include %in% gram)) %>%
        ungroup() %>%
        distinct(document_id) %>%
        pull(document_id)
    }
  })

  # Document IDs that must be excluded
  exclusive_filtered_corpus <- reactive({
    if (is.null(input$corpus_exclude)) {
      integer(0)
    } else {
      corpus_tokens() %>%
        filter(gram %in% input$corpus_exclude) %>%
        distinct(document_id) %>%
        pull(document_id)
    }
  })

  filtered_corpus <- reactive({
    uids <- dplyr::setdiff(inclusive_filtered_corpus(), exclusive_filtered_corpus())
    print(uids)
    corpus_tokens() %>%
      filter(document_id %in% uids)
  })

  base_corpus_metadata <- reactive({
    req(input$corpus_menu)
    corpus_document %>%
      filter(corpus_id == local(input$corpus_menu)) %>%
      inner_join(document_metadata, by = "document_id") %>%
      arrange(date, parent_title, item_title) %>%
      collect()
  })

  corpus_metadata <- reactive({
    base_corpus_metadata() %>%
      semi_join(filtered_corpus(), by = "document_id")
  })

  output$corpus_size <- renderText({
    format(nrow(corpus_metadata()), big.mark = ",")
  })

  token_choices <- reactive({
    toks <- corpus_tokens() %>%
      distinct(gram) %>%
      pull(gram)
  })

  # Update token selectize menus based on selected corpora tokens
  observe({
    x <- token_choices()
    updateSelectizeInput(session, "corpus_include",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "corpus_exclude",
                         choices = x,
                         selected = NULL,
                         server = TRUE)
    updateSelectizeInput(session, "wordchart_tokens",
                         choices = x,
                         selected = c("regulation", "big data"),
                         server = TRUE)
  })

  # TF-IDF ----

  corpus_tfidf <- reactive({
    filtered_corpus() %>%
      bind_tf_idf(gram, document_id, n) %>%
      group_by(document_id) %>%
      filter(row_number(desc(tf_idf)) <= 10) %>%
      arrange(desc(tf_idf)) %>%
      summarize(top_terms = str_c(gram, collapse = ", "))
  })

  output$document_metadata <- renderDataTable({
    corpus_tfidf() %>%
      inner_join(corpus_metadata(), by = "document_id") %>%
      select(doc_type, item_title, parent_title, date, url, top_terms) %>%
      mutate(
        date = ymd(date),
        url = glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{url}</a>")
      )
  }, escape = FALSE)

  # termsovertime ----

  termsovertime_tokens <- reactive({
    corpus_document %>%
      filter(corpus_id == 1) %>%
      left_join(document_metadata, by = "document_id") %>%
      select(document_id, date) %>%
      collect()
  })

  termsovertime_data <- reactive({
    filtered_corpus() %>%
      left_join(termsovertime_tokens(), by = "document_id") %>%
      mutate(approx_date = round_date(ymd(date), "year")) %>%
      group_by(approx_date) %>%
      mutate(total_docs = n_distinct(document_id)) %>%
      ungroup() %>%
      filter(gram %in% input$wordchart_tokens) %>%
      group_by(approx_date, gram) %>%
      summarize(
        percent_total = n() / first(total_docs)
      )
  })

  output$termsovertime_chart <- renderPlot({
    req(input$wordchart_tokens)
    termsovertime_data() %>%
      ggplot(aes(x = approx_date, y = percent_total, color = gram)) +
      geom_line() +
      theme_minimal()
  }, height = 600)

  termsovertime_metadata <- reactive({
    filtered_corpus() %>%
      filter(gram %in% input$wordchart_tokens) %>%
      distinct(document_id) %>%
      inner_join(corpus_metadata(), by = "document_id") %>%
      mutate(
        date = ymd(date),
        url = glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{url}</a>")
      )
  })

  output$termsovertime_metadata <- renderDataTable({
    termsovertime_metadata()
  }, escape = FALSE)

  # Annual TF-IDF ----

  yearly_tokens <- reactive({
    filtered_corpus() %>%
      left_join(corpus_metadata(), by = "document_id") %>%
      filter(year > 2013) %>%
      group_by(year, gram) %>%
      summarize(nn = sum(n))
  })

  yearly_tf_idf <- reactive({
    yearly_tokens() %>%
      bind_tf_idf(gram, year, nn)
  })

  output$yearly_tf_idf_table <- renderDataTable({
    # yearly_tf_idf()
    yearly_tf_idf() %>%
      group_by(year) %>%
      arrange(desc(tf_idf)) %>%
      filter(row_number(desc(tf_idf)) <= 40) %>%
      summarize(top_terms = str_c(gram, collapse = ", "))
  }, escape = FALSE)
}
