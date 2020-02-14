library(shiny)
library(tidytext)
library(glue)
library(lubridate)
library(topicmodels)

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
    withProgress({
      req(input$corpus_menu)
      corpus_key <- corpus_rdata %>%
        filter(corpus_id == local(input$corpus_menu), object_type == "tidy-tokens") %>%
        collect() %>%
        pull(storr_key)
      st$get(corpus_key)
    }, message = "Loading corpus from disk")
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
    withProgress({
      uids <- dplyr::setdiff(inclusive_filtered_corpus(), exclusive_filtered_corpus())
      corpus_tokens() %>%
        filter(document_id %in% uids)
    }, message = "Filtering corpus")
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
    withProgress({
      filtered_corpus() %>%
        bind_tf_idf(gram, document_id, n) %>%
        group_by(document_id) %>%
        filter(row_number(desc(tf_idf)) <= 10) %>%
        arrange(desc(tf_idf)) %>%
        summarize(top_terms = str_c(gram, collapse = ", "))
    }, message = "Calculating TF-IDF")
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
    withProgress({
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
    }, message = "Generating terms over time")
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
    withProgress({
      # yearly_tf_idf()
      yearly_tf_idf() %>%
        group_by(year) %>%
        arrange(desc(tf_idf)) %>%
        filter(row_number(desc(tf_idf)) <= 40) %>%
        summarize(top_terms = str_c(gram, collapse = ", "))
    }, message = "Calculating annual TF-IDF")
  }, escape = FALSE)

  # Topic models ----

  corpus_tm <- reactive({
    # withProgress({
    #   req(input$corpus_menu)
    #   tm_key <- corpus_rdata %>%
    #     filter(corpus_id == local(input$corpus_menu),  object_type == "topic-model") %>%
    #     collect() %>%
    #     pull(storr_key)
    #   st$get(corpus_key)
    # }, message = "Loading topic model from disk")
    n_topics <- input$n_topics
    st$get(glue("1-lda-{n_topics}"))
  })

  tm_terms <- reactive({
    as.list((as.data.frame(terms(corpus_tm(), 20)))) %>% unname()
  })

  tm_docs <- reactive({
    posterior(corpus_tm())[["topics"]]
  })

  output$tm_html <- renderUI({
    token_summaries <- shinydashboard::box(
      title = "Topic terms",
      purrr::imap(tm_terms(), function(x, i) {
        p(glue("Topic {i}: "), str_c(x, collapse = ", "))
      })
    )

    full_tables <- doc_tables <- purrr::imap(tm_terms(), function(x, i) {
      docs_ranking <- sort(min_rank(desc(tm_docs()[,i])))

      top_docs <- tibble(document_id = as.integer(names(docs_ranking))) %>%
        inner_join(corpus_metadata(), by = "document_id") %>%
        select(item_title, doc_type, authors, parent_title, date, url) %>%
        slice(1:20) %>%
        mutate(
          date = ymd(date),
          url = glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{url}</a>")
        )

      docstring <- str_c(x, collapse = ", ")

      shinydashboard::box(
        title = glue("Topic {i}: {docstring}"),
        DT::datatable(top_docs, escape = FALSE),
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE
      )
    })

    list(
      token_summaries,
      full_tables
    )
  })


}
