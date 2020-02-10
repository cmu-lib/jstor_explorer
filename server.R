library(shiny)
library(tidytext)
library(glue)

function(input, output) {

  available_corpora <- reactive({
    collect(corpora)
  })

  corpus_tokens <- reactive({
    return(st$get("corpus-1-tidy"))
  })

  corpus_tfidf <- reactive({
    corpus_tokens() %>%
      bind_tf_idf(gram, document_id, n) %>%
      group_by(document_id) %>%
      filter(row_number(desc(tf_idf)) <= 10) %>%
      arrange(desc(tf_idf)) %>%
      summarize(top_terms = str_c(gram, collapse = ", "))
  })

  corpus_decade_tfidf <- reactive({
    ethics_and_ai_tokens %>%
      left_join(document_metadata %>% filter(language == "eng") %>% select(document_id, year), by = "document_id", copy = TRUE) %>%
      group_by(gram) %>%
      filter(sum(n) > 10) %>%
      group_by(year, gram) %>%
      summarize(nn = sum(n)) %>%
      bind_tf_idf(gram, year, nn) %>%
      group_by(year) %>%
      filter(row_number(desc(tf_idf)) <= 10) %>%
      arrange(desc(tf_idf)) %>%
      summarize(top_terms = str_c(gram, collapse = ", "))
  })

  output$document_metadata <- renderDataTable({
    corpus_tfidf() %>%
      inner_join(document_metadata, by = "document_id", copy = TRUE) %>%
      select(item_title, parent_title, date, url, top_terms) %>%
      collect() %>%
      mutate(url = glue("<a href={url}>{url}</a>"))
  }, escape = FALSE)
}
