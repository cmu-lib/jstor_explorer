library(xml2)
library(lubridate)

parse_jstor_metadata <- function(doc_xml, docstring, document_id) {
  doc_type <- str_extract(docstring, "[a-z]+-[a-z]+")
  jstor_processor <- switch(doc_type,
                            "research-report" = research_report_processor,
                            "journal-article"= journal_article_processor,
                            "book-chapter" = book_chapter_processor)
  meta_table <- jstor_processor(doc_xml, document_id, docstring)
  if (nrow(meta_table) != 1) {
    stop(glue("{docstring} parsing returned 0-length table"))
  }
  return(meta_table)
}

journal_article_processor <- function(doc_xml, document_id, ...) {

  item_title <- doc_xml %>%
    xml_find_first("//title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  if (length(item_title) != 1) {
    item_title <- doc_xml %>%
      xml_find_first("//product") %>%
      xml_children() %>%
      xml_text(trim = TRUE) %>%
      str_c("Review", ., sep = ": ", collapse = ": ")
  }

  item_authors_nodes <- doc_xml %>%
    xml_find_all("//contrib")

  item_authors <- str_c(
    item_authors_nodes %>%
      xml_find_all("//surname") %>%
      xml_text(trim = TRUE),
    item_authors_nodes %>%
      xml_find_all("//given-names") %>%
      xml_text(trim = TRUE),
    sep = ", ",
    collapse = "; "
  )

  if (length(item_authors) != 1) {
    item_authors <- str_c(
      item_authors_nodes %>%
        xml_text(trim = TRUE),
      collapse = "; "
    )
  }

  if (length(item_authors) != 1) {
    item_authors <- NA_character_
  }

  journal_title <- doc_xml %>%
    xml_find_first("//journal-title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  article_day <- doc_xml %>%
    xml_find_first("//pub-date/day") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0") %>%
    if_else(is.na(.), "01", .)

  article_month <- doc_xml %>%
    xml_find_first("//pub-date/month") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0") %>%
    if_else(is.na(.), "01", .)

  article_year <- doc_xml %>%
    xml_find_first("//pub-date/year") %>%
    xml_text(trim = TRUE)

  article_date <- str_c(article_year, article_month, article_day, sep = "-")

  article_url <- doc_xml %>%
    xml_find_first("//self-uri") %>%
    xml_attr("href")

  article_abstract <- doc_xml %>%
    xml_find_first("//abstract") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = " ")

  if (length(article_abstract) != 1) {
    article_abstract <- NA_character_
  }

  language <- doc_xml %>%
    xml_find_first("//article") %>%
    xml_attr("lang")

  tibble(
    document_id = document_id,
    doc_type = "journal-article",
    item_title = item_title,
    authors = item_authors,
    parent_title = journal_title,
    date = article_date,
    year = as.integer(article_year),
    abstract = article_abstract,
    language = language,
    url = article_url
  )
}

book_chapter_processor <- function(doc_xml, document_id, docstring) {

  chapter_number <- docstring %>%
    str_extract("\\d+$") %>%
    as.integer()

  item_meta <- doc_xml %>%
    xml_find_first(glue("//book-part[{chapter_number}]"))

  item_title <- item_meta %>%
    xml_find_first(".//title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  item_authors_nodes <- item_meta %>%
    xml_find_all(".//contrib")

  if (length(item_authors_nodes) < 1) {
    item_authors_nodes <- doc_xml %>%
      xml_find_all(".//contrib")
  }

  item_authors <- str_c(
    item_authors_nodes %>%
      xml_find_all(".//surname") %>%
      xml_text(trim = TRUE),
    item_authors_nodes %>%
      xml_find_all(".//given-names") %>%
      xml_text(trim = TRUE),
    sep = ", ",
    collapse = "; "
  )

  if (length(item_authors) != 1) {
    item_authors <- str_c(
      item_authors_nodes %>%
        xml_text(trim = TRUE),
      collapse = "; "
    )
  }

  if (length(item_authors) != 1) {
    item_authors <- NA_character_
  }

  book_title <- doc_xml %>%
    xml_find_first("//book-title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  article_day <- doc_xml %>%
    xml_find_first("//pub-date/day") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0")

  article_month <- doc_xml %>%
    xml_find_first("//pub-date/month") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0")

  article_year <- doc_xml %>%
    xml_find_first("//pub-date/year") %>%
    xml_text(trim = TRUE)

  article_date <- str_c(article_year, article_month, article_day, sep = "-")

  article_url <- doc_xml %>%
    xml_find_first("//self-uri") %>%
    xml_attr("href") %>%
    str_c(chapter_number, sep = ".")

  article_abstract <- item_meta %>%
    xml_find_first(".//abstract") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = " ")

  if (length(article_abstract) != 1) {
    article_abstract <- NA_character_
  }

  language <- doc_xml %>%
    xml_find_first("//book") %>%
    xml_attr("lang")

  tibble(
    document_id = document_id,
    doc_type = "book-chapter",
    item_title = item_title,
    authors = item_authors,
    parent_title = book_title,
    date = article_date,
    year = as.integer(article_year),
    abstract = article_abstract,
    language = language,
    url = article_url
  )
}

research_report_processor <- function(doc_xml, document_id, ...) {
  item_title <- doc_xml %>%
    xml_find_first(".//book-title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  item_authors_nodes <- doc_xml %>%
    xml_find_all(".//contrib")

  item_authors <- str_c(
    item_authors_nodes %>%
      xml_find_all("//surname") %>%
      xml_text(trim = TRUE),
    item_authors_nodes %>%
      xml_find_all("//given-names") %>%
      xml_text(trim = TRUE),
    sep = ", ",
    collapse = "; "
  )

  if (length(item_authors) != 1) {
    item_authors <- str_c(
      item_authors_nodes %>%
        xml_text(trim = TRUE),
      collapse = "; "
    )
  }

  if (length(item_authors) != 1) {
    item_authors <- NA_character_
  }

  book_title <- doc_xml %>%
    xml_find_first("//book-title-group") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = ": ")

  publisher <- doc_xml %>%
    xml_find_first("//publisher") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = "; ")

  article_day <- doc_xml %>%
    xml_find_first("//pub-date/day") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0")

  article_month <- doc_xml %>%
    xml_find_first("//pub-date/month") %>%
    xml_text(trim = TRUE) %>%
    str_pad(2, pad = "0")

  article_year <- doc_xml %>%
    xml_find_first("//pub-date/year") %>%
    xml_text(trim = TRUE)

  article_date <- str_c(article_year, article_month, article_day, sep = "-")

  article_url <- doc_xml %>%
    xml_find_first("//self-uri") %>%
    xml_attr("href")

  article_abstract <- doc_xml %>%
    xml_find_first(".//abstract") %>%
    xml_children() %>%
    xml_text(trim = TRUE) %>%
    str_c(collapse = " ")

  if (length(article_abstract) != 1) {
    article_abstract <- NA_character_
  }

  language <- doc_xml %>%
    xml_find_first("//book") %>%
    xml_attr("lang")

  tibble(
    document_id = document_id,
    doc_type = "research-report",
    item_title = if_else(length(book_title) == 1, book_title, NA_character_),
    authors = item_authors,
    parent_title = if_else(length(publisher) == 1, publisher, NA_character_),
    date = article_date,
    year = as.integer(article_year),
    abstract = article_abstract,
    language = language,
    url = article_url
  )
}
