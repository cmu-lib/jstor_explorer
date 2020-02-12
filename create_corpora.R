library(tidyverse)
library(DBI)
library(RSQLite)
library(storr)
library(glue)
library(tidytext)

db <- dbConnect(SQLite(), "data/corpus.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = ON")
st <- storr_dbi("storr_data", "storr_keys", db, binary = TRUE)

bigrams <- tbl(db, "bigrams")
unigrams <- tbl(db, "unigrams")
document_unigram <- tbl(db, "document_unigram")
document_bigram <- tbl(db, "document_bigram")
documents <- tbl(db, "documents")
document_metadata <- tbl(db, "document_metadata")
corpora <- tbl(db, "corpus")
corpus_document <- tbl(db, "corpus_document")
corpus_rdata <- tbl(db, "corpus_rdata")

#' @param special_bigrams These bigrams will be explicitly added to the "tokens" table
create_corpus <- function(db, st, including_all_unigrams = NULL, including_all_bigrams = "artificial intelligence", special_bigrams = c("artificial intelligence", "machine learning", "big data"), min_token_n = 20, corpus_label = "'Ethics' and 'Robots'") {
  if (corpora %>%
      filter(label == corpus_label) %>%
      collect() %>%
      nrow() != 0) {
    stop(glue("A corpus labelled \"{corpus_label}\" already exists."))
  }

  corpus_document_ids <- documents %>%
    select(document_id)

  if (!is.null(including_all_unigrams)) {
    inclusive_unigrams <- unigrams %>%
      filter(unigram %in% including_all_unigrams)

    inclusive_unigram_docs <- inclusive_unigrams %>%
      left_join(document_unigram, by = "unigram_id")

    corpus_document_ids <- corpus_document_ids %>%
      semi_join(inclusive_unigram_docs, by = "document_id")
  }

  if (!is.null(including_all_bigrams)) {
    inclusive_bigrams <- bigrams %>%
      filter(bigram %in% including_all_bigrams)

    inclusive_bigram_docs <- inclusive_bigrams %>%
      left_join(document_bigram, by = "bigram_id")

    corpus_document_ids <- corpus_document_ids %>%
      semi_join(inclusive_bigram_docs, by = "document_id")
  }

  message("Collecting document ids...")
  dbExecute(db, "DROP TABLE IF EXISTS temp_corpus_table")


  corpus_document_ids %>%
    compute("temp_corpus_table")

  temp_corpus_table <- tbl(db, "temp_corpus_table")

  inclusive_bigrams <- bigrams %>%
    filter(bigram %in% c(special_bigrams, including_all_bigrams))

  corpus_tokens <- dplyr::union(
    document_unigram %>%
      inner_join(unigrams, by = "unigram_id") %>% # join all unigrams
      filter(is_numeric == 0, nchar >= 3) %>%
      semi_join(temp_corpus_table, by = "document_id") %>%
      select(document_id, gram = unigram, n),
    document_bigram %>%
      inner_join(inclusive_bigrams, by = "bigram_id") %>% # join only included & special bigrams
      semi_join(temp_corpus_table, by = "document_id") %>%
      select(document_id, gram = bigram, n)
  )

  dbAppendTable(db, "corpus", tibble(label = corpus_label))
  new_corpus_id <- corpora %>%
    filter(label == corpus_label) %>%
    compute() %>%
    pull(corpus_id)

  dbExecute(db, glue("INSERT OR IGNORE INTO corpus_document SELECT {new_corpus_id} AS corpus_id, document_id FROM temp_corpus_table"))

  message("Collecting tokens...")
  collected_tokens <- collect(corpus_tokens) %>%
    group_by(gram) %>%
    # Only include tokens that appear over min_token_n times across the entire corpus
    # This has to happen after collecting because it involves filtering on a window function
    # which is not suported in sqlite
    filter(n_distinct(document_id) >= min_token_n) %>%
    ungroup()
  collected_token_key <- glue("collected-{new_corpus_id}-tokens")
  st$set(collected_token_key, collected_tokens)
  dbAppendTable(db, "corpus_rdata", tibble(corpus_id = new_corpus_id, object_type = "tidy-tokens", storr_key = collected_token_key))

  message("Storing DFM...")
  token_dfm <- cast_dfm(collected_tokens, document_id, gram, n)
  dfm_key <- glue("{new_corpus_id}-dfm")
  st$set(dfm_key, token_dfm)
  dbAppendTable(db, "corpus_rdata", tibble(corpus_id = new_corpus_id, object_type = "dfm", storr_key = dfm_key))

  return(new_corpus_id)
}

drop_corpora <- function() {
  dbExecute(db, "delete from corpus")
  dbExecute(db, "delete from storr_keys")
  dbExecute(db, "delete from storr_data")
}

drop_corpora()

create_corpus(db, st, including_all_unigrams = c("ethics"), including_all_bigrams = "artificial intelligence", corpus_label = "'Ethics' and 'Artificial Intelligence' all years")

create_corpus(db, st, including_all_unigrams = NULL, including_all_bigrams = "artificial intelligence", corpus_label = "'Artificial Intelligence' all years")

create_corpus(db, st, including_all_unigrams = "ethics", including_all_bigrams = "big data", corpus_label = "'Big Data' and 'Ethics' all years")

create_corpus(db, st, including_all_unigrams = NULL, including_all_bigrams = "big data", corpus_label = "'Big Data' all years")
