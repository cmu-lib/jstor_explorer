library(DBI)
library(RSQLite)
library(tidyverse)
library(storr)

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

ai_docs <- bigrams %>%
  filter(bigram == "artificial intelligence") %>%
  left_join(document_bigram, by = "bigram_id")

ethics_docs <- unigrams %>%
  filter(unigram == "ethics") %>%
  left_join(document_unigram, by = "unigram_id")

ethics_and_ai_docs <- intersect(ai_docs %>% select(document_id), ethics_docs %>% select(document_id))

# dbAppendTable(db, "corpus", tibble(label = "'Ethics' and 'Artificial Intelligence' all years"))
# dbAppendTable(db, "corpus_document", collect(ethics_and_ai_docs) %>% mutate(corpus_id = 1))
#
# dbAppendTable(db, "corpus", tibble(label = "'Artificial Intelligence' all years"))
# dbAppendTable(db, "corpus_document", collect(ai_docs %>% distinct(document_id) %>% mutate(corpus_id = 2)))
#
# #
# ethics_and_ai_meta <- ethics_and_ai_docs %>%
#   inner_join(document_metadata, by = "document_id")
#
key_bigrams <- bigrams %>%
  filter(bigram %in% c("artificial intelligence", "machine learning", "big data"))

# ethics_and_ai_tokens <- union(
#   document_unigram %>% inner_join(unigrams, by = "unigram_id") %>% filter(is_numeric == 0) %>% semi_join(ethics_and_ai_docs, by = "document_id") %>%  select(document_id, gram = unigram, n),
#   document_bigram %>% inner_join(key_bigrams, by = "bigram_id") %>% semi_join(ethics_and_ai_docs, by = "document_id") %>% select(document_id, gram = bigram, n)
# ) %>% collect()

ai_tokens <- union(
  document_unigram %>% inner_join(unigrams, by = "unigram_id") %>% filter(is_numeric == 0) %>% semi_join(ai_docs, by = "document_id") %>%  select(document_id, gram = unigram, n),
  document_bigram %>% inner_join(key_bigrams, by = "bigram_id") %>% semi_join(ai_docs, by = "document_id") %>% select(document_id, gram = bigram, n)
) %>% collect()

st$set("corpus-2-tidy", ai_tokens)
