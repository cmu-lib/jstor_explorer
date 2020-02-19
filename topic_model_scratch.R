library(quanteda)
library(topicmodels)
library(DBI)
library(RSQLite)
library(tidyverse)
library(storr)
library(drake)
library(future)

db <- dbConnect(SQLite(), "data/shiny.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = ON")
st <- storr_dbi("storr_data", "storr_keys", db, binary = TRUE)
#
# documents <- tbl(db, "documents")
# document_metadata <- tbl(db, "document_metadata")
# corpora <- tbl(db, "corpus")
# corpus_document <- tbl(db, "corpus_document")
# corpus_rdata <- tbl(db, "corpus_rdata")
#

jstor_lda <- function(x, k) {
  trimmed_x <- x %>%
    dfm_remove(stopwords('en')) %>%
    dfm_remove(stopwords('de')) %>%
    dfm_trim(min_termfreq = 0.3, termfreq_type = "quantile",
             max_docfreq = 0.1, docfreq_type = "prop")

  converted_x <- convert(trimmed_x, to = "topicmodels")
  LDA(converted_x, k = k)
}

dfm1 <- st$get("1-dfm")
dfm2 <- st$get("2-dfm")
dfm3 <- st$get("3-dfm")
dfm4 <- st$get("4-dfm")

lda_plan = drake_plan(
  analysis = target(
    jstor_lda(x = x_value, k = k_value),
    transform = cross(
      x_value = c(dfm1, dfm2, dfm3, dfm4),
      k_value = c(5, 10, 15, 20)
    )
  )
)

plan(multiprocess)

make(lda_plan, jobs = 4, parallelism = "future")

lda_names <- cached()

walk(lda_names, function(x) {
  st$set(x, get(x))
})
