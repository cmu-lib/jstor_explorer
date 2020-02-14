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


lda_plan = drake_plan(
  jstor_ai = st$get("1-dfm"),
  jstor_ai_5_model = jstor_lda(jstor_ai, 5),
  jstor_ai_10_model = jstor_lda(jstor_ai, 10),
  jstor_ai_15_model = jstor_lda(jstor_ai, 15),
  jstor_ai_20_model = jstor_lda(jstor_ai, 20),
  set_1_lda_5 = st$set("1-lda-5", jstor_ai_5_model),
  set_1_lda_10 = st$set("1-lda-10", jstor_ai_10_model),
  set_1_lda_15 = st$set("1-lda-15", jstor_ai_15_model),
  set_1_lda_20 = st$set("1-lda-20", jstor_ai_20_model),
)

make(lda_plan)
