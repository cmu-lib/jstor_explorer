# Set up the sqlite db that actually needs to go to the shiny server (not including full unigram lists, etc.)

library(tidyverse)
library(DBI)
library(RSQLite)
library(storr)
library(glue)
library(tidytext)

db <- dbConnect(SQLite(), "data/corpus.sqlite3")
shn <- dbConnect(SQLite(), "data/shiny.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = ON")
dbExecute(shn, "PRAGMA foreign_keys = ON")
st <- storr_dbi("storr_data", "storr_keys", db, binary = TRUE)

dbExecute(shn, "CREATE TABLE documents(document_id INTEGER PRIMARY KEY NOT NULL, docstring TEXT UNIQUE NOT NULL)")
dbExecute(shn, "CREATE TABLE document_metadata(document_id INTEGER PRIMARY KEY NOT NULL, doc_type TEXT NOT NULL, item_title TEXT, authors TEXT, parent_title TEXT, date TEXT, year INTEGER, abstract TEXT, language TEXT, url TEXT, FOREIGN KEY(document_id) REFERENCES documents(document_id))")
dbExecute(shn, "CREATE INDEX document_date ON document_metadata(date DESC)")
dbExecute(shn, "CREATE INDEX document_year ON document_metadata(year DESC)")
dbExecute(shn, "CREATE INDEX document_type ON document_metadata(doc_type)")
dbExecute(shn, "CREATE TABLE corpus(corpus_id INTEGER PRIMARY KEY NOT NULL, label TEXT UNIQUE NOT NULL)")

dbExecute(shn, "CREATE TABLE IF NOT EXISTS corpus_document(corpus_id INTEGER NOT NULL, document_id INTEGER NOT NULL, FOREIGN KEY (corpus_id) REFERENCES corpus(corpus_id) ON DELETE CASCADE, FOREIGN KEY (document_id) REFERENCES documents(document_id), UNIQUE(corpus_id, document_id))")
dbExecute(shn, "CREATE INDEX corpus_document_document ON corpus_document(document_id ASC)")

dbExecute(shn, "CREATE TABLE corpus_rdata(corpus_id INTEGER NOT NULL, object_type TEXT NOT NULL, lda_n_topics INTEGER, storr_key TEXT UNIQUE NOT NULL, FOREIGN KEY (corpus_id) REFERENCES corpus(corpus_id) ON DELETE CASCADE, UNIQUE (corpus_id, object_type, storr_key))")

sh_st <- storr_dbi("storr_data", "storr_keys", shn, binary = TRUE)

documents %>%
  collect() %>%
  dbAppendTable(shn, "documents", .)

document_metadata %>%
  collect() %>%
  dbAppendTable(shn, "document_metadata", .)

corpora %>%
  collect() %>%
  dbAppendTable(shn, "corpus", .)

corpus_document %>%
  collect() %>%
  dbAppendTable(shn, "corpus_document", .)

corpus_rdata %>%
  collect() %>%
  dbAppendTable(shn, "corpus_rdata", .)
