library(DBI)
library(RSQLite)
library(tidyverse)
library(storr)

db <- dbConnect(SQLite(), "data/shiny.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = ON")
st <- storr_dbi("storr_data", "storr_keys", db, binary = TRUE)

documents <- tbl(db, "documents")
document_metadata <- tbl(db, "document_metadata")
corpora <- tbl(db, "corpus")
corpus_document <- tbl(db, "corpus_document")
corpus_rdata <- tbl(db, "corpus_rdata")
