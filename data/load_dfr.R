library(DBI)
library(fs)
library(tidyverse)
library(glue)
library(progress)

db <- dbConnect(RSQLite::SQLite(), "data/corpus.sqlite3")
dbExecute(db, "PRAGMA foreign_keys = ON")
RSQLite::initRegExp(db)

dbExecute(db, "CREATE TABLE IF NOT EXISTS documents(document_id INTEGER PRIMARY KEY NOT NULL, docstring TEXT UNIQUE NOT NULL)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS document_xml(document_id INTEGER PRIMARY KEY NOT NULL, xml TEXT NOT NULL, FOREIGN KEY (document_id) REFERENCES documents(document_id))")
dbExecute(db, "CREATE TABLE IF NOT EXISTS document_fulltext(document_id INTEGER PRIMARY KEY NOT NULL, fulltext TEXT NOT NULL, FOREIGN KEY (document_id) REFERENCES documents(document_id))")
dbExecute(db, "CREATE TABLE IF NOT EXISTS document_metadata(document_id INTEGER PRIMARY KEY NOT NULL, doc_type TEXT NOT NULL, item_title TEXT, authors TEXT, parent_title TEXT, date TEXT, year INTEGER, abstract TEXT, language TEXT, url TEXT, FOREIGN KEY(document_id) REFERENCES documents(document_id))")
dbExecute(db, "CREATE INDEX document_date ON document_metadata(date DESC)")
dbExecute(db, "CREATE INDEX document_year ON document_metadata(year DESC)")
dbExecute(db, "CREATE INDEX document_type ON document_metadata(doc_type)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS unigrams(unigram_id INTEGER PRIMARY KEY NOT NULL, unigram TEXT UNIQUE NOT NULL, is_numeric BOOLEAN NOT NULL)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS bigrams(bigram_id INTEGER PRIMARY KEY NOT NULL, bigram TEXT UNIQUE NOT NULL, gram1 INTEGER NOT NULL, gram2 INTEGER NOT NULL,
          FOREIGN KEY(gram1) REFERENCES unigrams(unigram_id), FOREIGN KEY (gram2) REFERENCES unigrams(unigram_id), UNIQUE(gram1, gram2))")
# dbExecute(db, "CREATE TABLE IF NOT EXISTS trigrams(trigram_id INTEGER PRIMARY KEY NOT NULL, trigram TEXT UNIQUE NOT NULL, gram1 INTEGER NOT NULL, gram2 INTEGER NOT NULL, gram3 INTEGER NOT NULL,
#           FOREIGN KEY(gram1) REFERENCES unigrams(unigram_id), FOREIGN KEY (gram2) REFERENCES unigrams(unigram_id), FOREIGN KEY (gram3) REFERENCES unigrams(unigram_id), UNIQUE(gram1, gram2, gram3))")
dbExecute(db, "CREATE INDEX IF NOT EXISTS bigrams_gram1 ON bigrams(gram1)")
dbExecute(db, "CREATE INDEX IF NOT EXISTS bigrams_gram2 ON bigrams(gram2)")
# dbExecute(db, "CREATE INDEX IF NOT EXISTS trigrams_gram1 ON trigrams(gram1)")
# dbExecute(db, "CREATE INDEX IF NOT EXISTS trigrams_gram2 ON trigrams(gram2)")
# dbExecute(db, "CREATE INDEX IF NOT EXISTS trigrams_gram3 ON trigrams(gram3)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS document_unigram(document_id INTEGER NOT NULL, unigram_id INTEGER NOT NULL, n INTEGER NOT NULL, FOREIGN KEY(document_id) REFERENCES documents(document_id), FOREIGN KEY(unigram_id) REFERENCES unigrams(unigram_id), UNIQUE(document_id, unigram_id))")
dbExecute(db, "CREATE INDEX IF NOT EXISTS document_unigram_document ON document_unigram(document_id ASC)")
dbExecute(db, "CREATE INDEX IF NOT EXISTS document_unigram_unigram ON document_unigram(unigram_id ASC)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS document_bigram(document_id INTEGER NOT NULL, bigram_id INTEGER NOT NULL, n INTEGER NOT NULL, FOREIGN KEY(document_id) REFERENCES documents(document_id), FOREIGN KEY(bigram_id) REFERENCES bigrams(bigram_id), UNIQUE(document_id, bigram_id))")
dbExecute(db, "CREATE INDEX IF NOT EXISTS document_bigram_document ON document_bigram(document_id ASC)")
dbExecute(db, "CREATE INDEX IF NOT EXISTS document_bigram_bigram ON document_bigram(bigram_id ASC)")
# dbExecute(db, "CREATE TABLE IF NOT EXISTS document_trigram(document_id INTEGER NOT NULL, trigram_id INTEGER NOT NULL, n INTEGER NOT NULL, FOREIGN KEY(document_id) REFERENCES documents(document_id), FOREIGN KEY(trigram_id) REFERENCES trigrams(id), UNIQUE(document_id, trigram_id))")
# dbExecute(db, "CREATE INDEX IF NOT EXISTS document_trigram_document ON document_trigram(document_id)")
# dbExecute(db, "CREATE INDEX IF NOT EXISTS document_trigram_trigram ON document_trigram(trigram_id)")

dbExecute(db, "CREATE TABLE IF NOT EXISTS errors(message TEXT NOT NULL, t TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")


dbExecute(db, "CREATE TABLE IF NOT EXISTS corpus(corpus_id INTEGER PRIMARY KEY NOT NULL, label TEXT UNIQUE NOT NULL)")
dbExecute(db, "CREATE TABLE IF NOT EXISTS corpus_document(corpus_id INTEGER NOT NULL, document_id INTEGER NOT NULL, FOREIGN KEY (corpus_id) REFERENCES corpus(corpus_id), FOREIGN KEY (document_id) REFERENCES documents(document_id), UNIQUE(corpus_id, document_id))")
dbExecute(db, "CREATE INDEX IF NOT EXISTS corpus_document_corpus ON corpus_document(corpus_id)")
dbExecute(db, "CREATE INDEX IF NOT EXISTS corpus_document_document ON corpus_document(document_id)")

# XML ----

document_xmls <- dir_ls("~/Development/ai-ethics-data/", recurse = TRUE, glob = "*.xml")

pb <- progress_bar$new(total = length(document_xmls))
dbBegin(db)
walk(document_xmls, function(f) {
  pb$tick()
  docstring <- path_ext_remove(path_file(f))
  xmltext <- read_file(f)
  df <- tibble(
    docstring = docstring,
    xml = xmltext,
  )
  dbWriteTable(db, "temp_documents", df, temporary = TRUE, overwrite = TRUE)
  dbExecute(db, "INSERT INTO documents(docstring) SELECT docstring FROM temp_documents")
  dbExecute(db, "INSERT INTO document_xml SELECT documents.document_id, temp_documents.xml FROM temp_documents INNER JOIN documents USING (docstring)")
})
dbCommit(db)

dbGetQuery(db, "SELECT MAX(rowid) FROM documents")

# Metadata ----
source("data/R/xml_parser.R")
dbExecute(db, "DELETE FROM errors")
doc_ids <- dbGetQuery(db, "SELECT document_id FROM documents WHERE NOT EXISTS (SELECT 1 FROM document_metadata WHERE document_metadata.document_id=documents.document_id)")$document_id
pb <- progress_bar$new(format = "  processing :current [:bar] :percent eta: :eta",
                       total = length(doc_ids))
walk(doc_ids, function(i) {
  pb$tick()
  doc <- dbGetQuery(db, glue("SELECT document_xml.xml AS xml, documents.docstring AS docstring FROM documents INNER JOIN document_xml USING (document_id) WHERE document_id={i}"))
  doc_xml <- read_xml(doc$xml) %>% xml_ns_strip()
  tryCatch({
    parsed_metadata <- parse_jstor_metadata(doc_xml, doc$docstring, i)
    dbAppendTable(db, "document_metadata", parsed_metadata)
  },
    error = function(e) {
      dbAppendTable(db, "errors", tibble(message = glue("{i} failed xml parsing: {e}")))
      return(NA)
    }
  )
})

# Unigrams ----

unigrams <- dir_ls("~/Development/ai-ethics-data/", recurse = TRUE, regexp = "ngram1/*.txt")

upb <- progress_bar$new(format = "  processing :current [:bar] :percent eta: :eta",
                        total = length(unigrams))
walk(unigrams, function(f) {
  upb$tick()
  docstring <- str_replace(path_ext_remove(path_file(f)), "-ngram1", "")
  tryCatch({
    ngrams <- read_tsv(f, col_types = "ci", col_names = c("unigram", "n")) %>%
      mutate(is_numeric = str_detect(unigram, "\\d"))
    ngrams$docstring <- docstring
    dbWriteTable(db, "temp_unigrams", ngrams, temporary = TRUE, overwrite = TRUE)
    dbExecute(db, "INSERT OR IGNORE INTO unigrams(unigram, is_numeric) SELECT DISTINCT unigram, is_numeric FROM temp_unigrams")
    dbExecute(db, "INSERT OR IGNORE INTO document_unigram
            SELECT documents.document_id as document_id, unigrams.unigram_id as unigram_id, temp_unigrams.n AS n
            FROM temp_unigrams
            INNER JOIN documents USING (docstring)
            INNER JOIN unigrams USING (unigram)")
    dbExecute(db, "DROP TABLE temp_unigrams")
  }, error = function(e) {
    dbAppendTable(db, "errors", tibble(message = glue("{f} had error: {e}")))
    return(NA)
  })
})

# Bigrams ----

completed_ids <- dbGetQuery(db, "SELECT DISTINCT documents.docstring FROM document_bigram INNER JOIN documents USING (document_id)")$docstring

bigrams_raw <- dir_ls("~/Development/ai-ethics-data/", recurse = TRUE, regexp = "ngram2/*.txt")

bigrams_filter <- bigrams_raw %>%
  path_file() %>%
  path_ext_remove() %>%
  str_replace("-ngram2", "")

bigrams <- bigrams_raw[!(bigrams_filter %in% completed_ids)]

upb <- progress_bar$new(format = "  processing :current [:bar] :percent eta: :eta",
                        total = length(bigrams))
walk(bigrams, function(f) {
  upb$tick()
  docstring <- str_replace(path_ext_remove(path_file(f)), "-ngram2", "")
  tryCatch({
    ngrams <- read_tsv(f, col_types = "ci", col_names = c("bigram", "n")) %>%
      separate(bigram, into = c("gram1", "gram2"), sep = " ", remove = FALSE) %>%
      mutate(
        is_numeric1 = str_detect(gram1, "\\d"),
        is_numeric2 = str_detect(gram2, "\\d")
      )
    ngrams$docstring <- docstring
    dbWriteTable(db, "temp_bigrams", ngrams, temporary = TRUE, overwrite = TRUE)
    dbExecute(db, "INSERT OR IGNORE INTO unigrams(unigram, is_numeric)
              SELECT gram1 AS unigram, is_numeric1 AS is_numeric FROM temp_bigrams
              UNION
              SELECT gram2 AS unigram, is_numeric2 AS is_numeric FROM temp_bigrams")

    dbExecute(db, "INSERT OR IGNORE INTO bigrams(bigram, gram1, gram2) SELECT DISTINCT bigram, u1.unigram_id AS gram1, u2.unigram_id AS gram2
               FROM temp_bigrams
               INNER JOIN unigrams AS u1 ON temp_bigrams.gram1=u1.unigram
               INNER JOIN unigrams AS u2 ON temp_bigrams.gram2=u2.unigram")

    # Why is this nested subquery for the bigram ID so much faster than doing some easier-to-read INNER JOINs? IDK?
    dbExecute(db, "INSERT OR IGNORE INTO document_bigram
            SELECT documents.document_id as document_id, (SELECT bigram_id FROM bigrams WHERE
              gram1=(SELECT unigram_id FROM unigrams WHERE unigram=temp_bigrams.gram1) AND
              gram2=(SELECT unigram_id FROM unigrams WHERE unigram=temp_bigrams.gram2)) as bigram_id,
            temp_bigrams.n AS n
            FROM temp_bigrams
            INNER JOIN documents USING (docstring)")
    dbExecute(db, "DROP TABLE temp_bigrams")
  }, error = function(e) {
    dbAppendTable(db, "errors", tibble(message = glue("{f} had error: {e}")))
    return(NA)
  })
})

# Trigrams ----

completed_ids <- dbGetQuery(db, "SELECT DISTINCT documents.docstring FROM documents INNER JOIN document_trigram USING (document_id)")$docstring

trigrams_raw <- dir_ls("~/Development/ai-ethics-data/", recurse = TRUE, regexp = "ngram3/*.txt")

trigrams_filter <- trigrams_raw %>%
  path_file() %>%
  path_ext_remove() %>%
  str_replace("-ngram3", "")

trigrams <- trigrams_raw[!(trigrams_filter %in% completed_ids)]

upb <- progress_bar$new(format = "  processing :current [:bar] :percent eta: :eta",
                        total = length(trigrams))
walk(trigrams, function(f) {
  upb$tick()
  docstring <- str_replace(path_ext_remove(path_file(f)), "-ngram3", "")
  tryCatch({
    ngrams <- read_tsv(f, col_types = "ci", col_names = c("trigram", "n")) %>%
      separate(trigram, into = c("gram1", "gram2", "gram3"), sep = " ", remove = FALSE) %>%
      mutate(
        is_numeric1 = str_detect(gram1, "\\d"),
        is_numeric2 = str_detect(gram2, "\\d"),
        is_numeric3 = str_detect(gram3, "\\d"),
      )
    ngrams$docstring <- docstring
    dbWriteTable(db, "temp_trigrams", ngrams, temporary = TRUE, overwrite = TRUE)
    dbExecute(db, "INSERT OR IGNORE INTO unigrams(unigram, is_numeric)
              SELECT gram1 AS gram, is_numeric1 AS is_numeric from temp_trigrams UNION
              SELECT gram2 AS gram, is_numeric2 AS is_numeric FROM temp_trigrams UNION
              SELECT gram3 AS gram, is_numeric3 AS is_numeric from temp_trigrams")
    dbExecute(db, "INSERT OR IGNORE INTO trigrams(trigram, gram1, gram2, gram3) SELECT DISTINCT temp_trigrams.trigram, u1.unigram_id AS gram1, u2.unigram_id AS gram2, u3.unigram_id AS gram3
              FROM temp_trigrams
              INNER JOIN unigrams AS u1 ON temp_trigrams.gram1=u1.unigram
              INNER JOIN unigrams AS u2 ON temp_trigrams.gram2=u2.unigram
              INNER JOIN unigrams AS u3 ON temp_trigrams.gram3=u3.unigram")


    # Why is this nested subquery for the trigram ID so much faster than doing some easier-to-read INNER JOINs? IDK?
    dbExecute(db, "INSERT OR IGNORE INTO document_trigram
            SELECT documents.document_id as document_id, (SELECT trigrams.trigram_id FROM trigrams WHERE
              gram1=(SELECT unigram_id FROM unigrams WHERE unigram=temp_trigrams.gram1) AND
              gram2=(SELECT unigram_id FROM unigrams WHERE unigram=temp_trigrams.gram2) AND
              gram3=(SELECT unigram_id FROM unigrams WHERE unigram=temp_trigrams.gram3)) as trigram_id,
            temp_trigrams.n AS n
            FROM temp_trigrams
            INNER JOIN documents USING (docstring)")
    dbExecute(db, "DROP TABLE temp_trigrams")
  }, error = function(e) {
    dbAppendTable(db, "errors", tibble(message = glue("{f} had error: {e}")))
    return(NA)
  })
})

dbDisconnect(db)
