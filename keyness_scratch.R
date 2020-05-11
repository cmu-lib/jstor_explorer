library(fs)
library(quanteda)
library(tidytext)
library(tidyverse)
library(drake)
library(stm)

effect_size <- function (n_target, n_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
  log2(percent_a / percent_b)
}

token_plan <- drake_plan(
  unigrams = dir_ls("~/Development/ai-ethics-data/all_ethics", recurse = TRUE, regexp = "ngram1/*.txt"),
  bigrams = dir_ls("~/Development/ai-ethics-data/all_ethics", recurse = TRUE, regexp = "ngram2/*.txt"),
  long_unigram_df = map_df(unigrams, possibly(function(p) {
    read_tsv(p, col_types = "ci", col_names = c("token", "n"))
  }, otherwise = NULL), .id = "docstring") %>%
    mutate(docstring = str_replace(path_ext_remove(path_file(docstring)), "-ngram1", "")) %>%
    distinct(token, docstring, .keep_all = TRUE),
  accepted_bigrams = c("artificial intelligence", "big data", "machine learning", "computer science"),
  long_bigram_df = map_df(bigrams, possibly(function(p) {
      read_tsv(p, col_types = "ci", col_names = c("token", "n")) %>%
        filter(token %in% accepted_bigrams)
  }, otherwise = NULL), .id = "docstring") %>%
    mutate(docstring = str_replace(path_ext_remove(path_file(docstring)), "-ngram2", "")) %>%
    distinct(token, docstring, .keep_all = TRUE),
  combined_tokens = bind_rows(long_unigram_df, long_bigram_df),
  trimmed_tokens = combined_tokens %>%
    mutate(token_length = nchar(token)) %>%
    add_count(token, name = "token_count") %>%
    filter(token_length >= 3, token_count > 10),
  ethics_dfm = cast_dfm(trimmed_tokens, document = "docstring", term = "token", value = "n"),
  ai_ethics_docs =  rownames(ethics_dfm)[as.logical(ethics_dfm[,"artificial intelligence"]) & as.logical(ethics_dfm[,"ethics"])],
  ai_no_ethics_docs = rownames(ethics_dfm)[as.logical(ethics_dfm[,"artificial intelligence"]) & !(as.logical(ethics_dfm[,"ethics"]))],
  ml_no_ethics_docs = rownames(ethics_dfm)[as.logical(ethics_dfm[,"machine learning"]) & !(as.logical(ethics_dfm[,"ethics"]))],
  ml_ethics_docs = rownames(ethics_dfm)[as.logical(ethics_dfm[,"machine learning"]) & as.logical(ethics_dfm[,"ethics"])],
  ethics_only_docs = rownames(ethics_dfm)[as.logical(ethics_dfm[,"ethics"]) & !(as.logical(ethics_dfm[,"artificial intelligence"])) & !(as.logical(ethics_dfm[,"machine learning"]))],
  trimmed_dfm = ethics_dfm %>%
    dfm_remove(pattern = "\\d", valuetype = "regex") %>%
    dfm_remove(pattern = " ", valuetype = "regex") %>%
    dfm_remove(stopwords("en")) %>%
    dfm_remove(stopwords("es")) %>%
    dfm_remove(stopwords("de")) %>%
    dfm_trim(min_termfreq = 10, termfreq_type = "count", min_docfreq = 0.01, max_docfreq = 0.8, docfreq_type = "prop"),
  stemmed_dfm = trimmed_dfm %>%
    dfm_wordstem(language = "en"),
  keyness_tester = trimmed_dfm %>% # Remove unigrams that we don't want used as part of the keyness check
    dfm_remove(c("intellig", "artifici", "https", "machin", "comput", "ethic", "learn")),
  keyness = target(
    textstat_keyness(x, target = fac) %>%
      mutate(effect_size = effect_size(n_target, n_reference)) %>%
      filter(p < 0.05) %>%
      arrange(desc(effect_size)),
    transform = cross(
      x = list("unstemmed" = trimmed_dfm, "stemmed" = stemmed_dfm),
      fac = list(ai_ethics_docs, ai_no_ethics_docs, ml_ethics_docs, ml_no_ethics_docs, ethics_only_docs))
  ),
  distinctive_ai_ethics_terms = keyness_stemmed_dfm_ai_ethics_docs %>%
    filter(ntile(effect_size, 100) >= 80) %>%
    pull(feature),
  reduced_ai_ethics_corpus = stemmed_dfm[ai_ethics_docs,distinctive_ai_ethics_terms],
  reduced_ai_ethics_corpus_tm = target(
    stm(reduced_ai_ethics_corpus, K = topic_count),
    transform = cross(
      topic_count = c(10, 20, 30)
    )
  )
)

make(token_plan)

loadd(keyness_stemmed_dfm_ai_ethics_docs)
loadd(keyness_stemmed_dfm_ai_no_ethics_docs)
loadd(keyness_stemmed_dfm_ethics_only_docs)

joined_stems <- keyness_stemmed_dfm_ai_ethics_docs %>%
  left_join(keyness_stemmed_dfm_ethics_only_docs, by = "feature", suffix = c("_ai_ethics", "_ethics_only")) %>%
  left_join(keyness_stemmed_dfm_ai_no_ethics_docs, by = "feature", suffix = c("", "_ai_only"))

