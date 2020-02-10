exploravec
================

[Shiny] app for exploring subsets foa  corpus with different methods, including TF-IDF, term frequencies over time, topic modelling, etc.

## Data setup

A persistent sqlite database resides in `data/corpus.sqlite3` and contains tables tracking documents, metadata, and various ngrams.
This is meant to acommodate both documents where we have access to full text, as well as documents where we only have n-gram access, such as those from [JSTOR DFR][dfr].

[Shiny]: https://shiny.rstudio.com

[dfr]: https://www.jstor.org/dfr/
