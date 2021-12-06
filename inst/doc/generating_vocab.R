## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#  # Install non-CRAN packages
#  # remotes::install_github("macmillancontentscience/wikimorphemes")
#  # remotes::install_github("macmillancontentscience/wordpiece.data")
#  library(morphemepiece)
#  library(wikimorphemes)
#  library(wordpiece.data)
#  library(dplyr)
#  library(ggplot2)
#  library(purrr)

## ----load-data----------------------------------------------------------------
#  # load functions to make the vocab + lookup
#  source(here::here("vignettes", "make_vocab_and_lookup.R"))
#  
#  # Load data from various packages related to this task.
#  original_lookup <- readRDS(wikimorphemes::download_wikimorphemes_lookup())
#  
#  # TODO: Add something to reproduce this or host it somewhere.
#  word_frequency_table <- readRDS(
#    fs::path(
#      morphemepiece_cache_dir(),
#      "word_frequency_table.rds"
#    )
#  )
#  
#  # Not all wiktionary words are in the wiktionary lookup. Use the full word list
#  # to add short words back in.
#  full_lookup <- .add_words_to_lookup(
#    original_lookup,
#    wikimorphemes::wiktionary_word_list()
#  )

## ----count-tokens-------------------------------------------------------------
#  # for much of this process, it's more convenient to have the processed words
#  # unnested, with one morpheme per row. This takes a few minutes.
#  
#  # Currently, we're considering only words with pure lowercase latin characters.
#  # We likely will want to also include the simplified version of words with
#  # accented characters in this list.
#  unnested_lookup <- .unnest_lookup(full_lookup, clean = TRUE)
#  
#  # count how many wiktionary words each token appears in.
#  token_counts <- count_tokens(unnested_lookup)
#  
#  utils::head(token_counts)

## ----temp-values, eval = TRUE, include = FALSE--------------------------------
# This is here so the package can still build properly when this vignette is
# "turned off."
words_vs_tokens <- list(
  max_rank = 194347L,
  n_words = 643745L
)

## ----token-ranks--------------------------------------------------------------
#  # Some words process into "non-clean" tokens (diacrits, etc.).
#  # Those tokens are excluded in token_counts, so will get an NA here.
#  # Deal with this better later, but for now, just remove those words.
#  # (only about 0.01% of words)
#  
#  # Find the highest-rank (rarest) token within each word.
#  words_with_max_token_ranks <- dplyr::left_join(
#    unnested_lookup,
#    token_counts,
#    by = "token"
#  ) %>%
#    dplyr::group_by(word) %>%
#    dplyr::summarize(max_rank = max(rank)) %>%
#    dplyr::filter(!is.na(max_rank))
#  
#  # Count how many total words are covered by tokens up to some rank:
#  words_vs_tokens <- words_with_max_token_ranks %>%
#    dplyr::group_by(max_rank) %>%
#    dplyr::summarize(n_words = dplyr::n_distinct(word)) %>%
#    dplyr::arrange(max_rank) %>%
#    dplyr::mutate(n_words = cumsum(n_words))

## ----plot-counts--------------------------------------------------------------
#  # plot!
#  words_vs_tokens %>%
#    dplyr::mutate(frac_words = n_words/max(n_words)) %>%
#    dplyr::mutate(frac_tokens = max_rank/max(max_rank)) %>%
#    ggplot2::ggplot(ggplot2::aes(x = frac_tokens, y = frac_words)) +
#    ggplot2::geom_point()
#  

## ----count-weighted-----------------------------------------------------------
#  # passing a word frequency table in (columns: word, word_count) applies weights
#  # to the token counts.
#  token_counts_weighted <- count_tokens(unnested_lookup, word_frequency_table)
#  utils::head(token_counts_weighted)

## ----plot-weights-------------------------------------------------------------
#  words_with_max_token_ranks_weighted <- dplyr::left_join(
#    unnested_lookup,
#    token_counts_weighted,
#    by = "token"
#  ) %>%
#    dplyr::group_by(word) %>%
#    dplyr::summarize(max_rank = max(rank)) %>%
#    dplyr::filter(!is.na(max_rank))
#  
#  weighted_tokens_and_words <- dplyr::left_join(
#    words_with_max_token_ranks_weighted,
#    word_frequency_table,
#    by = "word"
#  ) %>%
#    dplyr::mutate(
#      word_count = ifelse(
#        test = is.na(word_count),
#        yes = 1L,
#        no = word_count
#      )
#    )
#  
#  words_vs_tokens_weighted <- weighted_tokens_and_words %>%
#    dplyr::group_by(max_rank) %>%
#    dplyr::summarize(n_words = sum(word_count)) %>%
#    dplyr::arrange(max_rank) %>%
#    dplyr::mutate(n_words = cumsum(n_words))
#  
#  # plot!
#  words_vs_tokens_weighted %>%
#    dplyr::mutate(frac_words = n_words/max(n_words)) %>%
#    dplyr::mutate(frac_tokens = max_rank/max(max_rank)) %>%
#    ggplot2::ggplot(ggplot2::aes(x = frac_tokens, y = frac_words)) +
#    ggplot2::geom_point()

## ----make-vocabs--------------------------------------------------------------
#  vandl_small <- make_vocab_and_lookup(
#    full_lookup = original_lookup,
#    full_vocabulary = wikimorphemes::wiktionary_word_list(),
#    wordpiece_vocab = wordpiece.data::wordpiece_vocab(),
#    target_vocab_size = 0, # no extra tokens
#    word_frequency_table = word_frequency_table
#  )
#  
#  vandl_large <- make_vocab_and_lookup(
#    full_lookup = original_lookup,
#    full_vocabulary = wikimorphemes::wiktionary_word_list(),
#    wordpiece_vocab = wordpiece.data::wordpiece_vocab(),
#    target_vocab_size = 30000L,
#    word_frequency_table = word_frequency_table
#  )

## ----make-lookups-------------------------------------------------------------
#  # TODO: Make this save and reload stuff unnecessary!
#  text_lookup_small <- .make_text_lookup(
#    voc = vandl_small$vocab,
#    lu = vandl_small$lookup,
#    word_freq_tab = word_frequency_table
#  )
#  
#  writeLines(
#    text_lookup_small,
#    file.path(morphemepiece_cache_dir(), "mp_lookup_small.txt")
#  )
#  # vocab is already just a character vector
#  writeLines(
#    vandl_small$vocab,
#    file.path(morphemepiece_cache_dir(), "mp_vocab_small.txt")
#  )
#  
#  # now do large
#  text_lookup_large <- .make_text_lookup(
#    voc = vandl_large$vocab,
#    lu = vandl_large$lookup,
#    word_freq_tab = word_frequency_table
#  )
#  
#  readr::write_lines(
#    text_lookup_large,
#    file.path(morphemepiece_cache_dir(), "mp_lookup_large.txt")
#  )
#  # vocab is already just a character vector
#  readr::write_lines(
#    vandl_large$vocab,
#    file.path(morphemepiece_cache_dir(), "mp_vocab_large.txt")
#  )
#  
#  # Read back from text files to process as standard {morphemepiece} files:
#  vocab <- load_or_retrieve_vocab(
#    file.path(morphemepiece_cache_dir(), "mp_vocab_large.txt")
#  )
#  lookup <- load_or_retrieve_lookup(
#    file.path(morphemepiece_cache_dir(), "mp_lookup_large.txt")
#  )
#  
#  morphemepiece_tokenize("Surprisingly easy", vocab, lookup)
#  morphemepiece_tokenize("'Twas brillig, and the slithy toves", vocab, lookup)

## ----check-coverage-small-----------------------------------------------------
#  corpus_coverage_small <- dplyr::left_join(
#    word_frequency_table,
#    vandl_small$lookup,
#    by = "word"
#  ) %>%
#    dplyr::mutate(covered_lookup = !is.na(tokenization)) %>%
#    # not every word in the vocab is in the lookup; check vocab too
#    dplyr::mutate(covered_vocab = word %in% vandl_small$vocab) %>%
#    dplyr::mutate(covered = covered_lookup | covered_vocab) %>%
#    dplyr::mutate(covered_weighted = covered*word_count)
#  
#  corpus_coverage_small %>%
#    dplyr::summarize(sum(covered_weighted)/sum(word_count))

## ----check-coverage-large-----------------------------------------------------
#  corpus_coverage_large <- dplyr::left_join(
#    word_frequency_table,
#    vandl_large$lookup,
#    by = "word"
#  ) %>%
#    dplyr::mutate(covered_lookup = !is.na(tokenization)) %>%
#    # not every word in the vocab is in the lookup; check vocab too
#    dplyr::mutate(covered_vocab = word %in% vandl_large$vocab) %>%
#    dplyr::mutate(covered = covered_lookup | covered_vocab) %>%
#    dplyr::mutate(covered_weighted = covered*word_count)
#  
#  corpus_coverage_large %>%
#    dplyr::summarize(sum(covered_weighted)/sum(word_count))

## ----show-coverage-large------------------------------------------------------
#  #large
#  uncovered <- corpus_coverage_large %>%
#    dplyr::filter(!.data$covered) %>%
#    dplyr::arrange(dplyr::desc(.data$word_count)) %>%
#    dplyr::select(.data$word, .data$word_count) %>%
#    head(100) %>%
#    dplyr::mutate(
#      tokenization = morphemepiece_tokenize(
#        .data$word,
#        vocab = vocab,
#        lookup = lookup
#      )
#    ) %>%
#    dplyr::rowwise() %>%
#    dplyr::mutate(
#      tokenization = paste(names(.data$tokenization), collapse = " ")
#    ) %>%
#    dplyr::ungroup()
#  
#  head(uncovered, 10)

## ----xkcd, eval = FALSE-------------------------------------------------------
#  # just for fun :-D
#  xkcd_words_url <- "https://xkcd.com/simplewriter/words.js"
#  raw_words <- readr::read_lines(xkcd_words_url)
#  
#  raw_words <- raw_words[grepl("WORDS", raw_words)]
#  raw_words <- stringr::str_split(raw_words, '"')[[1]]
#  raw_words <- raw_words[grepl("\\|", raw_words)]
#  words <- dplyr::tibble(top_words = stringr::str_split(raw_words, "\\|")[[1]])
#  # I feel lied to. There are more than 3k words in this list.
#  words <- words %>%
#    dplyr::mutate(
#      tokenized = morphemepiece_tokenize(.data$top_words, vocab, lookup)
#    ) %>%
#    dplyr::rowwise() %>%
#    dplyr::mutate(
#      tokenized = paste(names(.data$tokenized), collapse = " ")
#    ) %>%
#    dplyr::ungroup()
#  
#  # ss_url <- "url of mp_scratch google sheet"
#  
#  # already authorized
#  # googlesheets4::write_sheet(words, ss_url)
#  # manual check, add column "is_ok"
#  
#  # checked_words <- googlesheets4::read_sheet(ss_url, sheet = "check common words")
#  
#  # if breakdown is ok, value is "y"
#  
#  # mean(checked_words$is_ok == "y")
#  
#  # [1] 0.9711062
#  # many of the exceptions can/should be fixed in wiktionary
#  # These have not been checked in a while.

## ----more-checks, eval = FALSE------------------------------------------------
#  all_words <- unique(unnested_lookup$word)
#  unbroken_vocab_words <- intersect(names(vocab), all_words)
#  unbroken_vocab_words <- unbroken_vocab_words[nchar(unbroken_vocab_words) > 3]
#  
#  # sample a few hundred
#  unbroken_sample <- dplyr::tibble(unbroken_word = sample(unbroken_vocab_words,
#                                                          size = 300))
#  # send to google sheet for manual check
#  
#  # googlesheets4::write_sheet(unbroken_sample, ss_url, sheet = "unbroken_check")
#  # manual check, add column "is_ok"
#  # checked_unbroken_words <- googlesheets4::read_sheet(
#  #   ss_url,
#  #   sheet = "check unbroken words"
#  # )
#  
#  # if breakdown is ok, value is "y"
#  # table(checked_unbroken_words$is_ok)
#   #  ?   n   y
#   # 24  32 244
#  # many of the exceptions can/should be fixed in wiktionary

