count_n_grams <- function(dt, text_col, n = 2) {
  data(stop_words)
  dt %>% unnest_tokens(
    output = n_grams,
    input = text_col,
    token = 'ngrams',
    n = n
  ) %>% filter(!is.na(n_grams)) %>%
    count(n_grams, sort = TRUE) %>%
    tidyr::separate(col = n_grams,
                    into = paste0('word_', seq(n)),
                    sep = " ") %>%
    filter(across(matches(paste0(
      "^word_[1-", n, "]$"
    )), ~ !. %in% stop_words$word)) %>%
    filter(n > 1)
}