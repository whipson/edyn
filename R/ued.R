#' Utterance Emotion Dynamics
#'
#' Takes a data frame of utterances (e.g., tweets, sentences, dialogue, words, etc.) and computes Utterance Emotion Dynamics.
#' This function can be used to compute UED metrics for the whole dataset or groups in the dataset.
#'
#' @param data A data frame or tibble containing the data.
#' @param text Input text column.
#' @param lexicon Emotion lexicon. Currently VAD lexicon is only option.
#' @param id Optional: id column for grouping.
#' @param time Optional: Column indicating the temporal ordering of the text.
#' If the data are pre-arranged in temporal order this can be ignored.
#' @param stop_words Optional: character string or data frame containing stop words.
#' If entering a data frame, function will use the first character column it sees.
#' @param roll_avg Number of words to use in the rolling average.
#' @param level What alpha level should be used to compute the home base? Defaults to 0.68 corresponding to +- 1 SD.
#' @param disp_length_min Minimum number of words for displacements.
#' @param summarise If TRUE, output will be summarised such that each row corresponds to an id with each column representing
#' the mean for the particular metric. If FALSE, output will contain a row for each word.
#' @param va_limits A range of scores in the middle of the VA lexicon to trim.
#' This is useful for eliminating "neutral" VA terms.
#'
#' @import
#' @return data.frame or tibble
#' @export
#'
#' @examples
ued <- function(data, text, lexicon = "vad", id = NULL, time = NULL, stop_words = NULL, roll_avg = 10, level = .68,
                disp_length_min = 3, summarise = FALSE, trim_middle = c(0, 1)) {

  #TODO: Error for setting roll_avg too high.

  require(tidytext)
  require(tibbletime)

  if(!is.numeric(roll_avg) | roll_avg < 0) {
    stop("Argument 'roll_avg' must be a positive integer, or 0 for no averaging.")
  }

  if(!is.numeric(level) | level < 0 | level > 1) {
    stop("Argument 'level' must be between 0 and 1.")
  }

  if(va_limits[1] >= va_limits[2]) {
    stop("va_limits[1] must be less than va_limits[2].")
  }

  enq_time <- enquo(time)
  is_time_null <- is.null(rlang::eval_tidy(enq_time, data))

  tokenized <- tidytext::unnest_tokens(data, word, {{text}})

  if(!is.null(stop_words)) {
    if(is.data.frame(stop_words)) {
      if(ncol(stop_words) > 1) {
        stop_words_tibble <- tibble(stop_words)
        word_cols <- purrr::map_lgl(stop_words_tibble, is.character)
        if(!any(word_cols)) {
          stop("No 'character' columns in 'stop_words'.")
        }
        sw <- stop_words_tibble[,min(which(word_cols))]
        warning("Using column '", names(sw), "' of ", substitute(stop_words), " as the stop words. Dropping other columns.",
                call. = FALSE, immediate. = TRUE)
      }
      names(sw) <- "word"
    } else if(is.character(stop_words)) {
      sw <- data.frame(word = stop_words)
    } else {
      stop("Argument 'stop_words' must be either a dataframe with a 'word' column OR a character vector of words.")
    }

    tokenized <- tokenized %>%
      anti_join(sw, "word")
  }

  if(lexicon == "vad") {
    lex <- readr::read_tsv("lexica/NRC-VAD-Lexicon.txt") %>%
      rename_all(function(x) stringr::str_to_lower(x)) %>%
      select(-dominance) %>%
      filter(across(valence:arousal, ~.x >= va_limits[1], ~.x <= va_limits[2])) %>%
      rename_if(is.character, "word")
  } else if(ncol(lexicon) < 2) {
    stop("Lexicon must contain at least one column containing words and at least one column containing word scores.")
  } else {
    lex <- lexicon
  }

  lex_names <- names(lex)[names(lex) != "word"]

  tokens_with_sentiments <- tokenized %>%
    inner_join(lex, by = "word")

  if(roll_avg != 0) {
    if(is_time_null) {
      warning("No time variable provided. Proceeding under the assumption that data are arranged temporally.",
              call. = FALSE, immediate. = TRUE)

      tokens_with_sentiments <- tokens_with_sentiments %>%
        group_by({{id}}) %>%
        filter(n() >= 200) # Hard coded for now
    } else {
      tokens_with_sentiments <- tokens_with_sentiments %>%
        group_by({{id}}) %>%
        filter(n() >= 200) %>% # Hard coded for now
        arrange({{id}}, {{time}})
    }

    data_roll <- tokens_with_sentiments %>%
      mutate(across(matches(lex_names), rollify(mean, roll_avg)),
             time_num = seq.int(n()),
             time_prop = time_num/max(time_num)) %>%
      filter(across(matches(lex_names), ~ !is.na(.x)))
  } else {
    data_roll <- tokens_with_sentiments
  }

  ued_output <- compute_dynamics(data_roll, !!sym(lex_names[1]), !!sym(lex_names[2]),
                                 id = {{id}}, time = time_num, level = level, disp_length_min)

  if(summarise) {
    ued_output <- ued_output %>%
      group_by({{id}}) %>%
      summarise(across(avg_emo_var:recovery_rate, mean, na.rm = TRUE),
                n_words = n()) %>%
      ungroup() %>%
      select(-disp_num, -is_peak)
  }

  return(ued_output)
}
