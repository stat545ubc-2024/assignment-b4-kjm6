#' Duck Latin Function
#'
#' Converts a word or list of words from english to duck latin
#'
#' @param .x The word or list of words to be converted
#'
#' @return The converted word or list of words
#'
#' @examples
#' word_list <- c("red", "orange", "yellow")
#' duck_latin(word_list)


duck_latin <- function(.x) {
  consonants <- c("b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z")
  
  # Function to check if a character is a consonant
  is_consonant <- function(char) {
    return(str_to_lower(char) %in% consonants)
  }
  
  # Function to check for the presence and length of a consonant cluster at the front
  starts_with_consonant_cluster <- function(.x) {
    cluster_end <- 1
    while (cluster_end <= str_length(.x) && is_consonant(str_sub(.x, cluster_end, cluster_end))) {
      cluster_end <- cluster_end + 1
    }
    return(cluster_end - 1)
  }
  
  # If the word starts with a consonant cluster, move the consonant cluster to the end and append "ak"
  consonant_cluster_len <- starts_with_consonant_cluster(.x)
  if (is.numeric(.x)){
    return("The input is numeric and cannot be translated to duck latin")
  }
  if (consonant_cluster_len > 0) {
    consonant_cluster <- str_sub(.x, 1, consonant_cluster_len)
    remaining_word <- str_sub(.x, consonant_cluster_len + 1, str_length(.x))
    return(str_c(remaining_word, consonant_cluster, "ak"))
  } else {
    # If the word starts with a vowel cluster, leave the word structure and append "ak" to the end
    return(str_c(.x, "ak"))
  }
}
