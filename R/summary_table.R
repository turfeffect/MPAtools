#' @export

summary_table <- function(results_bio, results_bio_i, results_soc, results_gov){

  results_bio <- results_bio %>%
    dplyr::filter(!is.na(color)) %>%
    dplyr::mutate(Puntaje = paste0(Ind, " ![](",color, ".gif)")) %>%
    {.$Puntaje}

  results_bio_i <- results_bio_i %>%
    dplyr::filter(!is.na(color)) %>%
    dplyr::mutate(Puntaje = paste0(Ind, " ![](",color, ".gif)")) %>%
    {.$Puntaje}

  results_soc <- results_soc %>%
    dplyr::filter(!is.na(color)) %>%
    dplyr::mutate(Puntaje = paste0(Ind, " ![](",color, ".gif)")) %>%
    {.$Puntaje}

  results_gov <- results_gov %>%
    dplyr::filter(!is.na(e)) %>%
    dplyr::mutate(Puntaje = paste0(Ind, " ![](",color, ".gif)")) %>%
    {.$Puntaje}

  length_bio <- length(results_bio)
  length_bio_i <- length(results_bio_i)
  length_soc <- length(results_soc)
  length_gov <- length(results_gov)

  max_length <- max(length_bio, length_bio_i, length_soc, length_gov)

  complete_bio <- rep(NA, max_length-length_bio)
  complete_bio_i <- rep(NA, max_length-length_bio_i)
  complete_soc <- rep(NA, max_length-length_soc)
  complete_gov <- rep(NA, max_length-length_gov)

  sum_table <- data.frame(Peces = c(results_bio, complete_bio),
             Invertebrados = c(results_bio_i, complete_bio_i),
             Socioeconomicos = c(results_soc, complete_soc),
             Gobernanza = c(results_gov, complete_gov), stringsAsFactors = F)

  sum_table[is.na(sum_table)] <- ""

  return(sum_table)
}
