#' Title
#'
#' @param fish a fish database
#' @param invert an invert database
#' @param rc the reserve - control pair
#'
#' @return sp_list
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#'
sp_list <- function(fish, invert, rc) {

  sp_list <- fish %>%
    filter(!is.na(Abundancia), RC %in% rc)  %>%
    {.$GeneroEspecie}%>%
    unique() %>%
    sort() %>%
    data.frame() %>%
    mutate(class = "fish") %>%
    magrittr::set_colnames(value = c("sp", "class"))

  if (!is.null(invert)) {
    invert <- invert %>%
      filter(!is.na(Abundancia), RC %in% rc) %>%
      {.$GeneroEspecie}%>%
      unique() %>%
      sort() %>%
      data.frame() %>%
      mutate(class = "invert") %>%
      magrittr::set_colnames(value = c("sp", "class"))

    sp_list <- rbind(sp_list, invert)
  }

  return(sp_list)
}
