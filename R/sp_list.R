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
sp_list <- function(fish, invert = NULL, rc) {

  year_min_fish <- min(fish$Ano[fish$RC %in% rc])

  fish_sp_before_reserve <- fish %>%
    filter(Ano <= year_min_fish,
           !is.na(Abundancia),
           RC %in% rc,
           Zona == "Reserva") %>%
    group_by(Ano, Transecto, GeneroEspecie) %>% 
    count() %>% 
    filter(n > 1) %>% 
           {.$GeneroEspecie} %>%
    unique() %>%
    data.frame(stringsAsFactors = F) %>%
    mutate(who1 = "before_reserve") %>%
    magrittr::set_colnames(value = c("sp", "who"))

  fish_sp_before_control <- fish %>%
    filter(Ano <= year_min_fish,
           !is.na(Abundancia),
           RC %in% rc,
           Zona == "Control") %>%
           {.$GeneroEspecie}%>%
    unique() %>%
    data.frame(stringsAsFactors = F) %>%
    mutate(who2 = "before_control") %>%
    magrittr::set_colnames(value = c("sp", "who"))

  fish_sp_after_reserve <- fish %>%
    filter(Ano > year_min_fish,
           !is.na(Abundancia),
           RC %in% rc,
           Zona == "Reserva") %>%
           {.$GeneroEspecie}%>%
    unique() %>%
    data.frame(stringsAsFactors = F) %>%
    mutate(who3 = "after_reserve") %>%
    magrittr::set_colnames(value = c("sp", "who"))

  fish_sp_after_control <- fish %>%
    filter(Ano > year_min_fish,
           !is.na(Abundancia),
           RC %in% rc,
           Zona == "Control") %>%
           {.$GeneroEspecie}%>%
    unique() %>%
    data.frame(stringsAsFactors = F) %>%
    mutate(who4 = "after_control") %>%
    magrittr::set_colnames(value = c("sp", "who"))

  sp_list <- inner_join(fish_sp_before_reserve, fish_sp_before_control, by = "sp") %>%
    inner_join(fish_sp_after_reserve, by = "sp") %>%
    inner_join(fish_sp_after_control, by = "sp") %>%
    select(sp) %>%
    arrange(sp) %>%
    mutate(class = "fish")


  if (!is.null(invert)) {

    year_min_invert <- min(invert$Ano[invert$RC %in% rc])

    invert_sp_before_reserve <- invert %>%
      filter(Ano <= year_min_invert,
             !is.na(Abundancia),
             RC %in% rc,
             Zona == "Reserva") %>%
             {.$GeneroEspecie}%>%
      unique() %>%
      data.frame(stringsAsFactors = F) %>%
      mutate(who1 = "before_reserve") %>%
      magrittr::set_colnames(value = c("sp", "who"))

    invert_sp_before_control <- invert %>%
      filter(Ano <= year_min_invert,
             !is.na(Abundancia),
             RC %in% rc,
             Zona == "Control") %>%
             {.$GeneroEspecie}%>%
      unique() %>%
      data.frame(stringsAsFactors = F) %>%
      mutate(who2 = "before_control") %>%
      magrittr::set_colnames(value = c("sp", "who"))

    invert_sp_after_reserve <- invert %>%
      filter(Ano > year_min_invert,
             !is.na(Abundancia),
             RC %in% rc,
             Zona == "Reserva") %>%
             {.$GeneroEspecie}%>%
      unique() %>%
      data.frame(stringsAsFactors = F) %>%
      mutate(who3 = "after_reserve") %>%
      magrittr::set_colnames(value = c("sp", "who"))

    invert_sp_after_control <- invert %>%
      filter(Ano > year_min_invert,
             !is.na(Abundancia),
             RC %in% rc,
             Zona == "Control") %>%
             {.$GeneroEspecie}%>%
      unique() %>%
      data.frame(stringsAsFactors = F) %>%
      mutate(who4 = "after_control") %>%
      magrittr::set_colnames(value = c("sp", "who"))

    invert <- inner_join(invert_sp_before_reserve, invert_sp_before_control, by = "sp") %>%
      inner_join(invert_sp_after_reserve, by = "sp") %>%
      inner_join(invert_sp_after_control, by = "sp") %>%
      select(sp) %>%
      arrange(sp) %>%
      mutate(class = "invert")

    sp_list <- rbind(sp_list, invert)
  }

  return(sp_list)
}
