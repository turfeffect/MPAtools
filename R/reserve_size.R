#' Title
#'
#' @param data 
#' @param spp 
#' @param length 
#' @param width 
#'
#' @return a list
#' @export
#'
#' @examples
#' 

reserve_size <- function(data, spp, length = NULL, width = NULL) {
  
  list_spp <- spp$sp[spp$class == "fish"]
  
  e = NA
  color = "yellow"
  string = "No disponible para invertebrados."
  plot = NA
  
  if (length(list_spp) > 0) {
  
    min_length <- min(length, width)
    
    required_length <- data %>% 
      dplyr::filter(GeneroEspecie %in% list_spp,
                    Zona == "Reserva") %>% 
      dplyr::group_by(GeneroEspecie) %>% 
      dplyr::summarize(Talla = log10(mean(Talla*10, na.rm = T))) %>% 
      mutate(spatial_length = 10 ^ (-3.75 + (2.35 * Talla))) %>% {
        .$spatial_length
      } %>% 
      max()
    
    e <- ifelse(required_length < min_length, 1, 0)
    string <- paste("El tamano minimo es", round(required_length))
    color <- ifelse(required_length <= min_length, "olive", "red")
    
    plot <- ggplot() +
      geom_col(aes(x = 0, y = min_length), fill = "steelblue", color = "black", size = 1) +
      geom_col(aes(x = 0, y = required_length), color = "black", linetype = "dashed", size = 1, alpha = 0) +
      coord_polar() +
      labs(x = "", y = "Distancia (m)", subtitle = "Linea punteada indica el minimo.") + 
      theme_bw() +
      scale_x_continuous(labels = NULL) +
      ggExtra::removeGrid()
  }
  
  return(list(e = e,
              color = color,
              string = string,
              plot = plot))
}