#' Convert data format
#'
#' @description Convert fish transect data from format a to format b
#'
#' @param a An object of class data.frame with the format a (one column for each size interval).
#'
#' @return b An object of class data.frame wih the format b (One column for sizes and one for abundance).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>#'
#'

a2b=function(a){

  library(dplyr)  # Load dplyr
  library(tidyr)  # Load tidyr

  colnames(a)=c('Dia',
                'Mes',
                'Ano',
                'Estado',
                'Comunidad',
                'Sitio',
                'Latitud',
                'Longitud',
                'Habitat',
                'Zonificacion',
                'TipoDeProteccion',
                'ANP',
                'BuzoMonitor',
                'HoraInicialBuceo',
                'HoraFinalBuceo',
                'ProfundidadInicial_m',
                'ProfundiadFinal_m',
                'Temperatura_C',
                'Visibilidad_m',
                'Corriente',
                'Transecto',
                'Genero',
                'Especie',
                'GeneroEspecie',
                'Sexo',
                '0a5',
                '6a10',
                '11a20',
                '21a30',
                '31a40',
                '>40',
                "Talla",
                "Total")

  b=a
  b$row=1:nrow(b)
  b=b%>%
    select(-Total) %>%
    spread(Talla, ">40") %>%
    select(-row) %>%
    gather(Talla, Abundancia, -c(1:25)) %>%
    filter(Abundancia>0)

  #Las líneas de abajo asignan los nombres correctos a las celdas, e inlcuyen los promedios que deben de ser utilizados:

  ## Lo hacemos para la columna Promedio de Talla (la que se usa en el análisis)
  b$PromedioDeTalla=as.numeric(b$Talla)
  b$PromedioDeTalla[b$Talla=="0a5"]=2.5
  b$PromedioDeTalla[b$Talla=="6a10"]=8.5
  b$PromedioDeTalla[b$Talla=="11a20"]=15.5
  b$PromedioDeTalla[b$Talla=="21a30"]=25.5
  b$PromedioDeTalla[b$Talla=="31a40"]=35.5

  ## Y lo hacemos para la columna Talla
  b$Talla[b$Talla=="0a5"]="0a5"
  b$Talla[b$Talla=="6a10"]="6a10"
  b$Talla[b$Talla=="11a20"]="11a20"
  b$Talla[b$Talla=="21a30"]="21a30"
  b$Talla[b$Talla=="31a40"]="31a40"
  b$Talla[b$PromedioDeTalla>=41]=">40"

  b=b %>%
    select(c(1:26, 28, 27))

  colnames(b)=c('Día',
                'Mes',
                'Año',
                'Estado',
                'Comunidad',
                'Sitio',
                'Latitud',
                'Longitud',
                'Hábitat',
                'Zonificación',
                'Tipo de protección',
                'ANP',
                'Buzo Monitor',
                'Hora inicial buceo',
                'Hora final buceo',
                'Profundidad inicial (m)',
                'Profundiad final (m)',
                'Temperatura (°C)',
                'Visibilidad (m)',
                'Corriente',
                'Transecto',
                'Género',
                'Especie',
                'Género + Especie',
                'Sexo',
                'Talla',
                'Promedio de talla',
                'Abundancia')

  return(b)

}
