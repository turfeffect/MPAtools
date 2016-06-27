#' Convert data format
#'
#' @description
#'
#' @param c An object of class data.frame wih the format c (One column for sizes and one for abundance).
#'
#' @return b An object of class data.frame with the format b (one column for each size interval).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

c2b=function(c){
  library(reshape)
  library(dplyr)
  library(tidyr)

  colnames(c)=c('Dia', #Set proper column names to avoid weird characters
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
                'ProfundidadFinal_m',
                'Temperatura_C',
                'Visibilidad_m',
                'Corriente',
                'Transecto',
                'Genero',
                'Especie',
                'GeneroEspecie',
                'Sexo',
                'Talla',
                'PromedioDeTalla',
                'Abundancia')

  b=c %>% #Set b equal to c
    group_by(Dia, #Group by all columns except for Abundancia
             Mes,
             Ano,
             Estado,
             Comunidad,
             Sitio,
             Latitud,
             Longitud,
             Habitat,
             Zonificacion,
             TipoDeProteccion,
             ANP,
             BuzoMonitor,
             HoraInicialBuceo,
             HoraFinalBuceo,
             ProfundidadInicial_m,
             ProfundidadFinal_m,
             Temperatura_C,
             Visibilidad_m,
             Corriente,
             Transecto,
             Genero,
             Especie,
             GeneroEspecie,
             Sexo,
             Talla,
             PromedioDeTalla) %>%
    summarize(Abundancia=sum(Abundancia)) #Sum the column Abundania to gather ell records into a single row

  return(b) #return a data.frame

}
