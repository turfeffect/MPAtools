#' Convert data format
#'
#' @description Convert fish transect data from format a to c
#'
#' @param b An object of class data.frame with the format a (one column for each size interval).
#'
#' @return c An object of class data.frame with the format c (one column for size and one row for each occurrence).
#'
#' @export
#'
#' @author Villaseñor-Derbez, J.C. <juancarlos.villader@gmail.com>
#'

b2c=function(b){

  library(dplyr)  # Load dplyr
  library(tidyr)  # Load tidyr
  library(reshape) #load reshape


  colnames(b)=c('Dia', #Set proper column names to avoid werid characters
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

  c=untable(df=b, num=b$Abundancia) #Untable b to convert to c

  c$Abundancia=1                    #Set abundances = to one


  return(c) #return a data.frame

}
