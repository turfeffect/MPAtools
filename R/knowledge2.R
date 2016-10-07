knowledge <- function(location, D, S, TL) {


fisheranswers <- read.csv(file="C:/Users/melai/Documents/GP/fisheranswers.csv", header=TRUE, sep=",", stringsAsFactors = F)


################################################################################################################
#Manually inputting correct values for reserve type and year:


reservename <- c("Maria Elena", "El Rosario")

year_correct <- c(2012,2000)

reservetype_correct <- c("Refugio Pesquero Total Temporal", "Refugio Pesquero Total Permanent")

correctanswertable <- data.frame(reservename, year_correct, reservetype_correct)


####################################################################################################################
##########################################################################################################

#select reserve of interest

fisherreserveselect <- filter(fisheranswers, reservename == location)

correctreserveselect <- filter(correctanswertable, reservename == location)


#################################################################################################################

#Find the correct values for the biophysical indicators using the density function (etc.) in the MPA tools package, then label a positive score as improved and a negative score as worsened (which matches the answers given on the survey)

# For density
  if (D > 0) {
    density_correct = "Improved"
  }
  if (D < 0) {
    density_correct = "Worsened"
  }

# For richness
  if (S > 0) {
    richness_correct = "Improved"
  }
  if (S < 0) {
    richness_correct = "Worsened"
  }


# For trophic level

  if (TL > 0) {
    trophic_correct = "Improved"
  }
  if (TL < 0) {
    trophic_correct = "Worsened"
  }


#################################################################################################################
#creating a table with the correct values

  correctvalues <- cbind(correctreserveselect, density_correct, trophic_correct, richness_correct)

##########Determining scores:


  if (fisherreserveselect$year == correctvalues$year_correct) {
    YearScore =1
  }
  if (fisherreserveselect$year != correctvalues$year_correct) {
    YearScore =0
  }


ReservetypeScore <-
  if (fisherreserveselect$reservetype == correctvalues$reservetype_correct) {
    ReservetypeScore =1
  }

  if (fisherreserveselect$reservetype != correctvalues$reservetype_correct) {
    ReservetypeScore =0
  }


DensityScore <-
  if (fisherreserveselect$density == correctvalues$density_correct) {
    DensityScore =1
  }

  if (fisherreserveselect$density != correctvalues$density_correct) {
    DensityScore =0
  }


TrophicScore <-
  if (fisherreserveselect$trophic == correctvalues$trophic_correct) {
    TrophicScore =1
  }
  if (fisherreserveselect$trophic != correctvalues$trophic_correct) {
    TrophicScore =0
  }


RichnessScore <-
  if (fisherreserveselect$richness == correctvalues$richness_correct) {
    RichnessScore =1
  }

  if (fisherreserveselect$richness != correctvalues$richness_correct) {
    RichnessScore =0
  }



#################################################################################################
#FinalScore

FinalScore = YearScore + ReservetypeScore + DensityScore + TrophicScore + RichnessScore

return(FinalScore)


}




#
#
#
#
# knowledge <- function(x) {
#
#   CorrectAnswersDatabase <- read.csv(file="C:/Users/melai/Documents/GP/CorrectAnswersDatabase.csv", header=TRUE, sep=",")
#
#   x= YearScore + ReservetypeScore + DensityScore + TrophicScore + RichnessScore
#
#   #########Correct values:
#
#   ######Step 1: Create a database of the correct year of implementation and reserve type for each reserve
#
#   ###Step 2: Create a function that will select by reserve name (location) and give you the year of implementation of that reserve
#
#   year_correct <- 2012
#   ########just want to select a particular cell in the database...
#
#   reservetype_correct <- function (location) {
#
#     subset(CorrectAnswersDatabase$ReserveName == location, CorrectAnswersDatabase$ReserveType)}
#   ####just want to select a particular cell in the database.....
#
#
#   ##############Step 3: Find the correct values for the biophysical indicators using the density function in the MPA tools package, then label a positive score as improved and a negative score as worsened (which matches the answers given on the survey)
#
#   density_correct <- density(data, location, species = NULL)
#
#   est = x$est
#
#   if (est > 0) {
#     score = "Improved"
#   }
#
#   if (est < 1) {
#     score = "Worsened"
#   }
#
#
#   trophic_correct <- trophic(data, location)
#   est = x$est
#
#   if (est > 0) {
#     score = "Improved"
#   }
#
#   if (est < 1) {
#     score = "Worsened"
#   }
#
#
#   richness_correct <- richness(data, location)
#   est = x$est
#
#   if (est > 0) {
#     score = "Improved"
#   }
#
#   if (est < 1) {
#     score = "Worsened"
#   }
#
#   ##################Step 4:
#
#   #####Determining scores:
#
#   YearScore <-
#     if (fisher value from .csv = year_correct) {
#       score =1
#     }
#   if (fisher value from .csv not equal to year_correct) {
#     score =0
#   }
#
#
#   ReservetypeScore <-
#     if (fisher value from .csv = reservetype_correct) {
#       score =1
#     }
#
#   if (fisher value from .csv not equal to reservetype_correct) {
#     score =0
#   }
#
#
#   DensityScore <-
#     if (fisher value from .csv = density_correct) {
#       score =1
#     }
#
#   if (fisher value from .csv = density_correct) {
#     score =0
#   }
#
#
#   TrophicScore <-
#     if (fisher value from .csv = trophic_correct) {
#       score =1
#     }
#   if (fisher value from .csv = trophic_correct) {
#     score =0
#   }
#
#
#   RichnessScore <-
#     if (fisher value from .csv = richness_correct) {
#       score =1
#     }
#
#   if (fisher value from .csv = richness_correct) {
#     score =0
#   }
#
# }

