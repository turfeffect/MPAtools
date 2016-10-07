# knowledge <- function(x) {
#
#   CorrectAnswersDatabase <- read.csv(file="C:/Users/melai/Documents/GP/CorrectAnswersDatabase.csv", header=TRUE, sep=",")
#
#   x= YearScore + ReservetypeScore + DensityScore + TrophicScore + RichnessScore
#
# #########Correct values:
#
# ######Step 1: Create a database of the correct year of implementation and reserve type for each reserve
#
# ###Step 2: Create a function that will select by reserve name (location) and give you the year of implementation of that reserve
#
#   year_correct <- 2012
# ########just want to select a particular cell in the database...
#
#   reservetype_correct <- function (location) {
#
#     subset(CorrectAnswersDatabase$ReserveName == location, CorrectAnswersDatabase$ReserveType)}
# ####just want to select a particular cell in the database.....
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
# ##################Step 4:
#
# #####Determining scores:
#
# YearScore <-
#   if (fisher value from .csv = year_correct) {
#     score =1
#   }
#   if (fisher value from .csv not equal to year_correct) {
#     score =0
#   }
#
#
# ReservetypeScore <-
#   if (fisher value from .csv = reservetype_correct) {
#     score =1
#   }
#
#   if (fisher value from .csv not equal to reservetype_correct) {
#     score =0
#   }
#
#
# DensityScore <-
#   if (fisher value from .csv = density_correct) {
#     score =1
#   }
#
#   if (fisher value from .csv = density_correct) {
#     score =0
#   }
#
#
# TrophicScore <-
#   if (fisher value from .csv = trophic_correct) {
#     score =1
#   }
#   if (fisher value from .csv = trophic_correct) {
#     score =0
#   }
#
#
# RichnessScore <-
#   if (fisher value from .csv = richness_correct) {
#     score =1
#   }
#
#   if (fisher value from .csv = richness_correct) {
#     score =0
#   }
#
# }
