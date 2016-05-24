#' Calculate bbiomass
#'
#' @description Calulates biomass for a selected species or the entire dataset, by year and transect, for each location.
#'
#' @param data A dataframe that contains at least columns of Year, Zone (e.g. reserve or control site), Transect Number, GenusSpecies.
#' @param site A quoted string that indicates the site. Options are "Rosario", "IslaNatividad", "IslaMagdalena".
#' @param species A quoted string that indicates a species for which density should be calulated. Common cases include "Paralabrax clathratus", "Semicossipjus pulcher", or "Anisotremus davidsonii".
#'
#' @return size A dataframe containing year, zone (insido or outside the reserve), transect number, species, and the statistic required.
