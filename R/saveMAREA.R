
#' Title
#'
#' @param data data to store
#' @param comunidad community
#' @param reserva reserve
#'
#' @export
#'
#' @examples
#'

saveMAREA <- function(data, comunidad, reserva, token) {

  # Create a unique file name
  fileName <- sprintf("%s_%s_%s.RData", as.integer(Sys.time()), comunidad, digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  save(data, file = filePath)
  # Upload the file to Dropbox
  drop_upload(file = filePath, path = "MAREA_Data", autorename = T, dtoken = token)
}
