#' Generate "Open" Class
#'
#' This function checks a DataFrame and classifies rows as "Open" based on specific criteria.
#'
#' @param DF The input DataFrame to be modified.
#' @return A modified DataFrame with the "Open" class assigned in the `Subclass` column.
#'
#' @examples
#'
#' # Load required libraries if not already loaded
#' # library(your_library_name)
#'
#' # Create a sample DataFrame (DF)
#' DF <- data.frame(
#'   p3_klit = c("Eng", "Hede", "Mose", "Overdrev", NA, NA),
#'   Urort_Skov = c(NA, NA, NA, NA, "Untouched", "Untouched"),
#'   Forest = c(NA, NA, NA, NA, NA, NA),
#'   markblokkort = c(NA, NA, NA, NA, NA, NA),
#'   National_Parks = c(NA, NA, NA, NA, NA, NA),
#'   Subclass = c(NA, NA, NA, NA, NA, NA)
#' )
#'
#' # Apply the OpenGenerator function to classify rows as "Open"
#' DF <- OpenGenerator(DF)
#'
#' @export
#'
OpenGenerator <- function(DF) {
  if (is.null(DF$Subclass)) {
    DF$Subclass <- NA
  }
  DF$Subclass <- ifelse(
    (DF$p3_klit %in% c("Eng", "Hede", "Mose", "Overdrev", "Strandeng", "Klit") &
       !is.na(DF$p3_klit) &
       is.na(DF$Urort_Skov) &
       is.na(DF$Forest) &
       is.na(DF$markblokkort)),
    "Open",
    DF$Subclass
  )
  return(DF)
}
