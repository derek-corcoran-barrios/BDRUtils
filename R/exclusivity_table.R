#' Calculate Exclusivity Table
#'
#' Given a data frame with dummy variables (encoded as "Yes" and NA), and an 'Area_sq_Km' column, this
#' function calculates exclusivity statistics for specified variables.
#'
#' @param Vars Character vector of variable names for which exclusivity should be calculated.
#' @param DF Data frame containing the dummy variables and 'Area_sq_Km' column.
#' @return A data frame summarizing exclusivity statistics for the specified variables.
#'
#' @examples
#'
#' # Create a sample data frame 'LT'
#' LT <- data.frame(
#'   planned_strictly_protected = c("Yes", "Yes", "Yes", "Yes", NA, NA, NA, NA),
#'   strictly_protected = c("Yes", "Yes", NA, NA, "Yes", "Yes", NA, NA),
#'   sea_natural_areas = c("Yes", NA, "Yes", NA, "Yes", NA, "Yes", NA),
#'   Area_sq_Km = c(4163.9369, 0.0043, 136.5007, 0.0001, 2153.2234, 23.0762, 24540.5325, 327940.2363)
#' )
#'
#' # Calculate exclusivity table
#' exclusivity_table(DF = LT, Vars = c("strictly_protected", "sea_natural_areas"))
#'
#' @importFrom dplyr mutate rowwise filter summarise rename bind_rows relocate ungroup full_join everything
#' @importFrom purrr reduce
#' @importFrom magrittr "%>%"
#' @importFrom utils combn
#' @export
#'


exclusivity_table <- function(Vars, DF){
  Yes <- Area_sq_Km <- Variable <- .data <- NULL
  n = length(Vars)

  Combinations <- utils::combn(Vars, m = n - 1) %>% as.data.frame()

  Table <- list()

  for(i in 1:ncol(Combinations)){
    Var <- Vars[!(Vars %in% Combinations[,i])]

    Temp <- DF %>%
      dplyr::filter(!is.na(.data[[Var]])) %>%
      rowwise %>%
      dplyr::mutate(Yes = sum(dplyr::c_across(dplyr::all_of(Vars)) == "Yes", na.rm = TRUE)) %>%
      ungroup()


    Exclusive <- Temp %>%
      dplyr::filter(Yes == 1) %>%
      dplyr::summarise(Area_sq_Km = sum(Area_sq_Km)) %>%
      rename(Exclusive = Area_sq_Km) %>%
      dplyr::mutate(Variable = Var)

    NonExclusive <- Temp %>%
      dplyr::filter(Yes > 1) %>%
      dplyr::summarise(Area_sq_Km = sum(Area_sq_Km)) %>%
      dplyr::rename(Non_exclusive = Area_sq_Km) %>%
      dplyr::mutate(Variable = Var)

    All <- Temp %>%
      dplyr::summarise(Area_sq_Km = sum(Area_sq_Km)) %>%
      dplyr::rename(Total = Area_sq_Km) %>%
      dplyr::mutate(Variable = Var)

    Table[[i]] <- list(All, Exclusive, NonExclusive) %>%
      purrr::reduce(dplyr::full_join) %>%
      dplyr::relocate(Variable, .before = everything())
  }



  Total <- DF %>%
    rowwise %>%
    dplyr::mutate(Yes = sum(dplyr::c_across(dplyr::all_of(Vars)) == "Yes", na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::filter(Yes > 0) %>%
    dplyr::summarise(Area_sq_Km = sum(Area_sq_Km)) %>%
    dplyr::rename(Total = Area_sq_Km) %>%
    dplyr::mutate(Variable = "Total")

  Table <- Table %>%
    purrr::reduce(bind_rows)
  Table <- dplyr::bind_rows(Total, Table) %>%
    dplyr::relocate(Variable, .before = everything())
  return(Table)
}
