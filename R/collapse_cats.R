#' Collapse categories.
#'
#' Collapse categories to an other category.
#' @x A data frame.
#' @var_name The unquoted variable name.
#' @levels A character vector with levels, depending on `keep` these
#'    are the levels that are kept or collapsed.
#' @new_category_name Name of the collapsed category.
#' @keep Logical, if TRUE the categories in `levels` are kept. If FALSE
#'     they are collapsed.
#' @new_var If NULL the new variable replaces the original in the
#'     data frame. Otherwise, this should be an unquoted variable name
#'     that will be added to the data frame.
#' @return The original data frame with either the original variable
#'      replaced, or a new variable added.
#' @examples
#' library(dplyr)
#' x <- data_frame(country = c("USA", "GER", "UK", "CAN", "FRA", "NL", "AUS"))
#' x %>% collapse_cats(country,
#'                     c("USA", "UK", "CAN", "AUS"))
#' x %>% collapse_cats(country,
#'                     c("USA", "UK", "CAN", "AUS"),
#'                     new_category_name = "Anglo",
#'                     keep = FALSE)
#' x %>% collapse_cats(country,
#'                     c("USA", "UK", "CAN", "AUS"),
#'                     new_var = country_new)
collapse_cats <- function(x,
                          var_name,
                          levels,
                          new_category_name = "other",
                          keep    = TRUE,
                          new_var = NULL) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(levels))
  var_name <- enquo(var_name)
  new_var  <- enquo(new_var)


  if (keep) {
    x <- mutate(x,
                "new_var" := ifelse(
                  (!!var_name) %in% levels,
                  !!var_name,
                  new_category_name))
  } else {
    x <- mutate(x,
                "new_var" := ifelse(
                  (!!var_name) %in% levels,
                  new_category_name,
                  !!var_name))
  }

  if ( rlang::quo_text(new_var) == "NULL") {
    new_var_name <- rlang::quo_text(var_name)
    x            <- mutate(x, !!new_var_name := new_var)
    x            <- select(x, -new_var)
  }
  else {
    new_var_name <- rlang::quo_text(new_var)
    x            <- rename(x, !!new_var_name := new_var)
  }
  x
}
