
#' @title Negative Conditional Filter
#' @description Conditionally filters value given that it is not the first parameter,
#' for use in shiny apps
#' @param data the dataframe to apply filter
#' @param if_not_this If value is not this
#' @param filter_this Filter for this
#' @param dat_filter Data column object to filter
#' @return filtered dataframe
#' @export
neg_cond_filter <- function(data, if_not_this, filter_this, dat_filter) {

  # Get quo for filtering the data
  quo_filter <- rlang::enquo(dat_filter)

  # Get a vector of the inputs to filter minus the "All x" pattern
  filter_this_no_all <- filter_this[filter_this != if_not_this]

  # Check if any of the filters are not the "All x" pattern and filter for the inputs if that is TRUE
  if (any(filter_this != if_not_this)) {
    df <- data %>%
      dplyr::filter(!!quo_filter %in% filter_this_no_all)
  } else {
    df <- data
  }

  df

}

#' @title Verify email domain
#' @description Verifies that an email is of a specified domain
#' @param email the email provided
#' @param domain the domain to check that it is from
#' @return TRUE or FALSE
#' @export
check_email_domain <- function(email, domain) {
  grepl(paste0("@", domain, "$"), email, ignore.case = TRUE)
}


#' @title Verify email from vector
#' @description Verifies that an email is in a list
#' @param email the email provided
#' @param approved_emails_list the email to check the list for
#' @return TRUE or FALSE
#' @export
check_email_approved <- function(email, approved_emails_list) {
  email %in% approved_emails_list
}


#' @title HTML/CSS Button Content Expander
#' @description Creates a button that will expand or hide content
#' @param before button default, collapsed/not collapsed
#' @param options unclear
#' @param envir also unclear
#' @param name chunk name
#' @return html wrapper
#' @export
drop1 <- function(before = T, options, envir, name) {

  if (before) {
    paste(
      '<p>',
      glue::glue('<button class="btn btn-primary collapsed" data-toggle="collapse" data-target="{name}">'),
      '</button>',
      '</p>',
      glue::glue('<div class="collapse" id="{name}">'),
      '<div class="card card-body">',  sep = "\n")
  } else {
    paste("</div>", "</div>", sep = "\n")
  }

}

#' @title HTML Text Wrapping
#' @description Takes a string and inserts <br> at the requested intervals
#' @param string the string
#' @param n the width of the string before a <br> tag
#' @return the same string with <br> inserted at the requested interval
#'
#' @examples
#' html_wrap("a random string that has about 40 characters in it")
#' @export

html_wrap <- function(string, n = 40) {
  stringr::str_replace_all(
    stringr::str_wrap(string = string, width = n),
    "\n", "<br>")
}


#' @title Find elements x not in a vector y
#'
#' @name notin
#' @aliases notin
#' @param x A vector of what shouldn't exist
#' @param y A vector to check against
#' @return Returns elements not in vector
#' @export

'%!in%' <- function (x, y) {
  !('%in%'(x, y))
}


#' @title Calculate nps score
#'
#' @param x A vector of nps scores
#' @return Returns the nps score
#' @export

calc_nps <- function(x) {
  nps <- round(((length(which(x %in% c(9, 10))) / length(x)) - (length(which(x %in% c(0:6))) / length(x))) * 100, 2)
  return(nps)
}







