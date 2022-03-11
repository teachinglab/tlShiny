#' #' @title Dashboard Time Series Plot
#' #' @description Creates a plot for session survey data with an adjustable time series component
#' #' @param data the data to be input
#' #' @param scale the date scale to use for the plot
#' #' @return Returns a ggplot
#' #' @export
#'
# session_agree_plot_ts <- function(data, scale = "1 month") {
#'
#'   df <- data %>%
#'     dplyr::select(
#'       Date,
#'       c(
#'         "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
#'         "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
#'         "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
#'         "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
#'         "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
#'       )
#'     ) %>%
#'     tidyr::pivot_longer(!`Date`, names_to = "question", values_to = "answer") %>%
#'     dplyr::mutate(question = stringr::str_remove_all(
#'       question,
#'       "How much do you agree with the following statements about this course\\? - "
#'     )) %>%
#'     # Rename with line breaks every 27 characters
#'     dplyr::mutate(question = gsub("(.{28,}?)\\s", "\\1\n", question)) %>%
#'     tidyr::drop_na(answer) %>%
#'     dplyr::group_by(question, Date) %>%
#'     # Group by input variable
#'     dplyr::mutate(`Number Agree/Disagree` = n()) %>%
#'     dplyr::mutate(answer = stringr::str_remove_all(answer, "\\([:digit:]\\) ")) %>%
#'     dplyr::mutate(
#'       Rating = dplyr::case_when(
#'         answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
#'         answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
#'       ),
#'       date_group = dplyr::case_when(scale == "1 month" ~ paste0(lubridate::month(Date, label = T, abbr = F), ", ", lubridate::year(Date)),
#'                              scale == "1 week" ~ paste0(lubridate::year(Date), lubridate::week(Date)),
#'                              scale == "1 day" ~ paste0(lubridate::day(Date)))
#'     ) %>%
#'     dplyr::ungroup() %>%
#'     dplyr::mutate(question = stringr::str_remove_all(
#'       question,
#'       "How much do you agree with the\nfollowing statements about this\nfacilitator today\\? - "
#'     )) %>%
#'     dplyr::group_by(date_group, question) %>%
#'     dplyr::mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) %>%
#'     dplyr::filter(Rating == "Agree/Strongly Agree") %>%
#'     dplyr::group_by(date_group, Rating, question) %>%
#'     dplyr::summarise(Percent = round(sum(Percent), 2),
#'               Date = Date)
#'
#'   df %>%
#'     ggplot2::ggplot(mapping = ggplot2::aes(x = lubridate::ymd(Date),
#'              y = Percent)) +
#'     ggplot2::geom_area(color = "gray50", mapping = ggplot2::aes(fill = Rating),
#'                        alpha = 0.6, position = ggplot2::position_identity()) + # position_identity is absolutely necessary here, not sure why
#'     ggplot2::geom_ribbon(color = "transparent", ggplot2::aes(
#'       ymin = Percent, ymax = 100,
#'       fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"
#'     ), alpha = 0.85) +
#'     ggplot2::geom_line(size = 1.25, alpha = 0.9, mapping = ggplot2::aes(group = 1)) +
#'     ggplot2::geom_point(size = 1, alpha = 0.9) +
#'     ggplot2::facet_wrap(~ fct_reorder(question, .x = `Percent`, .fun = mean, .desc = T)) +
#'     ggplot2::coord_cartesian() +
#'     ggplot2::scale_x_date(
#'       date_breaks = scale,
#'       date_labels = if (scale == "1 month") { "%b, %Y" } else if (scale == "1 week") { "%W" } else if (scale == "1 day") { "%W" },
#'       limits = c(min(df$Date), max(df$Date)),
#'       expand = c(0, 0)
#'     ) +
#'     ggplot2::scale_y_continuous(
#'       breaks = scales::pretty_breaks(n = 5), limits = c(0, 100),
#'       labels = scales::percent_format(scale = 1), expand = c(0, 0)
#'     ) +
#'     ggplot2::scale_fill_manual(values = c(rev(tl_palette(n = 2, color = "blue", theme = "dark")))) +
#'     ggplot2::labs(x = "Date", title = glue::glue("{if (scale == '1 month') {'Monthly'} else if (scale == '1 week') {'Weekly'} else if (scale == '1 day') {'Daily'}} Percent that Agree/Strongly Agree")) +
#'     ggplot2::theme_bw() + # BW Panel panel elements
#'     ggplot2::theme(
#'       legend.position = "bottom",
#'       legend.title = ggplot2::element_blank(),
#'       legend.text = ggplot2::element_text(family = "Calibri"),
#'       text = ggplot2::element_text(family = "Calibri", face = "bold"),
#'       panel.grid.minor.x = ggplot2::element_blank(),
#'       axis.text.x = ggplot2::element_text(size = 10),
#'       strip.background = ggplot2::element_rect(fill = "white"),
#'       strip.text = ggplot2::element_text(size = 13),
#'       axis.title.x = ggplot2::element_blank(),
#'       axis.title.y = ggplot2::element_blank(),
#'       plot.title = ggplot2::element_text(hjust = 0.5, family = "Calibri", size = 20, face = "bold"),
#'       axis.line = ggplot2::element_line(size = 1.5)
#'     )
#' }
#'
#' #' @title Dashboard Agree Type \% Plot
#' #' @description Creates a plot for session survey data that shows \% in each Likert category
#' #' @param data the data to be input
#' #' @return Returns a ggplot
#' #' @export
#'
#' session_agree_plot <- function(data) {
#'
#'   n <- nrow(data)
#'
#'   df <- data %>%
#'     dplyr::select(c(
#'       "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
#'       "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
#'       "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
#'       "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
#'       "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
#'     )) %>%
#'     tidyr::pivot_longer(tidyselect::everything(), names_to = "Question", values_to = "Response") %>%
#'     tidyr::drop_na() %>%
#'     dplyr::mutate(Question = stringr::str_remove_all(
#'       Question,
#'       "How much do you agree with the following statements about this facilitator today\\? - "
#'     )) %>%
#'     dplyr::group_by(Question, Response) %>%
#'     dplyr::count() %>%
#'     dplyr::ungroup() %>%
#'     dplyr::group_by(Question) %>%
#'     dplyr::mutate(Question = stringr::str_wrap(Question, width = 30)) %>%
#'     dplyr::summarise(
#'       n = n,
#'       Response = Response,
#'       Percent = n / sum(n) * 100
#'     )
#'
#'   df %>%
#'     ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
#'     ggplot2::geom_col() +
#'     ggplot2::geom_text(mapping = ggplot2::aes(label = dplyr::if_else(Percent >= 3, paste0(round(Percent), "%"), "")),
#'                        position = ggplot2::position_stack(vjust = 0.5)) +
#'     ggplot2::scale_fill_manual(values = c(
#'       "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
#'       "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
#'     )) +
#'     ggplot2::labs(
#'       fill = "", title = "Participant Perceptions of Session - Likert Scale Questions",
#'       x = "", y = "",
#'       subtitle = glue::glue("Given the filters applied there are {n} responses")
#'     ) +
#'     ggplot2::coord_flip() +
#'     ggplot2::guides(fill = ggplot2::guide_legend(reverse = T)) +
#'     ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#'     tlShiny::theme_tl(legend = T) +
#'     ggplot2::theme(
#'       axis.text.x = ggplot2::element_blank(),
#'       axis.text.y = ggplot2::element_text(lineheight = 1.1, size = 14),
#'       legend.position = "bottom",
#'       plot.title = ggplot2::element_text(lineheight = 1.1, size = 20, face = "bold"),
#'       plot.subtitle = ggplot2::element_text(size = 14, face = "bold"),
#'       legend.key.size = ggplot2::unit(1.25, "cm"),
#'       legend.text = ggplot2::element_text(size = 9)
#'     )
#' }

#' @title Negative Conditional Filter
#' @description Conditionally filters value given that it is not the first parameter, for use in shiny apps
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







