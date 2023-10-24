
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
#' With formula `%` promoters - `%` detractors where promoters are 9 or 10 ratings and detractors are 0 to 6
#' @export

calc_nps <- function(x) {
  nps <- round((
    (sum(x == 10 | x == 9, na.rm = T) / sum(!is.na(x))) -
      (sum(x == 6 | x == 5 | x == 4 | x == 3 | x == 2 | x == 1 | x == 0, na.rm = T) / sum(!is.na(x)))
  ) * 100, 2)
  return(nps)
}



#' @title Agree/Strongly agree
#' @description Gets the percent that agree and strongly agree
#' @param data the data
#' @param question a string - the question to get the percentage for
#' @examples
#' \dontrun{
#' plot_agree |>
#' agree_strongly_agree(question = "x")
#' }
#' @return a string
#' @export
agree_strongly_agree <- function(data, question) {
  data |>
    dplyr::filter(Question == question & Response %in% c("(4) Agree", "(5) Strongly agree")) |>
    dplyr::summarise(Percent = sum(Percent)) |>
    dplyr::pull(Percent) |>
    round() |>
    paste0("%")
}

#' @title File path
#' @description Gives the file path without double slash bug
#' @param ... The file path
#' @param fsep the file separation
#' @return fp a file path
#' @export
file.path2 <- function(..., fsep = .Platform$file.sep) {
  fp <- gsub("//", "/", file.path(..., fsep = fsep))
  return(fp)
}

#' @title No Data Plot
#' @description A plot that says no data available, and to check your filters
#' @export
no_data_plot_filters <- ggplot2::ggplot(data.frame(text = "No data available for this set of filters,\ncheck what you have set!", x = 0, y = 0)) +
  ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Calibri Bold", size = 10) +
  ggplot2::theme_void()

#' @title No Data Plot
#' @description A plot that says no data available yet this year
#' @export
no_data_plot_currently <- ggplot2::ggplot(data.frame(text = "No data available yet this year!", x = 0, y = 0)) +
  ggplot2::geom_text(ggplot2::aes(label = text, x, y), fontface = "bold", family = "Calibri Bold", size = 10) +
  ggplot2::theme_void()

#' @title GT or ggplot maker
#' @description makes a gt table with percent and n colored
#' @param df the data frame
#' @param column the column to get count and percent from
#' @param custom_title the title for the table
#' @param no_title make the table have no title
#' @param base_font overall table font size
#' @param heading_font title font size
#' @param custom_column_name a custom name for the column
#' @param viz_type gt by default, also has ggplot options like pie chart, waffle, or treemap
#' @return a gt table
#' @export
gt_percent_n <- function(df, column, custom_title, no_title = T, base_font = 10,
                         heading_font = 14, custom_column_name = "", viz_type = "gt") {

  if (nrow(df) >= 1) {
    column <- rlang::sym(column)

    if (viz_type == "gt") {
      df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        gt::gt() |>
        gt::cols_label({{ custom_column_name }} := gt::html(custom_column_name)) %>%
        {
          if (no_title == F) gt::tab_header(title = gt::md(glue::glue("*{custom_title}*"))) else .
        } %>%
        gt::data_color(
          columns = n,
          colors = scales::col_numeric(
            palette = tlShiny::tl_palette(color = "blue", n = 10),
            domain = NULL
          )
        ) |>
        gt::fmt_percent(
          columns = Percent,
          decimals = 2,
          scale_values = F
        ) |>
        gt::grand_summary_rows(
          columns = c(n),
          fns = list(
            Total = ~ sum(.)
          ),
          formatter = gt::fmt_number,
          decimals = 0
        ) |>
        gt::grand_summary_rows(
          columns = c(Percent),
          fns = list(
            Total = ~ sum(.)
          ),
          formatter = gt::fmt_percent,
          scale_values = F,
          decimals = 0
        ) %>%
        tlShiny::gt_theme_tl(base_font = base_font, heading_font = heading_font)
    } else if (viz_type == "pie") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(!!(column) := stringr::str_wrap(!!rlang::sym(column), width = 10),
                      Percent = round(100 * n / sum(n), 2)
        ) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.5 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )
      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          x = "", y = Percent,
          fill = !!rlang::ensym(custom_column_name)
        )) +
        ggplot2::geom_col(key_glyph = draw_key_point) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = paste0(Percent, "%"),
            y = ypos
          ),
          family = "Calibri",
          fontface = "bold",
          color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
                         "white",
                         "black"
          ),
          size = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
                        4,
                        6
          ),
          vjust = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent, -1.5, 0.5)
        ) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")")) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21, size = 10), reverse = T)) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = element_text(size = 17),
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20)
        )
    } else if (viz_type == "waffle") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.75 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )

      subtitle <- ggplot_data |>
        dplyr::select({{ custom_column_name }}, Percent, n) |>
        dplyr::arrange(dplyr::desc(Percent)) |>
        dplyr::mutate(color = rev(tlShiny::tl_palette(n = length(n), color = "blue"))) |>
        dplyr::summarise(text = stringr::str_c("<b style='color:", color, "'>", !!rlang::ensym(custom_column_name), ": ", Percent, "%</b>", collapse = "<br>")) |>
        dplyr::pull(text)

      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          fill = !!rlang::ensym(custom_column_name),
          values = n
        )) +
        ### ISSUE: some kind of weird error here internal to the library itself I think ###
        waffle::geom_waffle(
          n_rows = 10,
          size = 1, colour = "white",
          make_proportional = TRUE,
          na.rm = TRUE,
          radius = grid::unit(2, "pt"),
          height = 0.9,
          width = 0.9
        ) +
        ggplot2::labs(
          title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = TRUE), big.mark = ","), ")"),
          subtitle = subtitle
        ) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20),
          plot.subtitle = ggtext::element_markdown(
            hjust = 0.5, face = "italic",
            lineheight = 1.15, size = 18
          )
        )
    } else if (viz_type == "treemap") {
      ggplot_data <- df |>
        dplyr::group_by(!!column) |>
        dplyr::count(sort = T) |>
        tidyr::drop_na(!!column) |>
        dplyr::ungroup() |>
        dplyr::mutate(Percent = round(100 * n / sum(n), 2)) |>
        dplyr::rename({{ custom_column_name }} := {{ column }}) |>
        dplyr::mutate(
          prop = 100 * (Percent / sum(Percent)),
          ypos = cumsum(prop) - 0.5 * prop,
          {{ custom_column_name }} := forcats::fct_reorder(!!rlang::ensym(custom_column_name), Percent)
        )
      ggplot_data |>
        ggplot2::ggplot(ggplot2::aes(
          area = Percent,
          fill = !!rlang::ensym(custom_column_name)
        )) +
        treemapify::geom_treemap(key_glyph = draw_key_point) +
        treemapify::geom_treemap_text(ggplot2::aes(label = paste0(!!rlang::ensym(custom_column_name), ": ", Percent, "%")),
                                      family = "Calibri",
                                      fontface = "bold",
                                      # grow = T,
                                      reflow = T,
                                      color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10,
                                                     "white",
                                                     "black"
                                      ),
                                      place = "center"
        ) +
        ggplot2::labs(title = paste0(custom_column_name, " (n = ", format(sum(ggplot_data$n, na.rm = T), big.mark = ","), ")\n")) +
        ggplot2::scale_fill_manual(values = tlShiny::tl_palette(
          color = "blue",
          n = length(unique(ggplot_data[[custom_column_name]]))
        )) +
        ggplot2::theme_void(base_family = "Calibri") +
        ggplot2::theme(
          legend.position = "none",
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20)
        )
    }
  } else {
    tlShiny:::no_data_plot_filters
  }
}

#' @title Grade IPG Data
#' @param x the data
#' @param type character or numeric
#' @description function for grading different parts of the ipg forms
#' @return a percentage of correct either by checking 3 or 4 or yes
#' @export

grade_ipg <- function(x, type = "character") {
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!stringr::str_detect(x, "Not Observed|Not observed|NULL|NA|N/A|Not applicable|Did not observe")]
  # purrr::keep( ~ !is.null(.x)) %>%
  # purrr::keep( ~ !str_detect(.x, "Not Observed"))

  if (type == "character") {
    x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
      (sum(stringr::str_detect(x, "No"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
  } else if (type == "numeric") {
    x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
  } else if (type == "numeric_low") {
    x <- 100 * (sum(stringr::str_detect(x, "2|3"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "2|3"), na.rm = T) + sum(str_detect(x, "2|3"), na.rm = T))
  }

  x
}

#' @title Get the percent of a column that equals specific values
#' @description Automatically scaled stacked bar chart with TL theming
#' @param data the data for the plotter to use, should include all columns of interest
#' @param percent_equal string inputs to find the percent of the column that equals those values
#' @return a percentage as a string
#' @export

tl_select_percent <- function(data, percent_equal) {

  sum_correct <- data |>
    table() |>
    magrittr::extract(percent_equal) |>
    sum(na.rm = T)

  sum_table <- sum(!is.na(data))

  sum_correct / sum_table

}


#' @title Student percent agree/strongly agree
#' @description Automatically dodged bar chart for student data
#' @param data the data for the plotter to use, should include all columns of interest
#' @param title the title for the plot
#' @param string_remove NULL by default, provides an optional string removal
#' @param col_select the columns to select with `tidyselect::contains`
#' @param agree_select the type of agree/strongly agree to select, for example also often/always
#' @param legend_position c(0.8, 0.25) by default, adjustable
#' @return a ggplot object
#' @export

student_bar_chart <- function(data,
                              col_select,
                              agree_select,
                              string_remove,
                              title,
                              legend_position = c(0.8, 0.25)) {

  n_size_1 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Pre") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")
  n_size_2 <- format(sum(!is.na(data |> dplyr::filter(prepost == "Post") |> dplyr::select(tidyselect::contains(col_select)) |> dplyr::pull(1))), big.mark = ",")

  ### Makes race column, selects relevant columns and gets percent
  ### that selected relevant levels of agreeness
  student_data_summarised <- data |>
    dplyr::select(tidyselect::contains(col_select), prepost) |>
    dplyr::group_by(prepost) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ tlShiny::tl_select_percent(.x, agree_select))) |>
    tidyr::drop_na(prepost) |>
    (\(.) dplyr::mutate(., Overall = rowMeans(select(., -prepost))))()

  ### Reformat dataframe and prep for ggplot2
  student_data_percent <- student_data_summarised |>
    tidyr::pivot_longer(!prepost, names_to = "question", values_to = "percent") |>
    tidyr::drop_na(percent) |>
    dplyr::mutate(
      question = stringr::str_replace_all(question, c("crse_1" = "My teacher explains what we are learning in different ways",
                                                      "crse_2" = "My teacher wants students from different cultures to respect one another",
                                                      "crse_3" = "My teacher uses what I already know to help me understand new ideas",
                                                      "crse_4" = "My teacher uses examples from my culture when teaching",
                                                      "crse_5" = "My teacher asks about ways that students’ cultures may be different from others",
                                                      "crse_6" = "My teacher helps students learn about other students and their cultures",
                                                      "crse_7" = "My teacher asks about students’ home life",
                                                      "crse_8" = "My teacher treats all students like they are important members of the classroom",
                                                      "teacher_student_rel_1" = "My teacher makes me feel that he/she really cares about me",
                                                      "teacher_student_rel_2" = "I like the way my teacher treats me when I need help",
                                                      "teacher_student_rel_3" = "My teacher seems to know if something is bothering me",
                                                      "self_efficacy_1" = "I can do almost all the work in this class if I don’t give up",
                                                      "self_efficacy_2" = "Even when work is hard, I know I can learn it",
                                                      "self_efficacy_3" = "I'm certain I can master the skills taught in this class",
                                                      "self_efficacy_4" = "When doing work for this class, I focus on learning, not the time work takes",
                                                      "self_efficacy_5" = "I have been able to figure out the most difficult work in this class",
                                                      "happiness_belonging_1" = "This class is a happy place for me to be",
                                                      "happiness_belonging_2" = "In this class, I feel like I belong",
                                                      "happiness_belonging_3" = "Being in this class makes me feel sad or angry",
                                                      "happiness_belonging_4" = "The things we have done in class this year are interesting",
                                                      "happiness_belonging_5" = "Because of this teacher, I am learning to love this subject",
                                                      "happiness_belonging_6" = "I enjoy this subject this year",
                                                      "being_challenged_1" = "My teacher makes sure that I try to do my best",
                                                      "being_challenged_2" = "In this class, we learn a lot almost every day",
                                                      "being_challenged_3" = "In this class, we learn to correct our mistakes",
                                                      "being_challenged_4" = "In this class, my teacher accepts nothing less than our full effort",
                                                      "being_challenged_5" = "My teacher wants us to use our thinking skills, not just memorize things")),
      question = stringr::str_remove_all(question, string_remove),
      question = stringr::str_wrap(question, 25),
      percent = percent * 100
    )

  subtitle <- if (length(agree_select) > 1) {
    glue::glue('The following percentages show the % that selected "{agree_select[1]}" or "{agree_select[2]}"')
  } else {
    glue::glue('The following percentages show the % that selected "{agree_select[1]}"')
  }

  p <- ggplot2::ggplot(student_data_percent, aes(x = forcats::fct_relevel(question, "Overall", after = length(question)), y = percent, fill = prepost)) +
    ggplot2::geom_col(position = position_dodge()) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(percent), "%")
      ),
      size = 8,
      fontface = "bold",
      position = ggplot2::position_dodge2(width = 1, preserve = "total"),
      hjust = -0.25,
      family = "Calibri"
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("{title} <span style = 'color:#04abeb;'>(pre n = {n_size_1})</span> & (post n = {n_size_2})"),
      subtitle = subtitle,
      fill = "Race"
    ) +
    ggplot2::scale_fill_manual(values = c("Post" = "#040404", "Pre" = "#04ABEB")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = TRUE),
      color = "none"
    ) +
    ggplot2::coord_flip() +
    tlShiny::theme_tl(legend = F) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(family = "Calibri Bold", face = "bold"),
      plot.subtitle = ggtext::element_markdown(family = "Calibri", hjust = 0.25),
      legend.key.size = grid::unit(1.2, "cm"),
      axis.text.y = ggplot2::element_text(size = 18),
      axis.text.x = ggplot2::element_text(size = 13)
    )

  print(p)

}

#' @title End of session feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
session_feedback_graph <- function(data) {

  session_survey <- data |>
    tidyr::drop_na(coach_ongoing_feed_1)

  ### Get second facilitator responses ###
  second_fac <- session_survey |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_2_1,
      `Their facilitation or coaching is clear` = coach_ongoing_feed_2_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_2_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_2_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_2_5
    )


  ### Get first facilitator combined with second agree per question for end of session survey ###
  plot_agree <- session_survey |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they facilitated` = coach_ongoing_feed_1,
      `Their facilitation or coaching is clear` = coach_ongoing_feed_2,
      `They effectively built a safe learning environment` = coach_ongoing_feed_3,
      `They seemed fully prepared for the session` = coach_ongoing_feed_4,
      `They made adjustments based on participant needs` = coach_ongoing_feed_5
    ) |>
    dplyr::bind_rows(second_fac) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = stringr::str_wrap(Question, width = 50)) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )

  if (nrow(plot_agree) >= 1) {

    ### Calculate n size ###
    n_size_agree <- session_survey |>
      dplyr::count(sort = T)

    ### Make ggplot of session survey agree percent ###
    p <- plot_agree |>
      dplyr::group_by(Question, Response) |>
      dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
      dplyr::mutate(Question = factor(Question, levels = c("They demonstrated deep knowledge of the content\nthey facilitated",
                                                           "Their facilitation or coaching is clear",
                                                           "They seemed fully prepared for the session",
                                                           "They effectively built a safe learning environment",
                                                           "They made adjustments based on participant needs")),
                    Response = stringr::str_replace_all(Response, c("Neither agree nor disagree" = "Neither agree\nnor disagree",
                                                                    "Strongly agree" = "Strongly\nagree",
                                                                    "Strongly disagree" = "Strongly\ndisagree"))) |>
      ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
      ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        size = 6.5,
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold"
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404", "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A", "4 - Agree" = "#0182B4", "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white", "2 - Disagree" = "white",
        "3 - Neither agree\nnor disagree" = "black", "4 - Agree" = "black", "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::labs(
        fill = "", title = glue::glue("Participant Perceptions of Course Facilitation (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
        x = "", y = ""
      ) +
      ggplot2::coord_flip() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::scale_x_discrete(limits = rev) +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 19, margin = margin(t = 0, l = 0, r = -10, b = 0)),
        axis.text.x = ggplot2::element_text(size = 15),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold", hjust = 0.47),
        legend.text = ggplot2::element_text(size = 20),
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(0.85, "cm"),
        legend.position = "bottom"
      )

    print(p)

  } else {
    tlShiny:::no_data_plot_filters
  }
}

#' @title Course survey feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
course_feedback_graph <- function(data) {

  course_survey <- data |>
    tidyr::drop_na(coach_end_feed_1)

  ### Get first facilitator combined with second agree per question for end of course survey ###
  plot_agree <- course_survey |>
    dplyr::select(
      `I looked forward to attending this PL` = coach_end_feed_1,
      `I was fully present/"minds-on" during these PL sessions` = coach_end_feed_4,
      `The activities were well-designed to help me meet the learning targets` = coach_end_feed_15,
      `I am satisfied with how the sessions were facilitated` = coach_end_feed_5,
      `This PL was a good use of my time` = coach_end_feed_6,
      `I talk to other teachers about the things I learned in this PL` = coach_end_feed_7,
      `I felt a sense of community with the other participants in this course` = coach_end_feed_16,
      `The PL was relevant to my instructional practices` = coach_end_feed_8,
      `The strategies I’ve learned will improve my instruction` = coach_end_feed_9,
      `The strategies I’ve learned will improve my coaching or supervision of teachers` = coach_end_feed_10,
      `I have applied or will apply what I have learned to my practice` = coach_end_feed_11,
      `The PL has supported me in being responsive to students' backgrounds, cultures, and points of view.` = coach_end_feed_12,
      `I am satisfied with the overall quality of this PL` = coach_end_feed_13
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na() |>
    dplyr::group_by(Question, Response) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(Question = stringr::str_wrap(Question, width = 60)) |>
    dplyr::reframe(
      n = n,
      Response = Response,
      Percent = n / sum(n) * 100
    )

  if (nrow(plot_agree) >= 1) {

    ### Calculate n size ###
    n_size_agree <- course_survey |>
      dplyr::count(sort = T)

    ### Make ggplot of course survey agree percent ###
    p <- plot_agree |>
      dplyr::group_by(Question, Response) |>
      dplyr::summarise(Percent = weighted.mean(Percent, n)) |>
      dplyr::mutate(Question = factor(Question, levels = c("I looked forward to attending this PL",
                                                           "I was fully present/\"minds-on\" during these PL sessions",
                                                           "The activities were well-designed to help me meet the\nlearning targets",
                                                           "I am satisfied with how the sessions were facilitated",
                                                           "This PL was a good use of my time",
                                                           "I talk to other teachers about the things I learned in this\nPL",
                                                           "I felt a sense of community with the other participants in\nthis course",
                                                           "The PL was relevant to my instructional practices",
                                                           "The strategies I’ve learned will improve my instruction",
                                                           "The strategies I’ve learned will improve my coaching or\nsupervision of teachers",
                                                           "I have applied or will apply what I have learned to my\npractice",
                                                           "The PL has supported me in being responsive to students'\nbackgrounds, cultures, and points of view.",
                                                           "I am satisfied with the overall quality of this PL")),
                    Response = stringr::str_replace_all(Response, c("Neither agree nor disagree" = "Neither agree\nnor disagree",
                                                                    "Strongly agree" = "Strongly\nagree",
                                                                    "Strongly disagree" = "Strongly\ndisagree"))) |>
      ggplot2::ggplot(ggplot2::aes(x = Question, y = Percent, fill = factor(Response))) +
      ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        size = 6.5,
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold"
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404", "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A", "4 - Agree" = "#0182B4", "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white", "2 - Disagree" = "white",
        "3 - Neither agree\nnor disagree" = "black", "4 - Agree" = "black", "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::labs(
        fill = "", title = glue::glue("Participant Perceptions of Course (n = {format(sum(n_size_agree$n), big.mark = ',')})"),
        x = "", y = ""
      ) +
      ggplot2::coord_flip() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::scale_x_discrete(limits = rev) +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 19, margin = margin(t = 0, l = 0, r = -20, b = 0)),
        axis.text.x = ggplot2::element_text(size = 15),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold"),
        legend.text = ggplot2::element_text(size = 20),
        legend.margin = ggplot2::margin(t = 0, l = -30, r = 0, b = 0),
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(0.85, "cm"),
        legend.position = "bottom"
      )

    print(p)

  } else {
    tlShiny:::no_data_plot_filters
  }

}

#' @title Ongoing Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
ongoing_coaching_feedback_graph <- function(data) {
  ongoing_coaching <- data |>
    tidyr::drop_na(coach_ongoing_feed_1)

  coaching_plot_agree <- ongoing_coaching |>
    dplyr::select(
      `They demonstrated deep knowledge of the content they coach` = coach_ongoing_feed_1,
      `Their coaching is clear` = coach_ongoing_feed_2,
      `They seem fully prepared for the coaching sessions` = coach_ongoing_feed_3,
      `They effectively build a safe learning environment` = coach_ongoing_feed_4,
      `They make necessary adjustments based on my needs` = coach_ongoing_feed_5
    ) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = stringr::str_replace_all(Response, c("Neither agree nor disagree" = "Neither agree\nnor disagree",
                                                      "Strongly agree" = "Strongly\nagree",
                                                      "Strongly disagree" = "Strongly\ndisagree")),
      Response = factor(Response, levels = c(
        "1 - Strongly\ndisagree",
        "2 - Disagree",
        "3 - Neither agree\nnor disagree",
        "4 - Agree",
        "5 - Strongly\nagree"
      )),
      Question = stringr::str_wrap(Question, width = 60)
    )

  if (nrow(coaching_plot_agree) >= 1) {
    n_size_agree <- ongoing_coaching |>
      dplyr::count(sort = T)


    p <- coaching_plot_agree |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        Question = factor(Question, levels = c("They demonstrated deep knowledge of the content they coach",
                                               "Their coaching is clear",
                                               "They seem fully prepared for the coaching sessions",
                                               "They effectively build a safe learning environment",
                                               "They make necessary adjustments based on my needs"))
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x = Question,
        y = Percent,
        color = Response,
        fill = Response
      )) +
      ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        size = 6.5
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("Ongoing Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404",
        "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A",
        "4 - Agree" = "#0182B4",
        "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white",
        "2 - Disagree" = "black",
        "3 - Neither agree\nnor disagree" = "black",
        "4 - Agree" = "black",
        "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::scale_x_discrete(limits = rev) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 19,
                                            margin = margin(t = 0, l = 0, r = -80, b = 0)),
        axis.text.x = ggplot2::element_text(size = 15),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold"),
        legend.text = ggplot2::element_text(size = 20),
        legend.margin = ggplot2::margin(t = 0, l = -15, r = 0, b = 0),
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(0.85, "cm"),
        legend.position = "bottom"
      )

    print(p)

  } else {
    tlShiny:::no_data_plot_filters
  }

}

#' @title End of Coaching feedback graph dependent on race and content area
#' @description Returns a barchart for selections in the relevant questions of the end of coaching survey
#' @param data the data to use
#' @return prints a ggplot object
#' @export
end_coaching_feedback_graph <- function(data) {

  end_coaching <- data |>
    tidyr::drop_na(coach_end_feed_1)

  coaching_plot_agree <- end_coaching |>
    dplyr::select(
      `I looked forward to attending this PL` = coach_end_feed_1,
      `I was fully present/"minds-on" during these PL sessions` = coach_end_feed_4,
      `The activities were well-designed to help me meet the learning targets` = coach_end_feed_15,
      `I am satisfied with how the sessions were facilitated` = coach_end_feed_5,
      `This PL was a good use of my time` = coach_end_feed_6,
      `I talk to other teachers about the things I learned in this PL` = coach_end_feed_7,
      `I felt a sense of community with the other participants in this course` = coach_end_feed_16,
      `The PL was relevant to my instructional practices` = coach_end_feed_8,
      `The strategies I’ve learned will improve my instruction` = coach_end_feed_9,
      `The strategies I’ve learned will improve my coaching or supervision of teachers` = coach_end_feed_10,
      `I have applied or will apply what I have learned to my practice` = coach_end_feed_11,
      `The PL has supported me in being responsive to students' backgrounds, cultures, and points of view.` = coach_end_feed_12,
      `I am satisfied with the overall quality of this PL` = coach_end_feed_13
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(tidyr::everything(), names_to = "Question", values_to = "Response") |>
    tidyr::drop_na(Response) |>
    dplyr::group_by(Question, Response) |>
    dplyr::count(sort = T) |>
    dplyr::ungroup() |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Percent = round(100 * n / sum(n), 2),
      Response = stringr::str_replace_all(Response, c("Neither agree nor disagree" = "Neither agree\nnor disagree",
                                                      "Strongly agree" = "Strongly\nagree",
                                                      "Strongly disagree" = "Strongly\ndisagree")),
      Response = factor(Response, levels = c(
        "1 - Strongly\ndisagree",
        "2 - Disagree",
        "3 - Neither agree\nnor disagree",
        "4 - Agree",
        "5 - Strongly\nagree"
      )),
      Question = stringr::str_wrap(Question, width = 60)
    )

  if (nrow(coaching_plot_agree) >= 1) {
    n_size_agree <- end_coaching |>
      dplyr::count(sort = T)

    p <- coaching_plot_agree |>
      dplyr::mutate(
        Percent = round(100 * n / sum(n), 2),
        Question = factor(Question, levels = c("I looked forward to attending this PL",
                                               "I was fully present/\"minds-on\" during these PL sessions",
                                               "The activities were well-designed to help me meet the\nlearning targets",
                                               "I am satisfied with how the sessions were facilitated",
                                               "This PL was a good use of my time",
                                               "I talk to other teachers about the things I learned in this\nPL",
                                               "I felt a sense of community with the other participants in\nthis course",
                                               "The PL was relevant to my instructional practices",
                                               "The strategies I’ve learned will improve my instruction",
                                               "The strategies I’ve learned will improve my coaching or\nsupervision of teachers",
                                               "I have applied or will apply what I have learned to my\npractice",
                                               "The PL has supported me in being responsive to students'\nbackgrounds, cultures, and points of view.",
                                               "I am satisfied with the overall quality of this PL"))
      ) |>
      ggplot2::ggplot(ggplot2::aes(
        x = Question,
        y = Percent,
        color = Response,
        fill = Response
      )) +
      ggplot2::geom_col(color = NA, position = ggplot2::position_stack(reverse = TRUE)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(Percent >= 10, paste0(round(Percent), "%"), ""),
          color = Response
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        fontface = "bold",
        size = 6.5
      ) +
      ggplot2::labs(
        x = "", y = "",
        title = glue::glue("End of Coaching Participant Feedback (n = {sum(n_size_agree$n, na.rm = T)})"),
        fill = ""
      ) +
      ggplot2::scale_fill_manual(values = c(
        "1 - Strongly\ndisagree" = "#040404",
        "2 - Disagree" = "#032E3F",
        "3 - Neither agree\nnor disagree" = "#02587A",
        "4 - Agree" = "#0182B4",
        "5 - Strongly\nagree" = "#00ACF0"
      )) +
      ggplot2::scale_color_manual(values = c(
        "1 - Strongly\ndisagree" = "white",
        "2 - Disagree" = "black",
        "3 - Neither agree\nnor disagree" = "black",
        "4 - Agree" = "black",
        "5 - Strongly\nagree" = "black"
      )) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(),
        color = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        expand = c(0.14, 0)
      ) +
      ggplot2::scale_x_discrete(limits = rev) +
      ggplot2::coord_flip() +
      tlShiny::theme_tl(legend = TRUE) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 19, margin = margin(t = 0, l = 0, r = -80, b = 0)),
        axis.text.x = ggplot2::element_text(size = 15),
        plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold"),
        legend.text = ggplot2::element_text(size = 20),
        legend.margin = ggplot2::margin(t = 0, l = -15, r = 0, b = 0),
        legend.key.width = ggplot2::unit(1.5, "cm"),
        legend.key.height = ggplot2::unit(0.85, "cm"),
        legend.position = "bottom"
      )

    print(p)

  } else {
    tlShiny:::no_data_plot_filters
  }
}
