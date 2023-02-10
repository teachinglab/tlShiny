#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @param summary_path optional path to save plot to a file, if NULL does not save anywhere
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess, summary_path = "report_summary_images") {

  plot_data <- data |>
    dplyr::select(-site) |> # Get rid of site for when there is more than one
    dplyr::group_by(prepost) |>
    dplyr::summarise(percent = mean(percent, na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(name = ifelse(prepost == "pre",
                                "Before",
                                "After"),
                  value = percent,
                  name = factor(name, levels = c("Before", "After"))) |>
    dplyr::select(name, value)

  ### Make sure no over 100's ###
  plot_data$value <- ifelse(plot_data$value >= 100, 100, plot_data$value)

  title <- stringr::str_to_title(stringr::str_replace_all(know_assess, "_", " ")) |>
    stringr::str_replace_all("Ela", "ELA") |>
    stringr::str_replace_all("Ana", "ANA") |>
    stringr::str_replace_all("Eic", "EIC") # Correct title casing

  if (know_assess != "All Knowledge Assessments") {
    n1 <- data |>
      dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "pre") |>
      # dplyr::pull(id) |>
      nrow()

    n2 <- data |>
      dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "post") |>
      # dplyr::pull(id) |>
      nrow()
  } else {
    n1 <- data |>
      dplyr::filter(prepost == "pre") |>
      nrow()

    n2 <- data |>
      dplyr::filter(prepost == "post") |>
      nrow()
  }

  if (length(n1) == 0) {
    n1 <- 0
  }

  if (length(n2) == 0) {
    n2 <- 0
  }

  p <- plot_data |>
    dplyr::mutate(value = 100 * value) |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
                       vjust = -1,
                       fontface = "bold",
                       family = "Calibri",
                       size = 10) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(x = "", y = "",
                  title = paste0(title, "<br>% Correct <b style='color:#d17df7'>before (n = ", n1, ")</b> and <b style='color:#55bbc7'>after (n = ", n2, ")</b>")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(face = "bold", size = 12))

  if (!is.null(summary_path)) {
    ggplot2::ggsave(plot = p,
                    filename = glue::glue("{know_assess}.png"),
                    path = here::here(glue::glue("images/{summary_path}")),
                    bg = "white",
                    device = "png",
                    height = 5, width = 5)
  } else {
    return(p)
  }

}


#' @title Knowledge Assessment Graph Question-level Summary
#' @description Creates a graph specifically for Knowledge Assessments Scored by Question from Qualtrics
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @return a ggplot
#' @export
know_assess_summary_detailed <- function(data, know_assess) {

  plot_data <- data |>
    dplyr::select(-site) |> # Get rid of site for when there is more than one
    dplyr::group_by(prepost, question) |>
    dplyr::summarise(score = mean(score, na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(name = ifelse(prepost == "pre",
                                "Before",
                                "After"),
                  value = score,
                  name = factor(name, levels = c("Before", "After")),
                  question = stringr::str_wrap(question, width = 40)) |>
    dplyr::select(name, value, question)

  ### Make sure no over 100's ###
  plot_data$value <- ifelse(plot_data$value >= 100, 100, plot_data$value)

  title <- stringr::str_to_title(stringr::str_replace_all(know_assess, "_", " ")) |>
    stringr::str_replace_all("Ela", "ELA") |>
    stringr::str_replace_all("Ana", "ANA") |>
    stringr::str_replace_all("Eic", "EIC") # Correct title casing


  p <- plot_data |>
    dplyr::mutate(value = 100 * value) |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
                       vjust = -0.5,
                       fontface = "bold",
                       family = "Calibri",
                       size = 10) +
    ggplot2::facet_wrap( ~ question) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(x = "", y = "",
                  title = paste0("<b>", title, " % Correct <b style='color:#d17df7'>Before</b> and <b style='color:#55bbc7'>After</b> Per Question</b>")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5, size = 14),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(face = "bold", size = 12))

  return(p)

}
