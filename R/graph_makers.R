#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @param summary_path optional path to save plot to a file, if NULL does not save anywhere
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess, summary_path = "report_summary_images") {

  plot_data <- data %>%
    {
      if (know_assess != "All Knowledge Assessments") dplyr::filter(know_assess == !!rlang::enquo(know_assess)) else .
    } %>%
    # dplyr::filter(know_assess == !!rlang::enquo(know_assess)) |>
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

  n1 <- data |>
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "pre") |>
    # dplyr::pull(id) |>
    nrow()

  n2 <- data |>
    dplyr::filter(know_assess == !!rlang::enquo(know_assess) & prepost == "post") |>
    # dplyr::pull(id) |>
    nrow()

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
                       family = "Calibri") +
    ggplot2::facet_wrap( ~ know_assess) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    # ggtext::geom_richtext(data = data.frame(name = "Before", value = 100,
    #                                         label = "% Correct <b style='color:#d17df7'>before</b> and <b style='color:#55bbc7'>after</b>."),
    #                       aes(x = name, y = value, label = label)) +
    ggplot2::labs(x = "", y = "",
                  # title = paste0(title, "\n% Correct before and after")#,
                  title = paste0(title, "<br>% Correct <b style='color:#d17df7'>before (n = ", n1, ")</b> and <b style='color:#55bbc7'>after (n = ", n2, ")</b>")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    # TeachingLab::theme_tl(markdown = F) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # plot.title = ggplot2::element_text(),
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold"))

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
