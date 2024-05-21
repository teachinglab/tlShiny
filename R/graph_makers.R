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
                  title = paste0("<b>", title, "<br>% Correct <span style='color:#d17df7'>before (n = ", n1, ")</span> and <span style='color:#55bbc7'>after (n = ", n2, ")</span></b>")
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
                  question = stringr::str_wrap(question, width = 55)) |>
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
                       vjust = -0.1,
                       fontface = "bold",
                       family = "Calibri",
                       size = 10) +
    ggplot2::facet_wrap( ~ factor(question, levels = c("Total", unique(plot_data$question)[!plot_data$question %in% "Total"]))) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(x = "", y = "",
                  title = paste0("<b>", title, " % Correct <span style='color:#d17df7'>Before</span> and <span style='color:#55bbc7'>After</span> Per Question</b>")
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

#' @title ELA IPG Chart Summary Maker
#' @description Creates a chart to summarise the ELA IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export

make_ipg_ela_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  k_12_ela <- ipg_adjust |>
    dplyr::select(
      k12_ela_ca1a, k12_ela_ca1b, k12_ela_ca1c, # Core Action 1, Yes/No
      k12_ela_ca2a, k12_ela_ca2b, k12_ela_ca2c, k12_ela_ca2d, # Core Action 2, 1-4
      k12_ela_ca3a, k12_ela_ca3b, k12_ela_ca3c, k12_ela_ca3d, k12_ela_ca3e, k12_ela_ca3f
    ) |> # Core Action 3, 1-4
    dplyr::mutate(
      dplyr::across(everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1c, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3e, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3f, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca1_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_ela_ca1c, type = "character")
        ),
        na.rm = TRUE
      ),
      ca2_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca2d, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca3_score = mean(
        c(
          tlShiny::grade_ipg(k12_ela_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3e, type = "numeric"),
          tlShiny::grade_ipg(k12_ela_ca3f, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ca1_n = sum(!is.na(ca1_score)),
      ca1_score = mean(ca1_score, na.rm = TRUE),
      ca2_n = sum(!is.na(ca2_score)),
      ca2_score = mean(ca2_score, na.rm = TRUE),
      ca3_n = sum(!is.na(ca3_score)),
      ca3_score = mean(ca3_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = dplyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = case_when(
        `Core Action` == "ca1_score" ~ ca1_n,
        `Core Action` == "ca2_score" ~ ca2_n,
        `Core Action` == "ca3_score" ~ ca3_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Core Action 1",
        "Core Action 2",
        "Core Action 3"
      ))
    )

  unique_color_n <- length(unique(k_12_ela$`Core Action`))

  ela_plot <- k_12_ela |>
    ggplot2::ggplot(aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
              fontface = "bold",
              vjust = -0.25,
              size = 7
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 105),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators Baseline K-12 ELA IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators Mid-year K-12 ELA IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators End of Year K-12 ELA IPG",
                               round == "Ongoing" ~ "% Positive Indicators Ongoing K-12 ELA IPG",
                               round == "Other" ~ "% Positive Indicators Other K-12 ELA IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 16),
      axis.text.y = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold"),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  ela_plot
}


#' @title FSOT IPG Chart Summary Maker
#' @description Creates a chart to summarise the FSOT IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export

make_ipg_fsot_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  fsot <- ipg_adjust |>
    dplyr::select(
      fsot_ac1, fsot_ac2, # AC1/AC2, 1-4, Q69 is not 1-4
      fsot_td1, fsot_td2, fsot_td3, fsot_td4, # TD, 1-4
      fsot_sp1, fsot_sp2, fsot_sp3, fsot_sp4, # SP, 1-4
      fsot_ad1, fsot_ad2
    ) |> # AD1/AD2, 1-3
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ac1, type = "numeric"),
          tlShiny::grade_ipg(fsot_ac2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td1, type = "numeric"),
          tlShiny::grade_ipg(fsot_td2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td3, type = "numeric"),
          tlShiny::grade_ipg(fsot_td4, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp1, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp2, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp3, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp4, type = "numeric"),
          tlShiny::grade_ipg(fsot_ad1, type = "numeric_low"),
          tlShiny::grade_ipg(fsot_ad2, type = "numeric_low")
        ),
        na.rm = TRUE
      ),
      ac_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ac1, type = "numeric"),
          tlShiny::grade_ipg(fsot_ac2, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ad_score = mean(
        c(
          tlShiny::grade_ipg(fsot_ad1, type = "numeric_low"),
          tlShiny::grade_ipg(fsot_ad2, type = "numeric_low")
        ),
        na.rm = TRUE
      ),
      sp_score = mean(
        c(
          tlShiny::grade_ipg(fsot_sp1, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp2, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp3, type = "numeric"),
          tlShiny::grade_ipg(fsot_sp4, type = "numeric")
        ),
        na.rm = TRUE
      ),
      td_score = mean(
        c(
          tlShiny::grade_ipg(fsot_td1, type = "numeric"),
          tlShiny::grade_ipg(fsot_td2, type = "numeric"),
          tlShiny::grade_ipg(fsot_td3, type = "numeric"),
          tlShiny::grade_ipg(fsot_td4, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ac_n = sum(!is.na(ac_score)),
      ac_score = mean(ac_score, na.rm = TRUE),
      ad_n = sum(!is.na(ad_score)),
      ad_score = mean(ad_score, na.rm = TRUE),
      sp_n = sum(!is.na(sp_score)),
      sp_score = mean(sp_score, na.rm = TRUE),
      td_n = sum(!is.na(td_score)),
      td_score = mean(td_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = tidyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = dplyr::case_when(
        `Core Action` == "ac_score" ~ ac_n,
        `Core Action` == "ad_score" ~ ad_n,
        `Core Action` == "sp_score" ~ sp_n,
        `Core Action` == "td_score" ~ td_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ac_score" = "Aligned\nContent",
        "ad_score" = "Assessment\n& Differentiation",
        "sp_score" = "Student\nPractice",
        "td_score" = "Teacher-Directed\nInstruction"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Aligned\nContent",
        "Assessment\n& Differentiation",
        "Student\nPractice",
        "Teacher-Directed\nInstruction"
      ))
    )

  fsot_plot <- fsot |>
    ggplot2::ggplot(ggplot2::aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
                       fontface = "bold",
                       vjust = -0.25,
                       size = 7
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = length(unique(fsot$`Core Action`)), base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 100),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators Baseline FSOT IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators Mid-year FSOT IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators End of Year FSOT IPG",
                               round == "Ongoing" ~ "% Positive Indicators Ongoing FSOT IPG",
                               round == "Other" ~ "% Positive Indicators Other FSOT IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 17),
      axis.text.y = ggplot2::element_text(size = 17),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold"),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  fsot_plot
}

#' @title Math IPG Chart Summary Maker
#' @description Creates a chart to summarise the Math IPG based on the round selected
#' @param data the data
#' @param round the round of the IPG for which data should be pulled (can be one of "Baseline (first observation of the year)", "Mid-year (middle of service, if applicable)", "End of year (last observation of the year)", "Other", or "Ongoing")
#' @return a ggplot
#' @export


make_ipg_math_summary_chart <- function(round = "Baseline (first observation of the year)") {

  base_color <- "#040404"
  end_color <- "#04abeb"

  ipg_adjust <- data |>
    dplyr::filter(direct_to_ts_obs == round)

  k_12_math <- ipg_adjust |>
    dplyr::select(
      k12_m_ca1a, k12_m_ca1b, k12_m_ca1c, # Core Action 1 yes-no
      k12_m_ca2a, k12_m_ca2b, k12_m_ca2c, k12_m_ca2d, # Core Action 2 1-4
      k12_m_ca3a, k12_m_ca3b, k12_m_ca3c, k12_m_ca3d, k12_m_ca3e
    ) |> # Core Action 3 1-4
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ as.character(.x))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      overall_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1c, type = "character"),
          tlShiny::grade_ipg(k12_m_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3e, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca1_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca1a, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1b, type = "character"),
          tlShiny::grade_ipg(k12_m_ca1c, type = "character")
        ),
        na.rm = TRUE
      ),
      ca2_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca2a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca2d, type = "numeric")
        ),
        na.rm = TRUE
      ),
      ca3_score = mean(
        c(
          tlShiny::grade_ipg(k12_m_ca3a, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3b, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3c, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3d, type = "numeric"),
          tlShiny::grade_ipg(k12_m_ca3e, type = "numeric")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na(overall_score) |>
    dplyr::summarise(
      ca1_n = sum(!is.na(ca1_score)),
      ca1_score = mean(ca1_score, na.rm = TRUE),
      ca2_n = sum(!is.na(ca2_score)),
      ca2_score = mean(ca2_score, na.rm = TRUE),
      ca3_n = sum(!is.na(ca3_score)),
      ca3_score = mean(ca3_score, na.rm = TRUE),
      n = sum(!is.na(overall_score)),
      overall_score = mean(overall_score, na.rm = TRUE)
    ) |>
    tidyr::pivot_longer(cols = tidyr::ends_with("score"), names_to = "Core Action", values_to = "Score") |>
    dplyr::mutate(
      n = case_when(
        `Core Action` == "ca1_score" ~ ca1_n,
        `Core Action` == "ca2_score" ~ ca2_n,
        `Core Action` == "ca3_score" ~ ca3_n,
        `Core Action` == "overall_score" ~ n
      ),
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3"
      ))
    ) |>
    dplyr::select(-dplyr::ends_with("_n")) |>
    dplyr::mutate(
      `Core Action` = factor(`Core Action`, c(
        "Overall",
        "Core Action 1",
        "Core Action 2",
        "Core Action 3"
      ))
    )

  unique_color_n <- length(unique(k_12_math$`Core Action`))

  math_plot <- k_12_math |>
    ggplot2::ggplot(aes(x = `Core Action`, y = Score, fill = `Core Action`)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Score), "% (n = ", n, ")")),
              fontface = "bold",
              vjust = -0.25,
              size = 7
    ) +
    ggplot2::scale_fill_manual(values = tlShiny::tl_palette2(n = unique_color_n, base_color_start = base_color, end_color_start = end_color)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 105),
      expand = c(0, 12),
      breaks = seq(10, 100, 10)
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = dplyr::case_when(round == "Baseline (first observation of the year)" ~ "% Positive Indicators Baseline K-12 Math IPG",
                               round == "Mid-year (middle of service, if applicable)" ~ "% Positive Indicators Mid-year K-12 Math IPG",
                               round == "End of year (last observation of the year)" ~ "% Positive Indicators End of Year K-12 Math IPG",
                               round == "Ongoing" ~ "% Positive Indicators Ongoing K-12 Math IPG",
                               round == "Other" ~ "% Positive Indicators Other K-12 Math IPG"),
      caption = "Note that n sizes represent the number of overall scores per grouping, the actual number of responses that qualify for scores may vary"
    ) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(size = 16),
      axis.text.y = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold"),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  math_plot
}
