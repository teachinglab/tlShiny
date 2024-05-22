#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess_filter the knowledge assessment to make plot for
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess_filter) {

  plot_data <- data |>
    dplyr::filter(question1 == "Score" & know_assess == know_assess_filter) |>
    dplyr::mutate(percent = 100 * score / max_score) |>
    dplyr::group_by(prepost, know_assess) |>
    dplyr::summarise(
      percent = round(mean(percent, na.rm = T), 2),
      n = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      prepost = ifelse(prepost == "pre",
                       "Before",
                       "After"
      ),
      prepost = factor(prepost, levels = c("Before", "After")),
      know_assess = paste0(know_assess, " % Correct")
    ) |>
    print()

  n1 <- plot_data$n[plot_data$prepost == "Before"]
  if (length(plot_data$n[plot_data$prepost == "After"]) != 0) {
    n2 <- plot_data$n[plot_data$prepost == "After"]
  } else {
    n2 <- 0
  }

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = prepost, y = percent, fill = prepost)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(percent), "%")),
                       vjust = -1,
                       color = "black",
                       fontface = "bold",
                       family = "Calibri Bold",
                       size = 10) +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    ggplot2::labs(x = "", y = "",
                  title = paste0("<b>", plot_data$know_assess[1], "<br>% Correct <span style='color:#d17df7'>before (n = ", n1, ")</span> and <span style='color:#55bbc7'>after (n = ", n2, ")</span></b>")
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    tlShiny::theme_tl() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5, size = 25, family = "Calibri Bold"),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold", size = 18, family = "Calibri"),
      axis.text.y = ggplot2::element_text(face = "bold", size = 18, family = "Calibri"))

  return(p)

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
              color = "black",
              family = "Calibri Bold",
              vjust = -0.4,
              size = 9
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
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold", size = 30),
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
                       color = "black",
                       family = "Calibri Bold",
                       vjust = -0.4,
                       size = 9
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
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold", size = 30),
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


make_ipg_math_summary_chart <- function(data, round = "Baseline (first observation of the year)") {

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
              vjust = -0.4,
              color = "black",
              family = "Calibri Bold",
              size = 9
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
      axis.text.x = ggplot2::element_text(size = 24),
      axis.text.y = ggplot2::element_text(size = 24),
      plot.title = ggplot2::element_text(face = "bold", family = "Calibri Bold", size = 30),
      plot.caption = ggplot2::element_text(size = 12, face = "italic")
    )

  math_plot
}

#' @title Teacher Curriculum Usage Graph
#' @description Creates a chart to summarise the Teacher Curriculum Usage from the Educator Survey
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_curriculum_usage <- function(data) {

  curriculum_usage <- data |>
    tidytable::select(prepost, tidytable::contains("materials")) |>
    tidytable::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
    tidytable::drop_na(value) |>
    tidytable::mutate(name = stringr::str_replace_all(name, c(
      "materials_1" = "curriculum materials adopted by your district",
      "materials_2" = "materials developed by your school or district",
      "materials_3" = "materials you found on the internet",
      "materials_4" = "materials developed by yourself or with colleagues"
    ))) |>
    tidytable::group_by(name, value, prepost) |>
    tidytable::count(sort = T) |>
    tidytable::ungroup() |>
    tidytable::drop_na(value) |>
    tidytable::mutate(
      name = stringr::str_wrap(name, 25),
      value = stringr::str_wrap(value, 20)
    ) |>
    tidytable::group_by(name, prepost) |>
    tidytable::mutate(
      Percent = round(100 * n / sum(n), 2),
      value = factor(value, levels = c(
        "Never use",
        "Sometimes (once a\nmonth)",
        "Use often (once or\ntwice weekly)",
        "Use everyday"
      ))
    ) |>
    tidytable::filter(value %in% c("Use often (once or\ntwice weekly)", "Use everyday")) |>
    tidytable::summarise(
      Percent = sum(Percent),
      n = sum(n)
    )

  n_size_1 <- sum(!is.na(data$materials_1[data$prepost == "Pre"]))
  n_size_2 <- sum(!is.na(data$materials_1[data$prepost == "Post"]))

  curriculum_usage |>
    ggplot2::ggplot(aes(
    x = forcats::fct_reorder(name, Percent, .desc = T),
    y = Percent
  )) +
    ggplot2::geom_col(ggplot2::aes(fill = prepost, group = prepost),
             color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        color = prepost,
        label = tidytable::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
      ),
      position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
      hjust = -0.25,
      fontface = "bold",
      size = 6
    ) +
    ggplot2::labs(
      x = "Please indicate the extent to which you use...", y = "",
      title = glue::glue("Teacher Curriculum Usage\n% selected use everyday or use often"),
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = c(
      "#00ACF0", "black"
    ), labels = c(glue::glue("Pre (n = {n_size_1})"), glue::glue("Post (n = {n_size_2})"))) +
    ggplot2::scale_color_manual(values = c(
      "#00ACF0", "black"
    )) +
    ggplot2::guides(
      fill = guide_legend()
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = c(0.14, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(color = "none") +
    tlShiny::theme_tl(legend = FALSE) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        margin = margin(t = 0, l = 0, r = -110, b = 0),
        size = 19
      ),
      axis.title.y = ggplot2::element_text(size = 19),
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold"),
      legend.position = "bottom",
      legend.key.height = ggplot2::unit(1.3, "cm"),
      legend.key.width = ggplot2::unit(1.3, "cm"),
      legend.key.size = ggplot2::unit(0.75, "cm"),
      legend.text = ggplot2::element_text(size = 18),
      legend.margin = ggplot2::margin(-25, 0, 0, -150)
    )

}

#' @title Teacher Use of Lessons Graph
#' @description Creates a chart to summarise the teacher use of lessons from district or school-adopted materials
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_lesson_usage <- function(data) {

  lesson_usage <- data |>
    tidytable::select(prepost, lesson_modifications) |>
    tidytable::mutate(lesson_modifications = as.character(lesson_modifications)) |>
    tidytable::drop_na(lesson_modifications) |>
    tidytable::group_by(lesson_modifications, prepost) |>
    tidytable::count(sort = T) |>
    tidytable::ungroup() |>
    tidytable::group_by(prepost) |>
    tidytable::mutate(
      lesson_modifications = stringr::str_wrap(lesson_modifications, 20),
      Percent = round(100 * n / sum(n), 2),
      lesson_modifications = factor(lesson_modifications, levels = c(
        "with no or few\nmodifications",
        "with modifications\nto less than half of\na lesson plan",
        "with modifications\nto more than half of\na lesson plan",
        "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans"
      ))
    )

  n_size_1 <- sum(!is.na(data$lesson_modifications[data$prepost == "Pre"]))
  n_size_2 <- sum(!is.na(data$lesson_modifications[data$prepost == "Post"]))

  lesson_usage |>
    ggplot2::ggplot(ggplot2::aes(
      x = prepost,
      y = Percent,
      fill = lesson_modifications
    )) +
    ggplot2::geom_col(ggplot2::aes(group = lesson_modifications), color = NA, position = ggplot2::position_stack(), width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(
        color = lesson_modifications,
        label = tidytable::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      fontface = "bold",
      family = "Calibri Bold",
      size = 6
    ) +
    ggplot2::labs(
      y = "I typically use lessons with...", x = "",
      title = glue::glue("Teachers' use of lessons from district or\nschool-adopted materials (pre n = {format(n_size_1, big.mark = ',')}, post n = {format(n_size_2, big.mark = ',')})"),
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = c(
      "with no or few\nmodifications" = "#032E3F",
      "with modifications\nto less than half of\na lesson plan" = "#02587A",
      "with modifications\nto more than half of\na lesson plan" = "#0182B4",
      "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "gray30"
    )) +
    ggplot2::scale_color_manual(values = c(
      "with no or few\nmodifications" = "white",
      "with modifications\nto less than half of\na lesson plan" = "white",
      "with modifications\nto more than half of\na lesson plan" = "black",
      "my main materials do\nnot include lesson\nplans or I typically\ncreate my own lesson\nplans" = "white"
    )) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = TRUE),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = c(0.14, 0)
    ) +
    ggplot2::coord_flip() +
    tlShiny::theme_tl(legend = TRUE) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 20.5,
        margin = ggplot2::margin(t = 0, l = 0, r = -115, b = 0)
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 19, face = "bold", family = "Calibri Bold"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 19.5, family = "Calibri Bold"),
      legend.key.width = ggplot2::unit(3, "cm"),
      legend.key.height = ggplot2::unit(1, "cm"),
      plot.title = ggplot2::element_text(size = 26.5, face = "bold", family = "Calibri Bold"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

}


#' @title Teacher Curriculum Perceptions Graph
#' @description Creates a chart to summarise the teacher perceptions of curriculum
#' @param data the data
#' @return a ggplot
#' @export

make_teacher_curriculum_perceptions <- function(data) {

  curriculum_perc <- data |>
    tidytable::select(prepost, tidytable::contains("curriculum_sch_dist")) |>
    tidytable::pivot_longer(!prepost, names_to = "name", values_to = "value") |>
    tidytable::mutate(name = stringr::str_replace_all(name, c(
      "curriculum_sch_dist_1" = "The curriculum materials adopted by my school or district are well-suited to the needs of my students",
      "curriculum_sch_dist_2" = "The  curriculum materials adopted by my school or district offer students high-quality opportunities to learn.",
      "curriculum_sch_dist_3" = "The  curriculum materials adopted by my school or district are well-organized and easy to use.",
      "curriculum_sch_dist_4" = "I like the  curriculum materials adopted by my school or district.",
      "curriculum_sch_dist_5" = "The  curriculum materials adopted by my school or district will help my students learn.",
      "curriculum_sch_dist_6" = "The  curriculum materials adopted by my school are too scripted and don't provide me with enough autonomy."
    ))) |>
    tidytable::group_by(name, value, prepost) |>
    tidytable::count(sort = T) |>
    tidytable::ungroup() |>
    tidytable::drop_na(value) |>
    tidytable::mutate(name = stringr::str_wrap(name, 25)) |>
    tidytable::group_by(name, prepost) |>
    tidytable::mutate(
      Percent = round(100 * n / sum(n), 2),
      value = factor(value, levels = c(
        "Strongly disagree",
        "Disagree",
        "Neither agree nor disagree",
        "Agree",
        "Strongly agree"
      ))
    ) |>
    tidytable::filter(value %in% c("Agree", "Strongly agree")) |>
    tidytable::ungroup() |>
    tidytable::group_by(name, prepost) |>
    tidytable::summarise(
      Percent = sum(Percent),
      n = sum(n)
    ) |>
    tidytable::ungroup() |>
    tidytable::mutate(prepost = factor(prepost, levels = c("Pre", "Post")))

  n_size_1 <- sum(!is.na(data$curriculum_sch_dist_1[data$prepost == "Pre"]))
  n_size_2 <- sum(!is.na(data$curriculum_sch_dist_1[data$prepost == "Post"]))

  curriculum_perc |>
    ggplot2::ggplot(ggplot2::aes(
      x = forcats::fct_reorder(name, Percent, .desc = T),
      y = Percent
    )) +
    ggplot2::geom_col(ggplot2::aes(fill = prepost),
             color = NA, position = ggplot2::position_dodge2(width = 1, reverse = TRUE)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        color = prepost,
        label = tidytable::if_else(Percent >= 10, paste0(round(Percent), "%"), "")
      ),
      position = ggplot2::position_dodge2(reverse = TRUE, width = 1),
      hjust = -0.25,
      fontface = "bold",
      family = "Calibri Bold",
      size = 8
    ) +
    ggplot2::labs(
      x = "", y = "",
      title = glue::glue("Teacher perceptions of curriculum % that agree or strongly agree"),
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Pre" = "#00ACF0",
      "Post" = "black"
    ), labels = c(glue::glue("Pre (n = {format(n_size_1, big.mark = ',')})"), glue::glue("Post (n = {n_size_2})"))) +
    ggplot2::scale_color_manual(values = c(
      "Pre" = "#00ACF0",
      "Post" = "black"
    )) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      color = "none"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1),
      expand = c(0.14, 0)
    ) +
    ggplot2::coord_flip() +
    tlShiny::theme_tl(legend = T) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(t = 0, l = 0, r = -100, b = 0),
        size = 19
      ),
      axis.text.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 25, face = "bold", family = "Calibri Bold"),
      legend.position = "bottom",
      legend.key.height = ggplot2::unit(1.3, "cm"),
      legend.key.width = ggplot2::unit(1.3, "cm"),
      legend.key.size = ggplot2::unit(0.75, "cm"),
      legend.text = ggplot2::element_text(size = 22, family = "Calibri Bold"),
      legend.margin = ggplot2::margin(-25, 0, 0, -80)
    )

}
