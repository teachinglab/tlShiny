
#' @title IPG Character Columns
#' @description Character column names from the Instructional Practice Guide (IPG) rubric.
#' These are typically binary values like "Yes"/"No" indicating presence of a practice.
#' @export
ipg_character_cols <- c(
  "k12_m_ca1a", "k12_m_ca1b", "k12_m_ca1c",
  "k12_ela_ca1a", "k12_ela_ca1b", "k12_ela_ca1c"
)

#' @title IPG Numeric Columns
#' @description Numeric column names from the IPG rubric, scored from 1 to 4.
#' Higher scores indicate stronger alignment with the rubric.
#' @export
ipg_numeric_cols <- c(
  "k12_m_ca2a", "k12_m_ca2b", "k12_m_ca2c", "k12_m_ca2d",
  "k12_m_ca3a", "k12_m_ca3b", "k12_m_ca3c", "k12_m_ca3d", "k12_m_ca3e",
  "k12_ela_ca2a", "k12_ela_ca2b", "k12_ela_ca2c", "k12_ela_ca2d",
  "k12_ela_ca3a", "k12_ela_ca3b", "k12_ela_ca3c", "k12_ela_ca3d",
  "k12_ela_ca3e", "k12_ela_ca3f", "fsot_ac1", "fsot_ac2", "fsot_td1",
  "fsot_td2", "fsot_td3", "fsot_td4", "fsot_sp1", "fsot_sp2",
  "fsot_sp3", "fsot_sp4", "ss_ca1a", "ss_ca1b", "ss_ca1c",
  "sci_ca1a", "sci_ca1b", "sci_ca1c", "ss_ca2a", "ss_ca2b",
  "ss_ca2c", "ss_ca2d", "sci_ca2a", "sci_ca2b", "sci_ca2c",
  "sci_ca2d", "ss_ca3a", "ss_ca3b", "ss_ca3c", "ss_ca3d",
  "sci_ca3a", "sci_ca3b", "sci_ca3c", "sci_ca3d"
)

#' @title IPG Numeric Low Columns
#' @description Numeric columns from the IPG rubric with a lower scale where 2 and 3 are considered "high."
#' @export
ipg_numeric_low_cols <- c("fsot_ad1", "fsot_ad2")

#' @title Core Action Column Groups
#' @description Convenience vectors grouping IPG rubric items by Core Action (CA1, CA2, CA3) for scoring.
#' These include both character and numeric columns.
#' @export
ca1_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca1")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca1")])
#' @export
ca2_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca2")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca2")])
#' @export
ca3_cols <- c(ipg_numeric_cols[stringr::str_detect(ipg_numeric_cols, "ca3")], ipg_character_cols[stringr::str_detect(ipg_character_cols, "ca3")])

#' @title FSOT Domain Columns
#' @description Column groups used in FSOT scoring for Aligned Content (AC), Assessment & Differentiation (AD), Student Practice (SP), and Teacher-Directed Instruction (TD).
#' @export
ac_cols <- c("fsot_ac1", "fsot_ac2")
#' @export
ad_cols <- c("fsot_ad1", "fsot_ad2")
#' @export
td_cols <- c("fsot_td1", "fsot_td2", "fsot_td3", "fsot_td4")
#' @export
sp_cols <- c("fsot_sp1", "fsot_sp2", "fsot_sp3", "fsot_sp4")

#' @title Grade IPG Rubric Item
#' @description Converts a character-based rubric score into a logical value indicating proficiency.
#' Supports three types: `"character"` (Yes/No), `"numeric"` (1–4 scale), and `"numeric_low"` (1–3 scale).
#' @param x A character vector of rubric values.
#' @param type One of `"character"`, `"numeric"`, or `"numeric_low"` determining the rubric logic.
#' @return A logical vector with TRUE (proficient), FALSE (not proficient), or NA.
#' @export
new_grade_ipg <- function(x, type) {

  if (type == "character") {
    x <- dplyr::case_when(
      grepl("Yes", x) ~ TRUE,
      grepl("No", x) ~ FALSE,
      TRUE ~ NA
    )
  } else if (type == "numeric") {
    x <- dplyr::case_when(
      grepl("3|4", x) ~ TRUE,
      grepl("1|2", x) ~ FALSE,
      TRUE ~ NA
    )
  } else if (type == "numeric_low") {
    x <- dplyr::case_when(
      grepl("2|3", x) ~ TRUE,
      grepl("1", x) ~ FALSE,
      TRUE ~ NA
    )
  }

  return(x)
}


#' @title Summarize IPG Scores Per Teacher
#' @description Converts raw IPG rubric responses to logical scores and computes summary scores by core action and domain.
#' @param df A data frame with raw IPG rubric columns and metadata (e.g., `direct_to_ts_obs`, `ipg_rubric`).
#' @return A data frame of teacher-level scores, one row per observation.
#'
#' @examples
#' ipg_summarise_teacher(ipg_data)
#' @export
ipg_summarise_teacher <- function(df) {

  ipg_scored <- df |>
    collapse::fsubset(!is.na(direct_to_ts_obs) & direct_to_ts_obs != "Ongoing") |>
    collapse::fselect("direct_to_ts_obs", "ipg_rubric", c(character_cols, numeric_cols, numeric_low_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(character_cols), ~ new_grade_ipg(.x, type = "character"))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(numeric_cols)), ~ new_grade_ipg(.x, type = "numeric"))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(c(numeric_low_cols)), ~ new_grade_ipg(.x, type = "numeric_low"))) |>
    dplyr::mutate(
      ca1_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca1_cols)), na.rm = TRUE),
      ca2_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca2_cols)), na.rm = TRUE),
      ca3_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ca3_cols)), na.rm = TRUE),
      ac_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(ac_cols)), na.rm = TRUE),
      ad_score = 100 *  rowMeans(dplyr::pick(dplyr::all_of(ad_cols)), na.rm = TRUE),
      sp_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(sp_cols)), na.rm = TRUE),
      td_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(td_cols)), na.rm = TRUE),
      overall_score = 100 * rowMeans(dplyr::pick(dplyr::all_of(c(character_cols, numeric_cols, numeric_low_cols))), na.rm = TRUE)
    ) |>
    collapse::fselect(direct_to_ts_obs,
                      ipg_rubric,
                      ca1_score,
                      ca2_score,
                      ca3_score,
                      ac_score,
                      ad_score,
                      sp_score,
                      td_score,
                      overall_score)

  return(ipg_scored)
}

#' @title Summarize IPG Scores Across Observations
#' @description Aggregates teacher-level IPG scores by observation timepoint (`direct_to_ts_obs`) and reshapes to long format with scores and Ns.
#' Includes core actions and FSOT domain scores.
#' @param data Output from \code{\link{ipg_summarise_teacher}}.
#' @return A long-format data frame with columns: direct_to_ts_obs, Core Action, Score, n.
#'
#' @examples
#' teacher_scores <- ipg_summarise_teacher(ipg_data)
#' ipg_summarise_overall(teacher_scores)
#' @export
ipg_summarise_overall <- function(data) {

  data1 <- data |>
    collapse::fgroup_by(direct_to_ts_obs) |>
    collapse::fsummarise(
      overall_score = collapse::fmean(overall_score, na.rm = TRUE),
      ca1_n = collapse::fsum(!is.na(ca1_score)),
      ca1_score = collapse::fmean(ca1_score, na.rm = TRUE),
      ca2_n = collapse::fsum(!is.na(ca2_score)),
      ca2_score = collapse::fmean(ca2_score, na.rm = TRUE),
      ca3_n = collapse::fsum(!is.na(ca3_score)),
      ca3_score = collapse::fmean(ca3_score, na.rm = TRUE),
      ac_n = collapse::fsum(!is.na(ac_score)),
      ac_score = collapse::fmean(ac_score, na.rm = TRUE),
      ad_n = collapse::fsum(!is.na(ad_score)),
      ad_score = collapse::fmean(ad_score, na.rm = TRUE),
      sp_n = collapse::fsum(!is.na(sp_score)),
      sp_score = collapse::fmean(sp_score, na.rm = TRUE),
      td_n = collapse::fsum(!is.na(td_score)),
      td_score = collapse::fmean(td_score, na.rm = TRUE),
      n = length(overall_score)
    )

  data_ns <- data1 |>
    collapse::fselect("direct_to_ts_obs", "ca1_n", "ca2_n", "ca3_n", "ac_n", "ad_n", "sp_n", "td_n", "n") |>
    collapse::pivot(how = "longer",
                    names = list("Core Action", "n"),
                    values = c("ca1_n", "ca2_n", "ca3_n", "ac_n", "ad_n", "sp_n", "td_n", "n")) |>
    collapse::fmutate(
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "ca1_n" = "Core Action 1",
        "ca2_n" = "Core Action 2",
        "ca3_n" = "Core Action 3",
        "ac_n" = "Aligned Content",
        "ad_n" = "Assessment & Differentiation",
        "sp_n" = "Student Practice",
        "td_n" = "Teacher-Directed Instruction",
        "^n$" = "Overall"
      ))
    )

  data_final <- data1 |>
    collapse::pivot(how = "longer",
                    ids = "direct_to_ts_obs",
                    names = list("Core Action", "Score"),
                    values = c("overall_score",
                               "ca1_score", "ca2_score", "ca3_score",
                               "ac_score", "ad_score", "sp_score", "td_score")
    ) |>
    collapse::fmutate(
      `Core Action` = stringr::str_replace_all(`Core Action`, c(
        "overall_score" = "Overall",
        "ca1_score" = "Core Action 1",
        "ca2_score" = "Core Action 2",
        "ca3_score" = "Core Action 3",
        "ac_score" = "Aligned Content",
        "ad_score" = "Assessment & Differentiation",
        "sp_score" = "Student Practice",
        "td_score" = "Teacher-Directed Instruction"
      ))
    ) |>
    collapse::join(data_ns, how = "left", on = c("Core Action", "direct_to_ts_obs")) |>
    collapse::fmutate(direct_to_ts_obs = factor(direct_to_ts_obs, levels = c(
      "Baseline (first observation of the year)",
      "Mid-year (middle of service, if applicable)",
      "End of year (last observation of the year)"
    ))) |>
    collapse::roworderv(cols = "direct_to_ts_obs") |>
    collapse::fsubset(!is.na(Score) & !is.na(direct_to_ts_obs))

  return(data_final)
}
