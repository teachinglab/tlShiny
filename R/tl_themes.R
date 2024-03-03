#' @title Teaching Lab Custom Ggplot2 Theme
#'
#' @note It requires installing Roboto, Calibri fonts unless you change the font parameters
#'
#' \url{https://www.google.com/fonts}
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param strip_text_family facet label font family
#' @param strip_text_size facet label text size
#' @param plot_title_family plot title family
#' @param plot_title_size plot title font size
#' @param plot_title_margin plot title margin
#' @param subtitle_family plot subtitle family
#' @param subtitle_size plot subtitle size
#' @param subtitle_margin plot subtitle margin
#' @param caption_family plot caption family
#' @param caption_size plot caption size
#' @param caption_margin plot caption margin
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title font size
#' @param axis_title_just axis title font justification \code{blmcrt}
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param axis_text_size axis text size
#' @param ticks ticks \code{TRUE}, \code{FALSE}
#' @param dark dark mode \code{TRUE}, \code{FALSE}
#' @param markdown enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}
#' @param legend default no legend with F
#'
#' @export

theme_tl <- function(base_family = "Calibri",
                     base_size = 14,
                     strip_text_family = base_family,
                     strip_text_size = 15,
                     plot_title_family = "Calibri",
                     plot_title_size = 20,
                     plot_title_margin = 10,
                     subtitle_family = "Roboto",
                     subtitle_size = 15,
                     subtitle_margin = 15,
                     caption_family = "Roboto",
                     caption_size = 11,
                     caption_margin = 10,
                     axis_title_family = "Calibri",
                     axis_title_size = 12,
                     axis_title_just = "mm",
                     axis_text_size = 10.5,
                     dark = FALSE,
                     grid = TRUE,
                     axis = FALSE,
                     ticks = FALSE,
                     markdown = FALSE,
                     legend = F) {
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())


  if (dark == TRUE) {
    ret <- ret + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#2E3440"),
      text = ggplot2::element_text(color = "white"),
      strip.text = ggplot2::element_text(color = "white")
    )

    grid_color <- "#E5E9F0"
    tick_color <- "#E5E9F0"
  } else {
    grid_color <- "#cccccc"
    tick_color <- "black"
  }

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  if (!markdown) {
    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(size = axis_text_size, color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(size = axis_text_size, color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  } else {
    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(size = axis_text_size, color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(size = axis_text_size, color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  }

  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2))

  if (legend == F) {
    ret <- ret + ggplot2::theme(legend.position = "none")
  }

  ret
}


#' Create Teaching Lab theme to a gt table
#'
#' @param data An existing gt table object
#' @param all_caps Whether or not to capitalize titles
#' @param align Align options are "left", "center", "right"
#' @param base_font the font size
#' @param heading_font the title font size
#' @param ... Optional additional arguments to gt::table_options()
#' @return Creates a gt theme as a pipeable function
#'
#' @examples
#' mtcars |> utils::head() |> gt::gt() |> tlShiny::gt_theme_tl()
#' @export

gt_theme_tl <- function(data, all_caps = F, align = "center", base_font = 16, heading_font = 20, ...) {
  data |>
    gt::opt_all_caps(all_caps = all_caps) |>
    gt::opt_table_font(
      font = list(
        gt::google_font("Calibri"),
        gt::default_fonts()
      )
    ) |>
    # gt::tab_style(
    #   style = list(
    #     gt::cell_borders(
    #       sides = "bottom", color = "black", weight = gt::px(2)
    #     )
    #   ),
    #   locations = gt::cells_body(
    #     columns = gt::everything(),
    #     # This is a relatively sneaky way of changing the bottom border
    #     # Regardless of data size
    #     rows = nrow(data)
  #   )
  # ) |>
  # Set Table Text Size
  gt::tab_style(
    style = list(
      gt::cell_text(
        size = gt::px(base_font),
        align = align
      )
    ),
    locations = gt::cells_body(
      columns = gt::everything(),
      rows = gt::everything()
    )
  ) |>
    # Set default to center align everything
    gt::cols_align(align = "center") |>
    gt::tab_options(
      column_labels.background.color = "white",
      table.border.top.width = gt::px(3),
      table.border.top.color = "black",
      table.border.left.style = "solid",
      table.border.left.width = gt::px(3),
      table.border.left.color = "black",
      table.border.right.style = "solid",
      table.border.right.width = gt::px(3),
      table.border.right.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.color = "black",
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.lr.color = "black",
      column_labels.border.lr.width = gt::px(3),
      column_labels.font.weight = "bold",
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = gt::px(3),
      grand_summary_row.border.color = "black",
      grand_summary_row.border.width = gt::px(3),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      table.font.size = base_font,
      heading.title.font.size = heading_font,
      heading.title.font.weight = "bold",
      heading.align = "center",
      ...
    )
}
