#' Teaching Lab Color Palette Maker
#'
#' @param theme if theme is light or dark
#' @param n number of colors to generate
#' @param color the color palette to generate
#' @param base_color_start the base color of the palette to start with, ramping towards the color option
#' @return color ramp palette function
#' @export
tl_palette <- function(color = c("blue", "orange", "purple", "green", "teal", "tl_colors"),
                       theme = c("dark"),
                       n = 6,
                       base_color_start = NULL) {
  if (is.null(base_color_start)) {
        base_color_start <- if (theme == "light") {
        "#F7FBFD"
      } else if (theme == "dark") {
        "#040404"
      }
    }


  if (color == "blue") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#00acf0"))
  } else if (color == "green") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#43c6b9"))
  } else if (color == "orange") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#ff7b43"))
  } else if (color == "purple") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#d17df7"))
  } else if (color == "teal") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#55bbc7"))
  } else if (color == "tl_colors") {
    col <- grDevices::colorRampPalette(c("#30BCED", "#303036", "#FC5130", "#d17df7", "#6A2E35", "#B6D094"))
  } else {
    col <- grDevices::colorRampPalette(c(base_color_start, "#00acf0"))
  }

  col(n)
}

#' @title TL Default Blue Palette
#'
#' @export
tl_pal_blue <- c("#040404", "#031C25", "#023447", "#024C69", "#01648A", "#017CAC", "#0094CE", "#00ACF0")

#' Discrete color & fill scales based on the Teaching Lab palette
#'
#' See [tl_palette()].
#'
#' @md
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_tl
#' @param n the number of colors
#' @param color the color from `tl_palette`
#' @export
scale_colour_tl <- function(n, color = "blue", ...) {
  # ggplot2::discrete_scale("colour", "tl", tl_pal(), ...)
  ggplot2::scale_color_manual(values = tlShiny::tl_palette(color = color, theme = "dark", n = n))
}

#' @export
#' @rdname scale_tl
scale_color_tl <- scale_colour_tl

#' @export
#' @rdname scale_tl
scale_fill_tl <- function(n, color = c("blue", "orange", "purple", "green", "teal", "tl_colors"), ...) {
  # ggplot2::discrete_scale("fill", "tl", tl_pal(), ...)
  ggplot2::scale_fill_manual(values = rev(tlShiny::tl_palette(color = "blue", theme = "dark", n = n)))
}

#' Teaching Lab Color Palette Maker
#'
#' @param n number of colors to generate
#' @param base_color_start the base color of the palette to start with, ramping from the color option
#' @param base_color_end the base color of the palette to end with, ramping towards the color option
#' @return color ramp palette function
#' @export
tl_palette2 <- function(n = 6,
                       base_color_start = NULL,
                       end_color_start = NULL) {

  col <- grDevices::colorRampPalette(c(base_color_start, end_color_start))

  col(n)
}
