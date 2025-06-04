style <- jsonlite::read_json("inst/style.json")
color <- jsonlite::read_json("inst/color.json")

# define the pipe from magrittr
"%>%" <- magrittr::"%>%"

#' Add Logo
#'
#' Adds the Baseline logo to a ggplot2 plot.
#'
#' @param plot A ggplot2 plot (of class 'gg' and 'ggplot').
#' @param color Character. A color hex.
#'
#' @return A ggplot2 plot object with the logo added.
#' @export
#' @importFrom cowplot ggdraw draw_image
#' @importFrom magick image_blank
add_logo_gg <- function(plot, color = style$logo$color) {
  cowplot::ggdraw(plot) +
    cowplot::draw_image(
      magick::image_blank(
        width = 1000,
        height = 100,
        color = color
      ),
      x = 0,
      y = 1,
      hjust = 0,
      vjust = 1,
      halign = 0,
      valign = 1,
      width = 0.25,
      height = 0.02
    )
}

#' Register Montserrat
#'
#' Registers the Montserrat font for chart and table themes.
#' @importFrom here here
#' @importFrom sysfonts font_add font_families
register_montserrat <- function() {
  # path to the montserrat font files
  path <- here::here("inst", "font", "montserrat")

  # pull loaded fonts so we don't reload montserrat
  loaded <- sysfonts::font_families()

  # montserrat font faces
  montserrat <- c(
    "montserrat_thin",
    "montserrat_thin_italic",
    "montserrat_extra_light",
    "montserrat_extra_light_italic",
    "montserrat_light",
    "montserrat_light_italic",
    "montserrat_regular",
    "montserrat_regular_italic",
    "montserrat_medium",
    "montserrat_medium_italic",
    "montserrat_semibold",
    "montserrat_semibold_italic",
    "montserrat_bold",
    "montserrat_bold_italic",
    "montserrat_black",
    "montserrat_black_italic"
  )

  # check if montserrat is already loaded
  if (!all(montserrat %in% loaded)) {
    sysfonts::font_add("montserrat_thin", file.path(path, "Montserrat-Thin.ttf"))
    sysfonts::font_add("montserrat_thin_italic", file.path(path, "Montserrat-ThinItalic.ttf"))
    sysfonts::font_add("montserrat_extra_light", file.path(path, "Montserrat-ExtraLight.ttf"))
    sysfonts::font_add("montserrat_extra_light_italic", file.path(path, "Montserrat-ExtraLightItalic.ttf"))
    sysfonts::font_add("montserrat_light", file.path(path, "Montserrat-Light.ttf"))
    sysfonts::font_add("montserrat_light_italic", file.path(path, "Montserrat-LightItalic.ttf"))
    sysfonts::font_add("montserrat_regular", file.path(path, "Montserrat-Regular.ttf"))
    sysfonts::font_add("montserrat_regular_italic", file.path(path, "Montserrat-Italic.ttf"))
    sysfonts::font_add("montserrat_medium", file.path(path, "Montserrat-Medium.ttf"))
    sysfonts::font_add("montserrat_medium_italic", file.path(path, "Montserrat-MediumItalic.ttf"))
    sysfonts::font_add("montserrat_semibold", file.path(path, "Montserrat-SemiBold.ttf"))
    sysfonts::font_add("montserrat_semibold_italic", file.path(path, "Montserrat-SemiBoldItalic.ttf"))
    sysfonts::font_add("montserrat_bold", file.path(path, "Montserrat-Bold.ttf"))
    sysfonts::font_add("montserrat_bold_italic", file.path(path, "Montserrat-BoldItalic.ttf"))
    sysfonts::font_add("montserrat_black", file.path(path, "Montserrat-Black.ttf"))
    sysfonts::font_add("montserrat_black_italic", file.path(path, "Montserrat-BlackItalic.ttf"))
  }

  invisible(TRUE)
}

#' Baseline Theme (ggplot2)
#'
#' Styles the ggplot2 theme for the Baseline.
#'
#' @param scale Numeric. Scale factor relative to a base plot of 6x6 inches.
#'
#' @return A ggplot2 theme object that can be added to a ggplot.
#' @export
#' @importFrom ggplot2 theme_void theme element_rect element_text element_blank element_line margin
#' @importFrom scales alpha
theme_baseline_gg <- function(scale = 1) {
  if (!is.numeric(scale) || scale <= 0)
    stop("`scale` must be a positive number.")

  ggplot2::theme_void() + ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = color$background,
      color = color$background,
      linewidth = 0
    ),
    plot.title = ggplot2::element_text(
      family = "montserrat_semibold",
      size = scale * style$chart$font$size$title,
      color = style$chart$font$color$title,
      margin = ggplot2::margin(b = scale * 5)
    ),
    plot.subtitle = ggplot2::element_text(
      family = "montserrat_semibold",
      size = scale * style$chart$font$size$subtitle,
      color = style$chart$font$color$subtitle,
      margin = ggplot2::margin(b = scale * 10)
    ),
    plot.caption = ggplot2::element_text(
      family = "montserrat_regular",
      size = scale * style$chart$font$size$credit,
      color = style$chart$font$color$credit,
      hjust = 0.02,
      margin = ggplot2::margin(t = scale * 10, b = scale * 10),
      lineheight = 1
    ),
    plot.caption.position = "plot",
    plot.margin = ggplot2::margin(t = scale * 20, r = scale * 20),
    panel.background = ggplot2::element_rect(
      fill = color$background,
      color = color$background,
      linewidth = 0
    ),
    panel.grid.major = ggplot2::element_line(
      color = scales::alpha(style$chart$grid$color, style$chart$grid$alpha),
      linewidth = scale * style$chart$grid$linewidth,
      linetype = style$chart$grid$rstyle
    ),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(
      family = "montserrat_semibold",
      size = scale * style$chart$font$size$label,
      color = style$chart$font$color$label
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = scale * 10)
    ),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = scale * 10, l = scale * 10)
    ),
    axis.text.x = ggplot2::element_text(
      family = "montserrat_semibold",
      size = scale * style$chart$font$size$label,
      color = style$chart$font$color$body,
      margin = ggplot2::margin(t = scale * 2)
    ),
    axis.text.y = ggplot2::element_text(
      family = "montserrat_semibold",
      size = scale * style$chart$font$size$label,
      hjust = 1,
      color = style$chart$font$color$body,
      margin = ggplot2::margin(r = scale * 2)
    ),
    text = ggplot2::element_text(
      family = "montserrat_regular",
      size = scale * style$chart$font$size$body,
      color = style$chart$font$color$body
    )
  )
}