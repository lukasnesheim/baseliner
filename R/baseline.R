#' @importFrom jsonlite read_json
style <- jsonlite::read_json("inst/style.json")
color <- jsonlite::read_json("inst/color.json")

# define the pipe from magrittr
"%>%" <- magrittr::"%>%"

#' Add `ggplot2` Logo
#'
#' Adds the Baseline logo to a `ggplot2` plot.
#'
#' @param plot A `ggplot2` plot (of class 'gg' and 'ggplot').
#' @param color Character. A color hex defaulted to `#333333`.
#'
#' @return A `ggplot2` plot object with the logo added.
#'
#' @importFrom cowplot ggdraw draw_image
#' @importFrom magick image_blank
#'
#' @export
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

#' Add `gt` Logo
#'
#' Adds the Baseline logo to a `gt` table.
#'
#' @param table A magick_image.
#' @param width Numeric. A width value in pixels.
#' @param height Numeric. A height value in pixels.
#' @param color Character. A color hex defaulted to `#333333`.
#'
#' @importFrom magick image_blank image_composite
#'
#' @export
add_logo_gt <- function(table, width = 1000, height = 1000, color = style$logo$color) {
  magick::image_composite(
    table,
    magick::image_blank(
      width = width * 0.25,
      height = height * 0.02,
      color = color
    ),
    gravity = "northwest"
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

#' Baseline Plot Theme
#'
#' Apply the Baseline theme to a `ggplot2` plot.
#'
#' @param scale Numeric. Scale factor relative to a base plot of 6x6 inches.
#'
#' @returns A `ggplot2` theme object that can be added to a ggplot.
#'
#' @import ggplot2
#' @importFrom scales alpha
#'
#' @export
theme_baseline_gg <- function(scale = 1) {

  stopifnot(`'scale' must be a positive number.` = is.numeric(scale) && scale > 0)

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

#' Baseline Table Theme
#'
#' Apply the Baseline theme to a `gt` table.
#'
#' @param table An existing gt table object of class `gt_tbl`.
#' @param ... Additional optional arguments passed to `gt::tab_options()` to override table styling.
#'
#' @returns Returns a styled `gt` table.
#'
#' @import gt
#' @importFrom magrittr %>%
#'
#' @export
theme_baseline_gt <- function(table, ...) {

  stopifnot(`'table' must be a 'gt_tbl'.` = "gt_tbl" %in% class(table))

  montserrat <- gt::google_font("Montserrat")
  table_id <- table[["_options"]]$value[table[["_options"]]$parameter == "table_id"][[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    table[["_options"]][["value"]][[which("table_id" %in% table[["_options"]][["parameter"]])[[1]]]] <- table_id
  }

  table %>% # nolint
    # title
    gt::tab_style(
      locations = gt::cells_title(groups = "title"),
      style = gt::cell_text(
        color = style$table$font$color$title,
        font = montserrat,
        size = gt::px(style$table$font$size$title),
        weight = style$table$font$weight$title
      )
    ) %>%
    # subtitle
    gt::tab_style(
      locations = gt::cells_title(groups = "subtitle"),
      style = gt::cell_text(
        color = style$table$font$color$subtitle,
        font = montserrat,
        size = gt::px(style$table$font$size$subtitle),
        weight = style$table$font$weight$subtitle
      )
    ) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        color = style$table$font$color$body,
        font = montserrat,
        size = gt::px(style$table$font$size$label),
        weight = style$table$font$weight$label
      )
    ) %>%
    # column label
    gt::tab_style(
      locations = gt::cells_column_labels(),
      style = list(
        gt::cell_text(
          color = style$table$font$color$body,
          font = montserrat,
          size = gt::px(style$table$font$size$body),
          weight = style$table$font$weight$label
        ),
        gt::cell_fill(
          color = color$background
        )
      )
    ) %>%
    # row group
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          color = style$table$font$color$body,
          font = montserrat,
          size = gt::px(style$table$font$size$body),
          weight = style$table$font$weight$label
        ),
        gt::cell_fill(
          color = color$background
        )
      )
    ) %>%
    # row body
    gt::tab_style(
      locations = gt::cells_body(),
      style = gt::cell_text(
        color = style$table$font$color$body,
        font = montserrat,
        size = gt::px(style$table$font$size$body),
        weight = style$table$font$weight$body
      )
    ) %>%
    # source note
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        color = style$table$font$color$credit,
        font = montserrat,
        size = gt::px(style$table$font$size$credit),
        weight = style$table$font$weight$body
      )
    ) %>%
    # footnote
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        color = style$table$font$color$credit,
        font = montserrat,
        size = gt::px(style$table$font$size$credit),
        weight = style$table$font$weight$body
      )
    ) %>%
    # table options
    gt::tab_options(
      table.background.color = color$background,
      table.border.bottom.style = "none",
      table.border.top.style = "none",
      table_body.hlines.color = "transparent",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "white",
      heading.align = "left",
      heading.border.bottom.style = "none",
      column_labels.border.top.color = color$london[[3]],
      column_labels.border.top.width = gt::px(1),
      column_labels.border.bottom.style = "none",
      row_group.border.top.style = "none",
      row_group.border.top.color = color$london[[3]],
      row_group.border.bottom.style = "solid",
      row_group.border.bottom.color = color$london[[3]],
      row_group.border.bottom.width = gt::px(1),
      row_group.padding = gt::px(1.5),
      data_row.padding = 1,
      source_notes.border.lr.style = "none",
      ...
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_css(c(
      paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #333333;}"),
      paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px; text-align: center}"),
      paste0("#", table_id, " .gt_subtitle {padding-top: 0px !important; padding-bottom: 4px !important;}"),
      paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
      paste0("#", table_id, " .gt_column_spanner {text-decoration: underline;}")
    ))
}