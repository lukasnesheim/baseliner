#' @importFrom jsonlite read_json
style <- jsonlite::read_json("inst/style.json")
color <- jsonlite::read_json("inst/color.json")

# define the pipe from magrittr
"%>%" <- magrittr::"%>%"

#' Add `ggplot2` Logo
#'
#' Adds the Baseline logo to a `ggplot2` plot.
#'
#' @param plot A `ggplot2` plot object.
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
#' @param table A `magick_image`.
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

#' Baseline Plot
#'
#' Applies the Baseline theme and adds the Baseline logo to a `ggplot2` plot.
#' This is a convenience wrapper which applies the theme using \code{\link{theme_baseline_gg}}
#' to a plot and then adds the logo to a plot using \code{\link{add_logo_gg}}.
#' Use the individual functions for more manual control and styling overrides.
#'
#' @param plot A ggplot2 plot object.
#'
#' @return A ggplot2 plot object with the Baseline theme and logo.
#'
#' @export
baseline_plot <- function(plot) {
  add_logo_gg(plot + theme_baseline_gg())
}

#' Get Baseline Style
#'
#' Reads and returns the style configuration JSON as a list.
#'
#' @return A named, nested list of style settings.
#'
#' @importFrom jsonlite read_json
#'
#' @export
get_style <- function() {
  style_path <- system.file("style.json", package = "baseliner")
  jsonlite::read_json(style_path)
}

#' Get Baseline Color
#'
#' Reads and returns the color configuration JSON as a list.
#'
#' @return A named, nested list of color settings.
#'
#' @importFrom jsonlite read_json
#'
#' @export
get_color <- function() {
  color_path <- system.file("color.json", package = "baseliner")
  jsonlite::read_json(color_path)
}

#' Save `ggplot2` Plot with Logo
#'
#' Adds the Baseline logo to a `ggplot2` plot and saves the plot.
#'
#' @param plot A `ggplot2` plot object.
#' @param output A file path where the output image will be saved.
#' @param width Numeric. The width of the output image in **inches** defaulted to 6.
#' @param height Numeric. The height of the output image in **inches** defaulted to 6.
#' @param dpi Numeric. The resolution of the output image in **dots per inch (DPI)** defaulted to 6.
#'
#' @import ggplot2 grid
#' @importFrom grDevices dev.off
#' @importFrom magick image_read
#' @importFrom ragg agg_png
#'
#' @export
ggsave_with_logo <- function(plot, output, width = 6, height = 6, dpi = 600) {
  temp <- tempfile(fileext = ".png")

  ggplot2::ggsave(
    filename = temp,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    device = ragg::agg_png
  )

  plot_image <- magick::image_read(temp)
  plot_grob <- grid::rasterGrob(
    plot_image,
    width = ggplot2::unit(1, "npc"),
    height = ggplot2::unit(1, "npc")
  )

  logo_image <- magick::image_read(system.file("extdata", "logo.png", package = "baseliner"))
  logo_grob <- grid::rasterGrob(
    logo_image,
    x = ggplot2::unit(0, "npc"),
    y = ggplot2::unit(1, "npc"),
    just = c("left", "top"),
    width = ggplot2::unit(1.5, "in")
  )

  ragg::agg_png(
    output,
    width = width,
    height = height,
    units = "in",
    res = dpi
  )

  grid::grid.newpage()
  grid::grid.draw(plot_grob)
  grid::grid.draw(logo_grob)

  grDevices::dev.off()
}

#' Baseline Plot Theme
#'
#' Apply the Baseline theme to a `ggplot2` plot.
#'
#' @param scale Numeric. Scale factor relative to a base plot of 6x6 inches.
#'
#' @return A `ggplot2` theme object that can be added to a ggplot.
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
      family = style$chart$font$family$title,
      size = scale * style$chart$font$size$title,
      color = style$chart$font$color$title,
      margin = ggplot2::margin(b = scale * 5)
    ),
    plot.subtitle = ggplot2::element_text(
      family = style$chart$font$family$subtitle,
      size = scale * style$chart$font$size$subtitle,
      color = style$chart$font$color$subtitle,
      margin = ggplot2::margin(b = scale * 10)
    ),
    plot.caption = ggplot2::element_text(
      family = style$chart$font$family$credit,
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
      family = style$chart$font$family$label,
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
      family = style$chart$font$family$label,
      size = scale * style$chart$font$size$label,
      color = style$chart$font$color$color,
      margin = ggplot2::margin(t = scale * 2)
    ),
    axis.text.y = ggplot2::element_text(
      family = style$chart$font$family$label,
      size = scale * style$chart$font$size$label,
      hjust = 1,
      color = style$chart$font$color$label,
      margin = ggplot2::margin(r = scale * 2)
    ),
    text = ggplot2::element_text(
      family = style$chart$font$family$body,
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
#' @return A styled `gt` table.
#'
#' @import gt
#' @importFrom magrittr %>%
#'
#' @export
theme_baseline_gt <- function(table, ...) {
  stopifnot(`'table' must be a 'gt_tbl'.` = "gt_tbl" %in% class(table))

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
        font = style$table$font$family,
        size = gt::px(style$table$font$size$title),
        weight = style$table$font$weight$title
      )
    ) %>%
    # subtitle
    gt::tab_style(
      locations = gt::cells_title(groups = "subtitle"),
      style = gt::cell_text(
        color = style$table$font$color$subtitle,
        font = style$table$font$family,
        size = gt::px(style$table$font$size$subtitle),
        weight = style$table$font$weight$subtitle
      )
    ) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        color = style$table$font$color$body,
        font = style$table$font$family,
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
          font = style$table$font$family,
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
          font = style$table$font$family,
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
        font = style$table$font$family,
        size = gt::px(style$table$font$size$body),
        weight = style$table$font$weight$body
      )
    ) %>%
    # source note
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        color = style$table$font$color$credit,
        font = style$table$font$family,
        size = gt::px(style$table$font$size$credit),
        weight = style$table$font$weight$credit
      )
    ) %>%
    # footnote
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        color = style$table$font$color$credit,
        font = style$table$font$family,
        size = gt::px(style$table$font$size$credit),
        weight = style$table$font$weight$credit
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