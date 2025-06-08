.onLoad <- function(libname, pkgname) {
  montserrat_fonts <- c(
    "Montserrat-Thin",
    "Montserrat-ThinItalic",
    "Montserrat-ExtraLight",
    "Montserrat-ExtraLightItalic",
    "Montserrat-Light",
    "Montserrat-LightItalic",
    "Montserrat-Regular",
    "Montserrat-Italic",
    "Montserrat-Medium",
    "Montserrat-MediumItalic",
    "Montserrat-SemiBold",
    "Montserrat-SemiBoldItalic",
    "Montserrat-Bold",
    "Montserrat-BoldItalic",
    "Montserrat-Black",
    "Montserrat-BlackItalic"
  )

  system_fonts <- systemfonts::system_fonts()$name
  missing_mont <- setdiff(montserrat_fonts, system_fonts)

  if (length(missing_mont) > 0) {
    stop(
      "The full 'Montserrat' font family is not installed. The following fonts are missing:\n",
      paste(missing_mont, collapse = "\n"),
      "\nPlease install the full font family from Google Fonts before trying again."
    )
  }

  for (font in montserrat_fonts) {
    sysfonts::font_add(family = font, regular = systemfonts::match_font(font)$path)
  }

  showtext::showtext_auto()
}