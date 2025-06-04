.onLoad <- function(libname, pkgname) {
  register_montserrat() #nolint

  showtext::showtext_auto(enable = TRUE)
  showtext::showtext_opts(dpi = 600)
}