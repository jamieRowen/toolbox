#' Print a widget image to png
#'
#' Prints a browsable htmlwidget (that for example might show
#' in RStudio viewer pane to a png file)
#'
#' @param x a browsable htmlwidget
#' @param file a file path to write the image to
#' @param background colour for the image
#' @return invisibly the file path of written image
print_html_as_png = function(x, file, background = "white") {
  www_dir = tempfile("printhtml")
  dir.create(www_dir)
  on.exit({
    unlink(www_dir)
  }, add = TRUE, after = TRUE)
  index_html = file.path(www_dir, "index.html")
  htmltools::save_html(
    x, file = index_html, background = background,
    libdir = "lib"
  )
  webshot::webshot(index_html, file = file)
  invisible(file)
}
