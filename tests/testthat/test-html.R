test_that("print html to png", {
  # fake minimal shiny ui
  ui = shiny::fluidPage(
    shiny::tags$div("Hello World")
  )
  ui = htmltools::browsable(ui)
  target_file = "test.png"
  on.exit(unlink(target_file), add = TRUE)
  print_html_as_png(ui, target_file)
  expect_true(file.exists(target_file))
})
