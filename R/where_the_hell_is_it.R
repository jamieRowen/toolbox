#' Find string source in a function definition
#'
#' Occasionally I want to track down the source across all
#' packages of a piece of code. This solution was motivated by
#' trying to track down use of an unexported function from a package.
#' This function searches all function definitions in your package library
#' (both exports and non-exports) for a particular string.
#'
#' @examples \dontrun{
#'  where_the_hell_is_it("yaml_load_file")
#' }
#' @param str Character to search for
#' @export
where_the_hell_is_it = function(str) {
  p = utils::installed.packages()[, 1]
  pb = progress::progress_bar$new(
    total = length(p),
    format = "scanning  :what [:bar] :percent eta: :eta "
  )
  p_envs = purrr::map(p, getNamespace)
  all_funs = purrr::map_dfr(p_envs, ~{
    pb$tick(tokens = list(what = .x$.packageName))
    res = lapply(
      eapply(.x, function(x) if (is.function(x)) as.character(body(x))),
      function(x) any(stringr::str_detect(x, str))
    )
    if (length(res) > 0) {
      dplyr::filter(
        tibble::tibble(
          package = .x$.packageName,
          def = names(res),
          contains_str = unlist(res)
        ),
        .data$contains_str
      )
    } else {
      NULL
    }
  })
  all_funs
}
