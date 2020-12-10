#' Add a class to a R object
#'
#' @param x
#' @param class_name
#'
#' @return
#' @export
#'
#' @examples
add_class <- function(x, class_name) {
    class(x) <- class_name
    x
}
