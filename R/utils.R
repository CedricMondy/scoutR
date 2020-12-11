#' Add a class to a R object
#'
#' @param x
#' @param class_name
#'
#' @return
#' @export
#'
add_class <- function(x, class_name, replace = FALSE) {
    if (replace) {
        class(x) <- class_name
    } else {
        class(x) <- c(class(x), class_name)
    }

    x
}
