#' Add a class to a R object
#'
#' Simple wrapper around the {base} `class` function to allow its use in
#' {magrittr} pipe workflow
#'
#' @param x a R object
#' @param class_name the class name to be assigned to x
#' @param replace logical. Whether the class_name should be added to (FALSE,
#'   default) or should replace (TRUE) existing classes.
#'
#' @return the R object `x` with updated class assignment
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

#' Test if object is of class sf or sfc
#'
#' @param x a R object
#'
#' @return logical, whether or not the object if of class sf or sfc
#'
#' @export
#'
#' @importFrom stringr str_detect
is_sf.c <- function(x) {
    any(str_detect(string = class(x), pattern = "(^sf$)|(^sfc$)"))
}
