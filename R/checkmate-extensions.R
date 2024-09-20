# checkmate extensions ----

## check_empty ----
#' check_empty
#'
#' @param x object to check
#' @param mode vector mode, defaults to the type of the vector
#' @param null.ok allow NULL to be valid
#' @param zero.len.string.ok accept zero length strings
#' 
#' @return TRUE or string containing an error message
#' @export
#'
#' @examples
#'
#' mode <- "character"
#' check_empty(character(), mode)
#' check_empty(character(), mode, null.ok = TRUE)
#' check_empty(NULL, mode, null.ok = TRUE)
#' check_empty("", mode)
#' check_empty("", mode, null.ok = TRUE)
#'
check_empty <- function(
	x, mode = typeof(x), null.ok = FALSE, zero.len.string.ok = FALSE
) {
	if(null.ok && is.null(x)) { return(TRUE) } 
	if (mode != "NULL") { v <- vector(mode = mode) }
	if(zero.len.string.ok && mode == "character") {
		x <- strsplit(x, split = "")[[1]]
	}
	res <- identical(x, v)
	if(res) { return(TRUE) }
	text <- paste0("Must be empty ", mode, " vector")
	if (null.ok) { text <- paste0(text, " or NULL") }
	if (null.ok && mode == "NULL") { text <- "Must be empty vector or NULL" }
	return(text)
}

## assert_empty ----
#' assert_empty
#'
#' @param x object to check
#' @param mode vector mode
#' @param null.ok allow NULL to be valid
#' @param zero.len.string.ok accept zero length strings
#'
#' @return x or throws an error
#' @export
#'
#' @examples
#'
#' mode <- "character"
#' assert_empty(character(), mode)
#' assert_empty(character(), mode, null.ok = TRUE)
#' assert_empty(NULL, mode, null.ok = TRUE)
#' assert_empty("", mode)
#' assert_empty("", mode, null.ok = TRUE)
#' @importFrom checkmate makeAssertionFunction
assert_empty <- checkmate::makeAssertionFunction(check_empty)

## expect_empty ----
#' expect_empty
#'
#' @param x object to check
#' @param mode vector mode
#' @param null.ok allow NULL to be valid
#' @param zero.len.string.ok accept zero length strings
#'
#' @return use with testthat
#' @export
#'
#' @examples
#'
#' mode <- "character"
#' expect_empty(character(), mode)
#' expect_empty(character(), mode, null.ok = TRUE)
#' expect_empty(NULL, mode, null.ok = TRUE)
#' expect_empty("", mode)
#' expect_empty("", mode, null.ok = TRUE)
#'
#' @importFrom checkmate makeAssertionFunction makeExpectation
expect_empty <- checkmate::makeExpectationFunction(check_empty)

## test_empty ----
#' test_empty
#'
#' @param x object to check
#' @param mode vector mode
#' @param null.ok allow NULL to be valid
#' @param zero.len.string.ok accept zero length strings
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#'
#' mode <- "character"
#' test_empty(character(), mode)
#' test_empty(character(), mode, null.ok = TRUE)
#' test_empty(NULL, mode, null.ok = TRUE)
#' test_empty("", mode)
#' test_empty("", mode, null.ok = TRUE)
#'
#' @importFrom checkmate makeTestFunction
test_empty <- checkmate::makeTestFunction(check_empty)
