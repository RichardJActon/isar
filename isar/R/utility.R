#' check_comments
#'
#' @param comments a list of comments, named and with length 1 character vectors#
#' @importFrom checkmate check_list test_string
#' @importFrom purrr map_lgl
#'
#' @importFrom checkmate check_list test_string
#' @importFrom purrr map_lgl
check_comments <- function(comments) {
	check <- checkmate::check_list(
		comments, min.len = 1, types = "character", names = "named",
		null.ok = TRUE
	)
	if (!isTRUE(check)) {
		stop(check)
	} else {
		comment_is_string <- purrr::map_lgl(
			comments, ~checkmate::test_string(.x)
		)
		if(all(comment_is_string)) {
			return(TRUE)
		} else {
			stop(paste0(
				"Comment(s) ",
				paste0(which(!comment_is_string), collapse = ", "),
				"\n Are not Strings (character vectors of length 1)"
			))
		}
	}
}

#' error_with_check_message_on_failure
#'
#' Used in conjunction with checkmate check_* functions
#' has the behavior of returning TRUE if a check is TRUE and throwing an error
#' if check returns a string.
#'
#' assertions don't return TRUE on success but do error on failure and tests
#' don't produce any information about the reason for failure.
#' Checks produce a reasons on failure but don't throw an error.
#' This function turns the strings returned by check functions into error
#' messages, and passes through the TRUE if there is no message text.
#'
#' @param check the output from a `check_*` function from the checkmate package
#' @param nextline text to put on the line after check message
error_with_check_message_on_failure <- function(check, nextline = NULL) {
	if(isTRUE(check)) { return(TRUE) } else {
		if(!is.null(nextline)) { check <- paste0(check, "\n", nextline) }
		stop(check)
	}
}

#' s3_identical_maker
#'
#' function to aid in the creation of S3 generic methods for \code{[identical]}
#' or R6 objects
#' @param obj_pub_props a chacater vector of public properties whose values
#' should be compared to determine identity
#' @param get_id  (default = TRUE) Compare the object's unique identifiers
#' using the get_id method
#'
#' @importFrom purrr map_lgl
s3_identical_maker <- function(obj_pub_props, get_id = TRUE) {
	function(x, y) {
		obj_pub_props <- obj_pub_props
		obj_pub_props_lgl <- purrr::map_lgl(
			obj_pub_props, ~identical(x[[.x]], y[[.x]])
		)
		res <- NULL
		if(get_id) {
			res <- all(c(obj_pub_props_lgl, identical(x$get_id(), y$get_id())))
		} else {
			res <- all(obj_pub_props_lgl)
		}
		return(res)
	}
}

#' generate_id
#' generates a uuid with optional suffix
#' @param id a uuid
#' @param suffix a human readable string to be suffixed to the uuid
#'
#' @importFrom uuid UUIDgenerate UUIDvalidate
#' @importFrom checkmate qtest
# #' @export
generate_id <- function(id = uuid::UUIDgenerate(), suffix = character()) {
	if(!checkmate::qtest(suffix, "S?")) {
		stop("suffix must be a character vector of length 1")
	}
	if(!checkmate::qtest(suffix, "S[0]")) {
		if(!grepl("^\\w+$", suffix)) {
			# NB _ not considered special!
			stop("suffix must not contain any special characters")
		}
	}
	if(!uuid::UUIDvalidate(id)) { stop("invalid uuid!") }
	paste(c(id, suffix), collapse = "-")
}

#' test_id
#' Tests if an id is valid, returns TRUE if valid and FALSE if not
#' (follows checkmate conventions)
#' @param id a uuid or uuid with a suffix
#' @importFrom uuid UUIDvalidate
# #' @export
test_id <- function(id) {
	if(uuid::UUIDvalidate(id)) {
		return(TRUE)
	} else if (uuid::UUIDvalidate(sub("-\\w+$", "", id))) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# test_id(generate_id())
# test_id(generate_id(suffix = "x"))
# test_id(generate_id(suffix = "x-x"))
# test_id("notuuid")
# test_id(1)

#' check_id
#' Checks if an id is valid, returns TRUE if valid and explanatory string if not
#' (follows checkmate conventions)
#' @param id a uuid or uuid with a suffix
# #' @export
check_id <- function(id) {
	if(test_id(id)) { return(TRUE) } else {
		return("id must be a valid uuid or a valid uuid with a suffix")
	}
}


#' date_string_conversion
#'
#' @param date_string a string to be converted to a date
#'
#' @return a Date object
#' @export
#'
#' @examples
#' date_string_conversion("2012-08-01")
#' date_string_conversion("1Jan2009")
#'
#'
date_string_conversion <- function(date_string) {
	date <- tryCatch({
			stopifnot(grepl(
				"^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$",
				date_string
			))
			as.Date.character(date_string, tryFormats = "%Y-%m-%d")
		},
		error = function(res) {
			warning(paste0(
				"\n",
				#emo::ji("rage"),
				"😡",
				crayon::red(crayon::bold(" Date is not formated correctly!\n")),
				#emo::ji("halo"),
				"😇",
				crayon::green(
					" Please use ISO8601 compliant date strings: YYYY-mm-dd\n"
				),
				#emo::ji("worried"),
				"😟",
				crayon::yellow(
					" Attempting other date formats...\n"

				),
				crayon::yellow(crayon::bold(
				"This may result in errors!\n"
				)),
				crayon::yellow(
					"There may be ambiguity in other date formats please use the correct one!"
				)
			))
		}
	)
	if (!checkmate::test_date(date)) {
		date <- tryCatch(
			as.Date.character(
				date_string, tryFormats = c(
					"%Y-%m-%d",
					"%d/%m/%Y", "%d-%m-%Y", "%d_%m_%Y",
					"%m/%d/%Y", "%m-%d-%Y", "%m_%d_%Y",
					"%d %b %Y", "%d%b%Y"
				)
			),
			error = function(res) {
				stop(paste0(
					#emo::ji("disappointed"),
					"😞",
					crayon::red(" No Valid Date format found!")
				))
			}
		)
	}
	return(date)
}


#' green_bold
#'
#' make text green and bold with crayon
#'
#' @param string
#'
#' @export
#'
green_bold <- function(string) {
	crayon::green(crayon::bold(string))
}

#' green_bold_name_plain_content
#'
#' make text green and bold with crayon
#'
#' @param label
#' @param content
#'
#' @export
green_bold_name_plain_content <- function(label, content) {
	paste0(green_bold(paste0(label, ": ")), content)
}


#' pretty_print_comments
#'
#' Green bold section heading
#' indented comments with bold titles and wrapped contents
#'
#' @param comments a named list
#'
#' @export
pretty_print_comments <- function(comments) {
	cat(green_bold("Comments:\n")) # 🗩
	purrr::iwalk(
		# Improve comment formatting for longer comments
		comments, ~cat(
			#paste0("    ", crayon::bold(.x), ": ", .y),
			paste0("    ", crayon::bold(.y), ": "),
			stringr::str_wrap(.x, indent = 4, exdent = 4),
			sep = "\n"
		)
	)
}
