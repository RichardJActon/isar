#' check_comments
#'
#' @param comments a list of comments, named and with length 1 character vectors#
#' @importFrom checkmate check_list test_string
#' @importFrom purrr map_lgl
#'
#' @importFrom checkmate check_list test_string test_date
#' @importFrom purrr map_lgl iwalk
#' @importFrom cli col_red col_green col_yellow col_red col_cyan style_bold cli_h2 cli_h3 cli_text
#'
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


#' date_input_handling
#'
#' @param date_string a string to be converted to a date
#'
#' @return a Date object
#' @export
#'
#' @examples
#' date_input_handling("2012-08-01")
#' date_input_handling("1Jan2009", strict = FALSE)
#'
#'
#date_string_conversion <- function(date) {
date_input_handling <- function(date, strict = TRUE, null.ok = FALSE) {
	if(isTRUE(null.ok) && (is.null(date) || date == "" || test_empty(date)) ){
		return(NULL)
	}
	if(checkmate::test_date(date)) {
		return(date)
	}
	message <- paste0(
		"\n",
		#emo::ji("rage"),
		"😡",
		cli::col_red(cli::style_bold(
			" Date is not formated correctly!\n"
		)),
		#emo::ji("halo"),
		"😇",
		cli::col_green(
			" Please use an ISO8601 compliant date string: YYYY-mm-dd\n"
		),
		cli::col_yellow(
			" Alternatively you can directly provide an R date object\n",
			" It is prefereble to fix the source but if your date is\n",
			" in a known non-iso format you can use `as.Date.character()`\n",
			" and supply your date format in the `tryFormats` argument\n",
			" to get an R date object.\n"
		)
	)
	if(checkmate::test_string(date)) {
		date_string_lgl <- grepl(
			"^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$",
			date
		)
		if(isTRUE(date_string_lgl)) {
			return(as.Date.character(date, tryFormats = "%Y-%m-%d"))
		} else if(isTRUE(strict)) {
				stop(message)
			} else {
				warning(paste0(message,
					"😟",
					cli::col_yellow(" Attempting other date formats...\n"),
					cli::col_yellow(cli::style_bold(
						" This is NOT ADVISED may result in errors due to ambiguity in the date format!\n"
					)),
					cli::col_yellow(
						" Please check the messages to see if the guessed date format is correct!\n"
					)
				))
				date <- tryCatch(
					as.Date.character(
						date, tryFormats = c(
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
							cli::col_red(" No Valid Date format found!")
						))
					}
				)
				message(
					"Did you mean ", cli::col_yellow(cli::style_bold(
						format(date, "%A %d %B %Y (%Y-%m-%d)")
					)), "?\n"
				)
				return(date)
			}
	} else {
		stop(message)
	}
}


#' green_bold
#'
#' make text green and bold with crayon
#'
#' @param string a string to make green and bold
#'
#' @export
#'
green_bold <- function(string) {
	#crayon::green(crayon::bold(string))
	cli::col_green(cli::style_bold(string))
}

#' green_bold_name_plain_content
#'
#' make text green and bold with crayon
#'
#' @param label key
#' @param content value
#'
#' @export
green_bold_name_plain_content <- function(label, content) {
	cli::cli_text(paste0(green_bold(paste0(label, ": ")), content))
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
	if (length(comments) > 0) {
		cli::cli_h2(cli::col_cyan("Comments (", length(comments), ") 🗩"))
		#cat(green_bold("Comments:\n")) # 🗩
		purrr::iwalk(
			# Improve comment formatting for longer comments
			# comments, ~cat(
			# 	paste0("    ", crayon::bold(.y), ": "),
			# 	stringr::str_wrap(.x, indent = 4, exdent = 4),
			# 	sep = "\n"
			# )
			comments, ~{
				cli::cli_par()
				#cli::cli_h3(cli::cli_text( .y, ". ", .x$name))
				cli::style_bold(cli::cli_text( .y, ". ", .x$name))
				cli::cli_end()
				cli::cli_par()
				cli::cli_text(.x$value)
				cli::cli_end()
			}
		)
	}
}

#' unlist_sort_by_name
#'
#' @param x a named nested list
#'
unlist_sort_by_name <- function(x) {
	x <- unlist(x)
	x <- x[order(names(x))]
	x
}

#' recursive_sort_list_by_name
#'
#' @param x a list
#'
recursive_sort_list_by_name <- function(x) {
	if (is.null(names(x))) {
		srt <- x
	} else {
		srt <- x[order(names(x))]
	}
	for(i in seq_along(srt)) {
		if(is.list(srt[[i]])) {
			srt[[i]] <- sort_list_by_name(srt[[i]])
		}
	}
	srt
}

#' comment_to_table
#'
#' @param comments list of comments
comment_to_table <- function(comments) {
	if (is.null(comments)) { return(NULL) }
	comments %>%
		purrr::map_dfr(
			~tibble::tribble(
				~rowname, ~value,
				paste0("Comment[", .x$name, "]"), .x$value
			)
		) %>%
		dplyr::arrange(rowname)
		# dplyr::arrange(dplyr::desc(rowname))
}

comment_to_table_wide <- function(comments) {
	comments %>%
		purrr::map_dfc(~{
			tibble::tibble(.x$value) %>%
				purrr::set_names(paste0("Comment[", .x$name, "]"))
		})
}

#' get_r6_class
#'
#' Checks if an object is an R6 object and if it is returns the class.
#' An error is thrown if the object is not and R6 object.
#'
#' @param x an R6 object for which to get the class
get_r6_class <- function(x) {
	if(checkmate::test_r6(x, "R6")) {
		class(x)[1]
	} else {
		stop("x is not and R6 object!")
	}
}

#
# enum <- function(...) {
#
# 	values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)
#
# 	stopifnot(identical(unique(values), values))
#
# 	res <- setNames(seq_along(values), values)
# 	res <- as.environment(as.list(res))
# 	lockEnvironment(res, bindings = TRUE)
# 	res
# }
#
# test_enum <- function(value, enum) {if(!value %in% names(enum)){
# 	stop(
# 		value, " is not in the list of permitted values: ",
# 		paste0(names(enum), collapse = ", ")
# 	)
# }}

# no name requirement - not an enum per se but can be use to
# define a set of arbitrary permitted values
# unnamed_enum <- function(lst) {
# 	lst <- list(...)
# 	nameless <- lst
# 	setNames(nameless, NULL)
# 	stopifnot(
# 		identical(unique(lst), nameless) &&
# 			identical(unique(names(lst)), names(lst))
# 	)
# 	res <- rlang::env(values = lst)
# 	lockEnvironment(res, bindings = TRUE)
# 	res
# }
#
# check_unnamed_enum <- function(value, unnamed_enum) {
# 	flag <- new.env()
# 	for(i in enum$values) { if(identical(value, i)){ flag <- value } }
# 	if(identical(flag, value)) {
# 		return(TRUE)
# 	} else {
# 		paste0(value, " is not in the list of permitted values!")
# 	}
# }
#
# assert_unnamed_enum <- checkmate::makeAssertionFunction(unnamed_enum)
# expect_unnamed_enum <- checkmate::makeExpectationFunction(unnamed_enum)
# test_unnamed_enum <- checkmate::makeTestFunction(unnamed_enum)

# ev <- new.env()
# x <- enum(list(x=NA,NULL,"","name","nom","nam",list("x"),ev))
# test_enum("",x)
# test_enum("x",x)
# test_enum(list("x"),x)
# check_enum("",x)
# check_enum("x",x)
# check_enum(list(list(x="y")),x)
# assert_enum(list(list(x="y")),x)
# assert_enum(list("x"),x)
# # expect_enum(list(list(x="y")),x)
# expect_enum(list("x"),x)
# check_enum(new.env(),x)
# check_enum(ev,x)

enum <- function(...) {
	lst <- list(...)
	nameless <- lst
	names(nameless) <- NULL
	stopifnot(
		identical(unique(lst), nameless) &&
		identical(unique(names(lst)), names(lst))
	)
	res <- as.environment(lst)
	lockEnvironment(res, bindings = TRUE)
	res
}

check_enum <- function(value, enum) {
	flag <- new.env()
	for(i in names(enum)) {
		if(identical(value, enum[[i]])){
			flag <- value
			break
		}
	}
	if(identical(flag, value)) {
		return(TRUE)
	} else {
		paste0(value, " is not in the list of permitted values!")
	}
}

assert_enum <- checkmate::makeAssertionFunction(check_enum)
expect_enum <- checkmate::makeExpectationFunction(check_enum)
test_enum <- checkmate::makeTestFunction(check_enum)

# ev <- new.env()
# #x <- enum(x=NA,null=NULL,emptystr="")
# x <- enum(x=NA,null=NULL,emptystr="",q="name",j="nom",n="nam",lst=list("x"),ev=ev)
# test_enum("",x)
# test_enum("x",x)
# test_enum(list("x"),x)
# check_enum("",x)
# check_enum("x",x)
# check_enum(list(list(x="y")),x)
# assert_enum(list(list(x="y")),x)
# assert_enum(list("x"),x)
# # expect_enum(list(list(x="y")),x)
# expect_enum(list("x"),x)
# check_enum(new.env(),x)
# check_enum(ev,x)

assert_enum_reference <- function(value, enum) {
	flag <- new.env()
	flag_i <- NULL
	for(i in names(enum)) {
		if(identical(value, enum[[i]])) {
			flag <- value
			flag_i <- i
		}
	}
	if(identical(flag, value)) {
		return(flag_i)
	} else {
		paste0(value, " is not in the list of permitted values!")
	}
}

# assert_enum_reference("",x)

valid_isa_process_types <- enum(
	`Unspecified` = NULL,
	`Data Transformation` = "Data Transformation",
	Normalization = "Normalization"
)
