#' R6 class for a Source
#'
#' @field name A name/reference for the source material.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#'
#' @export
Source <- R6::R6Class(
	"Source",
	public = list(
		name = character(),
		characteristics = NULL,
		comments = NULL,

		#' @param name A name/reference for the source material.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = character(),
			characteristics = NULL,
			comments = NULL
		) {
			self$name <- name
			if(is.null(characteristics)) {
				self$characteristics <- characteristics
			} else {
				self$set_characteristics(characteristics)
			}
			self$comments <- comments
		},

		#' @details
		#' check characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
		check_characteristics = function(characteristics) {
			if(
				checkmate::test_list(characteristics, min.len = 1) &&
				all(purrr::map_lgl(
					characteristics, ~checkmate::test_r6(.x, "Characteristic")
				))
			) { return(TRUE) } else {
				stop("All characteristics must be Characteristic objects")
			}
		},
		#' @details
		#' set characteristics if characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
		set_characteristics = function(characteristics) {
			if (self$check_characteristics(characteristics)) {
				self$characteristics <- characteristics
			}
		},
		#' @details
		#' checks if comments are a named list of character vectors
		#' @param comments comments
		check_comments = function(comments) { check_comments(comments) },
		#' @details
		#' Sets comments if they are in a valid format
		#' @param comments a list of comments
		set_comments = function(comments) {
			if(self$check_comments(comments)) { self$comments <- comments }
		},
		#' @details
		#'
		#' make an R list convertible to json
		#'
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			source = list(
				"id" = private$id,
				"name" = self$name,
				"characteristics" = self$characteristics,
				"comments" = self$comments
			)
			return(source)
		},

		#' #' @details
		#'
		#' Make \code{[Source]} from list
		#'
		#' @param lst a source object serialized to a list
		from_list = function(lst) {
			private$id <- lst[["id"]]
			self$name <- lst[["name"]]
			self$characteristics <- purrr::map(lst[["characteristics"]], ~{
				ch <- Characteristic$new()
				ch$from_list(.x)
				ch
			})
			self$comments <- lst[["comments"]]
		},

		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		}
	),
	private = list(
		id = uuid::UUIDgenerate()
	)
)
