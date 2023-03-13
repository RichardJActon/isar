#' R6 class for a Source
#'
#' @field id ...
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
		id = "",
		name = "",
		characteristics = list(),
		comments = list(),

		#' @param id ...
		#' @param name A name/reference for the source material.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			id = "",
			name = "",
			characteristics = list(),
			comments = list()
		) {
			seld$id <- id
			self$name <- name
			self$characteristics <- characteristics
			self$comments <- comments
		},

		#' @details
		#'
		#' make an R list convertible to json
		#'
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			source = list(
				"id" = self$id,
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
			self$id <- lst[["id"]]
			self$name <- lst[["name"]]
			self$characteristics <- lst[["characteristics"]]
			self$comments <- lst[["comments"]]
		}

	)
)
