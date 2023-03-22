#' An R6 object for representing ParameterValue
#' A ParameterValue represents the instance value of a \code{[ProtocolParameter]}, used in a Process.
#' @field category A link to the relevant \code{[ProtocolParameter]} that the value is set for.
#' @field value The value of the parameter.
#' @field unit The qualifying unit classifier, if the value is numeric.
#' @field comments Comments associated with instances of this class.
ParameterValue <- R6::R6Class(
	"ParameterValue",
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		comments = NULL,
		#' @details
		#' New \code{[ParameterValue]} object
		#' @param category A link to the relevant \code{[ProtocolParameter]} that the value is set for.
		#' @param value The value of the parameter.
		#' @param unit The qualifying unit classifier, if the value is numeric.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL,
			comments = NULL
		){
			self$category <- category
			self$value <- value
			if(is.null(unit)) { self$unit <- unit } else {
				self$set_unit(unit)
			}
			self$comments <- comments
		},
		#' @details
		#' check if unit is a \code{[Unit]} object
		#' @param unit a \code{[Unit]} object
		check_unit = function(unit) {
			check <- checkmate::check_r6(unit, "Unit")
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set unit if input is valid
		#' @param unit a \code{[Unit]} object
		set_unit = function(unit) {
			if(self$check_unit(unit)) { self$unit <- unit }
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
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' Person$new()
		to_list = function(ld = FALSE) {
			parameter_value = list(
				"id" = private$id,
				"category" = self$category,
				"value" = self$value,
				"unit" = self$unit,
				"comments" = self$comments
			)
			return(parameter_value)
		},

		#' @details
		#'
		#' Make \code{[Person]} from list
		#'
		#' @param lst an \code{[Person]} object serialized to a list
		from_list = function(lst) {
			private$id <- lst[["id"]]
			self$category <- lst[["category"]]
			self$value <- lst[["value"]]
			self$unit <- lst[["unit"]]
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
