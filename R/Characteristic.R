#' A Characteristic acts as a qualifying property to a material object.
#'
#' @field category The classifier of the type of characteristic being described.
#' @field value The value of this instance of a characteristic as relevant to the attached material.
#' @field unit If applicable, a unit qualifier for the value (if the value is numeric).
#'
#' @export
Characteristic <- R6::R6Class(
	"Characteristic",
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL
		) {
			self$category <- category
			self$value <- value
			self$unit <- unit
		},

		check_category = function() {

		},
		check_value = function() {

		},
		check_unit = function() {

		},
		set_category = function() {

		},
		set_value = function() {

		},
		set_unit = function() {

		},
		to_list = function(){

		},
		from_list = function() {

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
