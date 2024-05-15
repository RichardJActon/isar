#' Characteristic
#'
#' A Characteristic acts as a qualifying property to a material object.
#'
#' @field category The classifier of the type of characteristic being described.
#' @field value The value of this instance of a characteristic as relevant to the attached material.
#' @field unit If applicable, a unit qualifier for the value (if the value is numeric).
#' @field comments comments
#'
#' @export
Characteristic <- R6::R6Class(
	"Characteristic",
	inherit = OntologySourceReferences,
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		comments = NULL,
		`@id` =  character(),
		#' @details
		#' Create a new \code{[Characteristics]} object
		#' @param category The classifier of the type of characteristic being described.
		#' @param value The value of this instance of a characteristic as relevant to the attached material.
		#' @param unit If applicable, a unit qualifier for the value (if the value is numeric).
		#' @param comments comments
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL,
			comments = NULL,
			`@id` = character()
		) {
			if(is.null(category)) {
				self$category <- category
			} else {
				self$set_category(category)
			}
			self$value <- value
			self$unit <- unit
			self$comments <- comments
			self$`@id` <- `@id`
			#self$`@id` <- paste0("#characteristic_category/", self$)
		},
		#' @details
		#' Check that category is an \code{[OntologyAnnotation]} object
		#' @param category an \code{[OntologyAnnotation]} object
		check_category = function(category) {
			check <- checkmate::check_r6(category, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set category if input is valid
		#' @param category an \code{[OntologyAnnotation]} object
		set_category = function(category) {
			if(self$check_category(category)) { self$category <- category }
		},
		#' @details
		#' check that value is numeric
		#' @param value a value in the specified units
		check_value = function(value) {
			if (is.numeric(value)) { return(TRUE) }
		},
		#' @details
		#' set the value if input is valid
		#' @param value a value in the specified units
		set_value = function(value) {
			if(self$check_value(value)) { self$value <- value }
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
		#' Add comment if it is in a valid format
		#' @param comment a list of comments
		add_comment = function(comment) {
			if(self$check_comments(comment)) {
				self$comments <- c(comments, comment)
			}
		},
		#' @details
		#' An R list representation of a \code{[Characteristic]} object
		#' @param ld linked data (default FALSE)
		#' @param recursive call to_list methods of any objects within this object (default FALSE)
		to_list = function(ld = FALSE, recursive = FALSE){
			characteristic <- list(
				"id" = private$id,
				"category" = ifelse(
					recursive, self$category$to_list(), self$category$term
				),
				"value" = self$value,
				"unit" = ifelse(
					recursive, self$unit$to_list(), self$unit$unit$term
				),
				"comments" = self$comments
			)
			return(characteristic)
		},
		#' @details
		#' Make \code{[Characteristic]} object from list
		#' @param lst an Characteristic object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				self$set_id()
				if(is.null(lst[["@id"]])) {
					self$`@id` <- lst[["category"]][["@id"]]
					ont_anno <- lst[["value"]]
				} else {
					self$`@id` <- lst[["@id"]]
					ont_anno <- lst[["characteristicType"]]
				}
				self$category <- sub(
					"#characteristic_category/", "", self$`@id`, fixed = TRUE
				)
				if (is.null(lst[["unit"]])) {
					self$value <- OntologyAnnotation$new()
					self$value$from_list(
						ont_anno, recursive = recursive, json = json
					)
				} else {
					self$value <- lst[["value"]]
				}
				self$set_comments(lst[["comments"]])
			}
			# else {
			# 	private$id <- lst[["id"]]
			# 	if(recursive) {
			# 		self$category <- OntologyAnnotation$new()
			# 		self$category$from_list(lst[["category"]])
			# 	} else {
			# 		if(checkmate::test_r6(
			# 			lst[["category"]], "OntologyAnnotation"
			# 		)) {
			# 			stop(
			# 				"not a list contains raw OntologyAnnotation object"
			# 			)
			# 		} else if(is.null(lst[["category"]])) {
			# 			self$category <- NULL
			# 		} else {
			# 			self$category$term <- lst[["category"]]
			# 		}
			# 	}
			# 	self$value <- lst[["value"]]
			# 	if(recursive) {
			# 		self$unit <- Unit$new()
			# 		self$unit$from_list(lst[["unit"]])
			# 	} else {
			# 		if(checkmate::test_r6(
			# 			lst[["unit"]], "Unit"
			# 		)) {
			# 			stop("not a list contains raw Unit object")
			# 		} else if(is.null(lst[["unit"]])) {
			# 			self$unit <- NULL
			# 		} else {
			# 			self$unit$unit$term <- lst[["unit"]]
			# 		}
			# 	}
			# 	self$comments <- lst[["comments"]]
			# }
		},

		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		},
		#' @details
		#' set the uuid of this object
		#' @param id a uuid
		#' @param suffix a human readable suffix
		set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
			private$id <- generate_id(id, suffix)
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Characteristic"))
			green_bold_name_plain_content("category", self$category)
			green_bold_name_plain_content("ID", self$get_id())
			green_bold_name_plain_content("value", self$value$term)
			#green_bold_name_plain_content("unit", self$unit)
			pretty_print_comments(self$comments)
		}
	),
	private = list(
		id = generate_id()
	)
)

