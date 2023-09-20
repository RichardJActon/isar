#' R6 object for Process
#' @details
#' Process nodes represent the application of a protocol to some input material (e.g. a Source) to produce some output (e.g.a Sample).
#' @field name If relevant, a unique name for the process to disambiguate it from other processes.
#' @field executes_protocol A reference to the Protocol that this process executes.
#' @field date A date formatted as an ISO8601 string corresponding to when the process event occurred.
#' @field performer The name of the person or organisation that carried out the process.
#' @field parameter_values A list of ParameterValues relevant to the executing protocol.
#' @field inputs A list of input materials, possibly Sources, Samples, Materials, DataFiles
#' @field outputs A list of output materials, possibly Samples, Materials, DataFiles
#' @field comments Comments associated with instances of this class.
Process <- R6::R6Class(
	"Process",
	public = list(
		name = character(),
		executes_protocol = NULL,
		date = NULL,
		performer = NULL,
		parameter_values = NULL,
		inputs = NULL,
		outputs = NULL,
		comments = NULL,
		`@id` = NULL,
		#' @details
		#' Create a new \code{[Process]}
		#' @param name If relevant, a unique name for the process to disambiguate it from other processes.
		#' @param executes_protocol A reference to the Protocol that this process executes.
		#' @param date A date formatted as an ISO8601 string corresponding to when the process event occurred.
		#' @param performer The name of the person or organisation that carried out the process.
		#' @param parameter_values A list of ParameterValues relevant to the executing protocol.
		#' @param inputs A list of input materials, possibly Sources, Samples, Materials, DataFiles
		#' @param outputs A list of output materials, possibly Samples, Materials, DataFiles
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = character(),
			executes_protocol = NULL,
			date = NULL,
			performer = NULL,
			parameter_values = NULL,
			inputs = NULL,
			outputs = NULL,
			comments = NULL,
			`@id` = NULL
		) {
			self$name <- name
			self$executes_protocol <- executes_protocol
			self$date <- date
			self$performer <- performer
			self$parameter_values <- parameter_values
			self$inputs <- inputs
			self$outputs <- outputs
			self$comments <- comments
			self$`@id` <- `@id`
		},
		#' @details
		#' Check the the name has a non-zero length
		#' @param name of the process
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set the name of the \code{[Process]}
		#' @param name of the process
		set_name = function(name) {
			if (self$check_name(name)) { self$name <- name }
		},
		#' @details
		#' check performer is a list of \code{[Person]} objects
		#' @param performer a list of \code{[Person]} objects
		check_performer = function(performer) {
			if(
				checkmate::test_list(performer, min.len = 1) &&
				all(
					purrr::map_lgl(performer, ~checkmate::test_r6(.x, "Person"))
				)
			) { return(TRUE) } else {
				stop("All performer must be Person objects")
			}
		},
		#' @details
		#' Check date is a Date object
		#' @param date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		check_date = function(date) {
			if (is.character(date)) {
				date <- as.Date.character(date, tryFormats = c("%Y-%m-%d"))
			} else {
				check <- checkmate::check_date(date)
				error_with_check_message_on_failure(check)
			}
		},
		#' @details
		#' Set date to a Date object
		#' @param date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		set_date = function(date) {
			if(self$check_submission_date(date)) { self$date <- date }
		},
		#' @details
		#' set performer if performer is a list of \code{[Person]} objects
		#' @param performer a list of \code{[Person]} objects
		set_performer = function(performer) {
			if (self$check_performer(performer)) { self$performer <- performer }
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
		#' An R list representation of a \code{[Process]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE){
			process <- list(
				"name" = self$name,
				"executes_protocol" = self$executes_protocol,
				"date" = self$date,
				"performer" = purrr::map(self$performer, ~.x$to_list),
				"parameter_values" = self$parameter_values,
				"inputs" = self$inputs,
				"outputs" = self$outputs,
				"comments" = self$comments
			)
			if(ld) {
				process$`@id` <- self$`@id`
			}
			return(process)
		},
		#' @details
		#' Make \code{[Process]} object from list
		#' @param lst an Process object serialized to a list
		from_list = function(lst, recursive = TRUE, json = FALSE) {
			if(json) {
				self$executes_protocol <- lst[["executesProtocol"]][["@id"]]
				self$parameter_values <- lst[["parameterValues"]]
				self$outputs <- purrr::map_chr(lst[["outputs"]], ~.x$`@id`)
				self$inputs <- purrr::map_chr(lst[["inputs"]], ~.x$`@id`)
			} else {
				self$executes_protocol <- lst[["executes_protocol"]] # protocol object
				self$parameter_values <- lst[["parameter_values"]] # ont anno?
				self$outputs <- lst[["outputs"]] # sample obj ?
				self$inputs <- lst[["inputs"]] # source obj
			}
			self$name <- lst[["name"]]
			self$date <- lst[["date"]]
			if(recursive && !json) {
				self$performer <- purrr::map(lst[["performer"]], ~{
					p <- Person$new()
					p$from_list(.x)
					p
				})
			} else {
				self$performer <- lst[["performer"]]
			}
			self$`@id` <- lst[["@id"]]
			self$comments <- lst[["comments"]]
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
			cli::cli_h1(cli::col_blue("Process ⚙️"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("Date", self$date)
			green_bold_name_plain_content("@id", self$`@id`)
			if (checkmate::test_r6(self$performer, "Person")) {
				green_bold_name_plain_content(
					"Performer", self$performer$get_full_name()
				)
			} else {
				green_bold_name_plain_content("Performer", self$performer)
			}
			cli::cli_h2("Inputs")
			cli::cli_ul(self$inputs)

			cli::cli_h2("Outputs")
			cli::cli_ul(self$outputs)

			if(checkmate::test_r6(self$executes_protocol, "Protocol")){
				green_bold_name_plain_content(
					"Executes Protocol", self$executes_protocol$name
				)
			} else {
				green_bold_name_plain_content(
					"Executes Protocol", self$executes_protocol
				)
			}

			cli::cli_h2("Parameter Values")
			if(checkmate::test_r6(self$parameter_values, "ParameterValue")) {
				self$parameter_values$get_value_in_units()
			} else {
				cli::cli_ul(unlist(self$parameter_values))
			}

			pretty_print_comments(self$comments)
		}
	),
	private = list(
		id = generate_id()
	)
)

#' identical.Process
#'
#' Allows checking for the identity of \code{[Process]} objects
#'
#' @param x a \code{[Process]} object
#' @param y a \code{[Process]} object
#' @export
identical.Process <- s3_identical_maker(c(
	"name",
	"executes_protocol",
	"date",
	"performer",
	"parameter_values",
	"inputs",
	"outputs",
	"comments"
))
