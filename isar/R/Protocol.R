#' An R6 object a Protocol
#' @details
#' An experimental Protocol used in the study.
#'
#' @field name The name of the protocol used
#' @field protocol_type Term to classify the protocol.
#' @field description A free-text description of the protocol.
#' @field uri Pointer to protocol resources externally that can be accessed by their Uniform Resource Identifier (URI).
#' @field version An identifier for the version to ensure protocol tracking.
#' @field parameters A list of ProtocolParameter describing the list of required to execute the protocol.
#' @field components A list of \code{[OntologyAnnotation]} describing a protocol's components; e.g. instrument names, software names, and reagents names.
#' @field comments Comments associated with instances of this class.
Protocol <- R6::R6Class(
	"Protocol",
	public = list(
		name = character(),
		protocol_type = NULL,
		description = character(),
		uri = character(),
		version = character(),
		parameters = NULL,
		components = NULL,
		comments = NULL,
		`@id` = character(),

		#' @details
		#' Create a new Protocol object
		#' @param name The name of the protocol used
		#' @param protocol_type Term to classify the protocol.
		#' @param description A free-text description of the protocol.
		#' @param uri Pointer to protocol resources externally that can be accessed by their Uniform Resource Identifier (URI).
		#' @param version An identifier for the version to ensure protocol tracking.
		#' @param parameters A list of \code{[ProtocolParameter]}s describing the list of to execute the protocol.
		#' @param components A list of \code{[OntologyAnnotation]} describing a protocol's components; e.g. instrument names, software names, and reagents names.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = character(),
			protocol_type = NULL,
			description = character(),
			uri = character(),
			version = character(),
			parameters = NULL,
			components = NULL,
			comments = NULL,
			`@id` = NULL
		) {
			if (checkmate::qtest(name, "S[0]")) { self$name <- name } else {
				self$set_name(name)
			}
			self$protocol_type <- protocol_type
			if (checkmate::qtest(description, "S[0]")) {
				self$description <- description
			} else {
				self$set_description(description)
			}
			if (checkmate::qtest(uri, "S[0]")) { self$uri <- uri } else {
				self$set_uri(uri)
			}
			if (checkmate::qtest(version, "S[0]")) {
				self$version <- version
			} else {
				self$set_version(version)
			}
			if(is.null(parameters)) {
				self$parameters <- parameters
			} else {
				self$set_parameters(parameters)
			}
			if(is.null(parameters)) {
				self$components <- components
			} else {
				self$aet_components(components)
			}
			self$check_comments(comments)
			self$`@id` <- paste0("#protocol/", self$name)
		},
		#' @details
		#' Check that name is a single string
		#' @param name the name of the protocol
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set name if it passes validation
		#' @param name the name of the protocol
		set_name = function(name) {
			if(self$check_name(name)) { self$name <- name }
		},

		#' @details
		#' Check that description is a single string
		#' @param description the description of the protocol
		check_description = function(description) {
			check <- checkmate::check_string(description, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set description if it passes validation
		#' @param description the description of the protocol
		set_description = function(description) {
			if(self$check_description(description)) {
				self$description <- description
			}
		},

		#' @details
		#' Check that uri is a single string
		#' @param uri the uri of the protocol
		check_uri = function(uri) {
			check <- checkmate::check_string(uri, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set uri if it passes validation
		#' @param uri the uri of the protocol
		set_uri = function(uri) {
			if(self$check_uri(uri)) { self$uri <- uri }
		},

		#' @details
		#' Check that version is a single string
		#' @param version the version of the protocol
		check_version = function(version) {
			check <- checkmate::check_string(version, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set version if it passes validation
		#' @param version the version of the protocol
		set_version = function(version) {
			if(self$check_uri(version)) { self$version <- version }
		},

		#' @details
		#' Check that protocol_type is an \code{[OntologyAnnotation]} object
		#' @param protocol_type an \code{[OntologyAnnotation]} object
		check_protocol_type = function(protocol_type) {
			check <- checkmate::check_r6(protocol_type, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set protocol_type if input is valid
		#' @param protocol_type an \code{[OntologyAnnotation]} object
		set_protocol_type = function(protocol_type) {
			if(self$check_protocol_type(protocol_type)) {
				self$protocol_type <- protocol_type
			}
		},
		#' @details
		#' Set parameters if input is valid
		#' @param parameters a \code{[ProtcolParameter]} object
		check_parameters = function(parameters) {
			check <- checkmate::check_r6(parameters, "ProtcolParameter")
			error_with_check_message_on_failure(
				check, nextline = "Class: ProtcolParameter"
			)
		},
		#' @details
		#' Set parameters if input is valid
		#' @param parameters an \code{[ProtcolParameter]} object
		set_parameters = function(parameters) {
			if(self$check_parameters(parameters)) {
				self$check_parameter <- parameters
			}
		},
		#' @details
		#' check components is a list of \code{[OntologyAnnotation]} objects
		#' @param components a list of \code{[OntologyAnnotation]} objects
		check_components = function(components) {
			if(
				checkmate::test_list(components, min.len = 1) &&
				all(purrr::map_lgl(
					components, ~checkmate::test_r6(.x, "OntologyAnnotation")
				))
			) { return(TRUE) } else {
				stop("All components must be OntologyAnnotation objects")
			}
		},
		#' @details
		#' set components if components is a list of \code{[Study]} objects
		#' @param components a list of \code{[Study]} objects
		set_components = function(components) {
			if (self$check_studies(components)) {
				self$components <- components
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
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		to_list = function(ld = FALSE, recursive = TRUE){
			protocol <- list(
				"name" = self$name,
				"id" = private$id,
				"protocol_type" = switch(as.character(recursive),
					"TRUE" = self$protocol_type$to_list(),
					"FALSE" = self$protocol_type$term
				),
				"description" = self$description,
				"uri" = self$uri,
				"version" = self$version,
				"parameters" = switch(
					as.character(recursive),
					"TRUE" = purrr::map(self$parameters, ~.x$to_list()),
					"FALSE" = if (!is.null(self$components)) {
						purrr::map(self$parameters, ~.x$term)
					} else {NULL}
				),
				"components" = switch(
					as.character(recursive),
					"TRUE" = purrr::map(self$components, ~.x$to_list()),
					"FALSE" = if (!is.null(self$components)) {
						purrr::map(self$components, ~.x$parameter_name)
					} else {NULL}
				),
				"comments" = self$comments
			)
			return(protocol)
		},
		#' @details
		#' Make \code{[Protocol]} object from list
		#' @param lst an Protocol object serialized to a list
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		from_list = function(lst, recursive = TRUE, json = FALSE) {
			if(json) {
				self$name <- lst[["name"]]
				self$`@id` <- lst[["@id"]]
				if (recursive) {
					self$protocol_type <- OntologyAnnotation$new()
					self$protocol_type$from_list(lst[["protocolType"]])
				} else {
					self$protocol_type <- lst[["protocolType"]]
				}

				self$description <- lst[["description"]]
				self$uri <- lst[["uri"]]
				self$version <- lst[["version"]]

				self$parameters <- lst[["parameters"]]

				# if (recursive) {
				# 	self$parameters <- purrr::map(lst[["parameters"]], ~{
				# 		pv <- ParameterValue$new()
				# 		pv$from_list(.x)
				# 		pv
				# 	})
				# } else {
				# 	self$parameters <- lst[["parameters"]]
				# }

				if (recursive) {
					self$components <- purrr::map(lst[["components"]], ~{
						oa <- OntologyAnnotation$new()
						oa$from_list(.x)
						oa
					})
				} else {
					self$components <- lst[["components"]]
				}
				self$comments <- lst[["comments"]]
			} else {
				self$name <- lst[["name"]]
				private$id <- lst[["id"]]

				if (recursive) {
					self$protocol_type <- OntologyAnnotation$new()
					self$protocol_type$from_list(lst[["protocol_type"]])
				} else {
					self$protocol_type <- lst[["protocol_type"]]
				}

				self$description <- lst[["description"]]
				self$uri <- lst[["uri"]]
				self$version <- lst[["version"]]

				self$parameters <- lst[["parameters"]]

				if (recursive) {
					self$components <- purrr::map(lst[["components"]], ~{
						oa <- OntologyAnnotation$new()
						oa$from_list(.x)
						oa
					})
				} else {
					self$components <- lst[["components"]]
				}
				self$comments <- lst[["comments"]]
			}
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
		}
	),
	private = list(
		id = generate_id()
	)
)
#' identical.Protocol
#'
#' Allows checking for the identity of \code{[Protocol]} objects
#'
#' @param x a \code{[Protocol]} object
#' @param y a \code{[Protocol]} object
#' @export
identical.Protocol <- s3_identical_maker(c(
	"name",
	"id",
	"protocol_type",
	"description",
	"uri",
	"version",
	"parameters",
	"components",
	"comments"
))
