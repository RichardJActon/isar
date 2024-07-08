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
#' @field components A list of [OntologyAnnotation] describing a protocol's components; e.g. instrument names, software names, and reagents names.
#' @field comments Comments associated with instances of this class.
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate qtest check_string check_r6 test_list
#' @importFrom purrr map_lgl set_names map
#' @importFrom cli cli_h1 col_blue cli_h2 col_green cli_text cli_par cli_end cli_ul
#'
#' @export
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
		#' @param parameters A list of [ProtocolParameter]s describing the list of to execute the protocol.
		#' @param components A list of [OntologyAnnotation] describing a protocol's components; e.g. instrument names, software names, and reagents names.
		#' @param comments Comments associated with instances of this class.
		#' @param @id identifier
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
		#' Check that protocol_type is an [OntologyAnnotation] object
		#' @param protocol_type an [OntologyAnnotation] object
		check_protocol_type = function(protocol_type) {
			check <- checkmate::check_r6(protocol_type, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set protocol_type if input is valid
		#' @param protocol_type an [OntologyAnnotation] object
		set_protocol_type = function(protocol_type) {
			if(self$check_protocol_type(protocol_type)) {
				self$protocol_type <- protocol_type
			}
		},
		#' @details
		#' Set parameters if input is valid
		#' @param parameters a [ProtcolParameter] object
		check_parameters = function(parameters) {
			check <- checkmate::check_r6(parameters, "ProtcolParameter")
			error_with_check_message_on_failure(
				check, nextline = "Class: ProtcolParameter"
			)
		},
		#' @details
		#' Set parameters if input is valid
		#' @param parameters an [ProtcolParameter] object
		set_parameters = function(parameters) {
			if(self$check_parameters(parameters)) {
				self$check_parameter <- parameters
			}
		},
		#' @details
		#' check components is a list of [OntologyAnnotation] objects
		#' @param components a list of [OntologyAnnotation] objects
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
		#' set components if components is a list of [Study] objects
		#' @param components a list of [Study] objects
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
		#' An R list representation of a [Protocol] object
		#' @param ld linked data (default FALSE)
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		to_list = function(ld = FALSE, recursive = TRUE){
			lst <- list()
			lst[["parameters"]] <- if(!is.null(self$parameters)) {
				self$parameters %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else { list() }
			lst[["components"]] <- if (!is.null(self$components)) {
				purrr::map(self$components, ~.x$to_list())
			} else { list() }
			lst[["uri"]] <- self$uri
			lst[["description"]] <- self$description
			lst[["version"]] <- self$version
			lst[["@id"]] <- self$`@id`
			lst[["name"]] <- self$name

			# weird inconsistencies with missing data in the json output?
			ptl <- self$protocol_type$to_list()
			# if(ptl[["termAccession"]] == "") {
			# 	ptl <- ptl[names(ptl) != "termAccession"]
			# }
			# if(ptl[["termSource"]] == "") {
			# 	ptl <- ptl[names(ptl) != "termSource"]
			# }
			if(ptl[["annotationValue"]] == "Unspecified Term") {
				ptl[["annotationValue"]] <- ""
			}
			lst[["protocolType"]] <- ptl

			return(lst)

			# protocol <- list(
			# 	"name" = self$name,
			# 	"id" = private$id,
			# 	"protocol_type" = switch(as.character(recursive),
			# 		"TRUE" = self$protocol_type$to_list(),
			# 		"FALSE" = self$protocol_type$term
			# 	),
			# 	"description" = self$description,
			# 	"uri" = self$uri,
			# 	"version" = self$version,
			# 	"parameters" = switch(
			# 		as.character(recursive),
			# 		"TRUE" = purrr::map(self$parameters, ~.x$to_list()),
			# 		"FALSE" = if (!is.null(self$components)) {
			# 			purrr::map(self$parameters, ~.x$term)
			# 		} else {NULL}
			# 	),
			# 	"components" = switch(
			# 		as.character(recursive),
			# 		"TRUE" = purrr::map(self$components, ~.x$to_list()),
			# 		"FALSE" = if (!is.null(self$components)) {
			# 			purrr::map(self$components, ~.x$parameter_name)
			# 		} else {NULL}
			# 	),
			# 	"comments" = self$comments
			# )
			# return(protocol)
		},
		#' @details
		#' Make [Protocol] object from list
		#' @param lst an Protocol object serialized to a list
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		#' @param json json  (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				self$name <- lst[["name"]]
				self$`@id` <- lst[["@id"]]
				if (recursive) {
					self$protocol_type <- OntologyAnnotation$new(
						ontology_source_references =
							self$ontology_source_references
					)
					self$protocol_type$from_list(
						lst[["protocolType"]], json = json
					)
				} else {
					self$protocol_type <- lst[["protocolType"]]
				}

				self$description <- lst[["description"]]
				self$uri <- lst[["uri"]]
				self$version <- lst[["version"]]

				#self$parameters <- lst[["parameters"]]

				if (!checkmate::test_list(lst[["parameters"]], len = 0)) {
					self$parameters <-
						lst[["parameters"]] %>%
						purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
						purrr::map(~{
							pp <- ProtocolParameter$new(
								ontology_source_references =
									self$ontology_source_references
							)
							pp$from_list(.x)
							pp
							# pv <- ParameterValue$new()
							# pv$from_list(.x)
							# pv
						})
				} #else {
				# 	self$parameters <- lst[["parameters"]]
				# }

				if (!checkmate::test_list(lst[["components"]], len = 0)) {
					self$components <- purrr::map(lst[["components"]], ~{
						oa <- OntologyAnnotation$new(
							ontology_source_references =
								self$ontology_source_references
						)
						oa$from_list(.x, json = json)
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
					self$protocol_type <- OntologyAnnotation$new(
						ontology_source_references =
							self$ontology_source_references
					)
					self$protocol_type$from_list(
						lst[["protocol_type"]], json = json
					)
				} else {
					self$protocol_type <- lst[["protocol_type"]]
				}

				self$description <- lst[["description"]]
				self$uri <- lst[["uri"]]
				self$version <- lst[["version"]]

				self$parameters <- lst[["parameters"]]

				if (recursive) {
					self$components <- purrr::map(lst[["components"]], ~{
						oa <- OntologyAnnotation$new(
							ontology_source_references =
								self$ontology_source_references
						)
						oa$from_list(.x, json = json)
						oa
					})
				} else {
					self$components <- lst[["components"]]
				}
				self$comments <- lst[["comments"]]
			}
		},

		# #' @details
		# #' Get the uuid of this object
		# #' @return a uuid
		# get_id = function() {
		# 	private$id
		# },
		# #' @details
		# #' set the uuid of this object
		# #' @param id a uuid
		# #' @param suffix a human readable suffix
		# set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
		# 	private$id <- generate_id(id, suffix)
		# },

		#' @details
		#' Pretty Prints [Protocol] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Protocol 📋"))

			green_bold_name_plain_content("name", self$name)
			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("uri", self$uri)
			green_bold_name_plain_content("version", self$version)
			green_bold_name_plain_content("protocol_type", self$protocol_type$term)
			cli::cli_h2(cli::col_green("Description"))
			cli::cli_text(self$description)
			cli::cli_par()
			cli::cli_end()

			#
			cli::cli_h2(cli::col_green("Parameters"))
			# needs cleaning up list not from isajson
			if (!checkmate::test_list(self$parameters, len = 0)) {
				cli::cli_ul(purrr::map_chr(
					self$parameters, ~.x$parameter_name$term
				))
			}
			# model a param values of ont annotations
			#green_bold_name_plain_content("parameters", self$parameters)

			cli::cli_h2(cli::col_green("Components"))
			if (!checkmate::test_list(self$components, len = 0)) {
				cli::cli_ul(purrr::map_chr(self$components, ~.x$term))
			}
			pretty_print_comments(self$comments)
		}
	)#  ,
	# private = list(
	# 	id = generate_id()
	# )
)
