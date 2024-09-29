#' R6 class for an experimental factor value
#'
#' @details
#' [OntologyAnnotation]
#'
#' @field term the name of ontology term
#' @field term_source the ontology that is the source of the term represented by an [OntologySource] object
#' @field term_accession the unique identifier of the ontology term
#' @field comments comments
#' @field ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6 test_string
#' @importFrom purrr set_names
#' @importFrom cli cli_h1 col_blue
#'
#' @export
OntologyAnnotation <- R6::R6Class(
	"OntologyAnnotation",
	public = list(
		term = NULL, # str
		term_source = NULL, # OntologySource
		term_accession = NULL, # str
		comments = NULL, # comment
		ontology_source_references = NULL,
		`@id` = character(),
		#' @details
		#' create a new factor value
		#' @param term the name of ontology term
		#' @param term_source the ontology that is the source of the term represented by an [OntologySource] object.
		#' @param term_accession the unique identifier of the ontology term
		#' @param comments comments
		#' @param ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used.
		#' @param @id identifier
		#'
		#' @examples
		#' OA <- OntologyAnnotation$new()
		initialize = function(
			term = character(), # str
			term_source = NULL, # OntologySource
			term_accession = character(), # str
			comments = NULL,
			ontology_source_references = NULL,
			`@id` = character()
		) {
			# transition to character() !!
			if(is.null(ontology_source_references)){
				self$ontology_source_references <- OntologySourceReferences$new()
			} else {
				self$ontology_source_references <- ontology_source_references
			}
			if (
				test_empty(term, mode = "character", null.ok = TRUE) &&
				!test_empty(term_accession, mode = "character", null.ok = TRUE)
			) {
				self$set_term_accession(term_accession)
			}

			if (is.null(term_source)) {
				self$term_source <- term_source # OntologySource
			} else {
				self$set_term_source(term_source)
			}

			if (
				!test_empty(term, mode = "character", null.ok = TRUE) &&
				test_empty(term_accession, mode = "character", null.ok = TRUE)
			) {
				self$set_term(term)
			}

			if (
				!test_empty(term, mode = "character", null.ok = TRUE) &&
				!test_empty(term_accession, mode = "character", null.ok = TRUE)
			) {
				if (self$term_source$terms_list[[term]] != term_accession) {
					stop("Supplied term & term accession do not match!")
				} else {
					self$set_term(term)
				}
			}

			if (
				test_empty(term, mode = "character", null.ok = TRUE) &&
				test_empty(term_accession, mode = "character", null.ok = TRUE)
			) {
				term <- character()
				term_accession <- character()
			}
			#-self$set_term(term)
			self$`@id` <- `@id`
			# if(!is.null(term)) {
			# 	# handling on not explicitly enumerated lists?
			# 	# - check a remote resource with an API call?
			# 	if(term %in% names(self$term_source$terms_list)) {
			# 		self$term <- term # str
			# 		if (is.null(term_accession)) {
			# 			self$term_accession <- self$term_source$terms_list[[term]]
			# 		} else if (self$term_source$terms_list[[term]] != term_accession) {
			# 			stop("Supplied term & term accession do not match!")
			# 		}
			# 	} else {
			# 		stop("term is not in term source")
			# 	}
			# }
			# if(!is.null(term_accession)) {
			# 	if(term_accession %in% unlist(self$term_source$terms_list)) {
			# 		self$term_accession <- term_accession # str
			# 		term_vec <- unlist(self$term_source$terms_list)
			# 		if (is.null(term)) {
			# 			self$term <- names(term_vec[term_vec == term_accession])
			# 		} else if (names(term_vec[term_vec == term_accession]) != term) {
			# 			stop("Supplied term accession & term do not match!")
			# 		}
			# 	} else {
			# 		stop("term accession is not in the term source")
			# 	}
			# }
			self$set_comments(comments)
			# invisible(self)
		},

		#' @details
		#' Checks that the source of ontology terms is an [OntologySource] object
		#' @param term_source an [OntologySource] object
		check_term_source = function(term_source) {
			# browser()
			check <- checkmate::check_r6(term_source, "OntologySource")
			error_with_check_message_on_failure(check)
			#if(term_source$name %in% super$get_ontology_source_names()) {
			if(
				term_source$name %in%
				self$ontology_source_references$get_ontology_source_names()
			) {
				if(term_source$name == "UnknownSource") {
					# not provided previously generated
					warning("Using an automatically generated OntologySource")
					return(TRUE)
				} else {
					return(TRUE)
				}
			} else {
				warning(
					"OntologySource: '", term_source$name,
					"' is not in the ontology source reference!\n",
					"To silence this warning provide an",
					" OntologySourceReference which includes the",
					" OntologySource from which you wish to create an",
					" annotation\n",
					"Attempting to add it to the reference..."
				)
				src_to_add <- list(term_source)
				names(src_to_add) <- term_source$name
				self$ontology_source_references$add_ontology_sources(src_to_add)
				return(TRUE)
			}
		},

		#' @details
		#' Checks that the supplied term is in the list of valid terms from the ontology source object
		#' @param term an ontology term
		check_term = function(term) {
			if(term %in% names(self$term_source$terms_list)) {
				return(TRUE)
			} else {
				stop("term is not in term source")
			}
		},

		#' @details
		#' Checks that the supplied term accession is in the the list of valid accession terms from the ontology source object
		#' @param term_accession an accession for an ontology term
		check_term_accession = function(term_accession) {
			if(term_accession %in% unlist(self$term_source$terms_list)) {
				return(TRUE)
			} else {
				stop("term accession is not in the term source")
			}
		},

		#' @details
		#'
		#' Sets the value of term_source if it passes the checks
		#'
		#' @param term_source an [OntologySource] object
		set_term_source = function(term_source) {
			if(self$check_term_source(term_source)) {
				self$term_source <- term_source
			}
		},

		#' @details
		#' Sets the term and the term accession corresponding to that term if the term passes validity checks
		#' @param term an ontology term
		set_term = function(term) {
			if(self$check_term(term)) {
				self$term <- term
				self$term_accession <- self$term_source$terms_list[[term]]
			}
		},

		#' @details
		#' Sets the term accession and the term corresponding to that accession if the accession passes validity checks
		#' @param term_accession an accession for an ontology term
		set_term_accession = function(term_accession) {
			if(self$check_term_accession(term_accession)) {
				self$term_accession <- term_accession
				term_vec <- unlist(self$term_source$terms_list)
				self$term <- names(term_vec[term_vec == term_accession])
			}
		},

		#' @details
		#' Set the value of an ontology annotation checking that the term,
		#' accession, and source are mutually compatible and that the source
		#' is present in the ontology references
		#' @param term the ontology term
		#' @param term_accession the accession of the ontology term
		#' @param term_source_name the name of the source of the ontology term
		set_valid_annotation = function(term, term_accession, term_source_name) {
			term_accession <- switch(
				as.character(is.null(term_accession)),
				"TRUE" = "", "FALSE" = term_accession
			)

			if(is.null(term_source_name)) { term_source_name <- "" }
			#if(!checkmate::test_string(term, min.chars = 1)) {
			# if (!checkmate::test_string(term, n.chars = 0, null.ok = TRUE)) {
			# 	term <- "Unspecified Term"
			# 	warning("Unspecified Term!")
			# }
			if (is.null(term)) {
				term <- "Unspecified Term"
				warning("Unspecified Term!")
			}
			if(term == '') {
				term <- "Unspecified Term"
				warning("Unspecified Term!")
			}

			# if(!checkmate::test_r6(term_source,"OntologySource")) {
			# 	term_source <- list(name = term_source)
			# }

			# if(term_source$name %in% names(self$ontology_source_references)) {
			# 	self$term_source <- self$ontology_source_references[[
			# 		term_source$name
			# 	]]
			if(
				term_source_name %in%
				self$ontology_source_references$get_ontology_source_names()
			) {
				self$term_source <-
					self$ontology_source_references$ontology_source_references[[
						term_source_name
					]]
				# self$ontology_source_references$get_ontology_sources(
				# 	"UnknownSource"
				# )
			} else if(term_source_name != "") {
				warning(
					"Term Source Unknown, Attempting to add a placeholder..."
				)
				os <- OntologySource$new(
					name = term_source_name,
					terms_list = list(term_accession) %>%
						purrr::set_names(term),
					explicitly_provided = FALSE,
					source = "Unknown"
				)
				osl <- list(os) %>% purrr::set_names(term_source_name)
				self$ontology_source_references$add_ontology_sources(osl)
				self$term_source <-
					self$ontology_source_references$ontology_source_references[[
						term_source_name
					]]
			} else {
				warning(
					"Term Source Unknown, Attempting to add a placeholder..."
				)
				if(
					!"UnknownSource" %in%
					self$ontology_source_references$get_ontology_source_names()
				) {
					os <- OntologySource$new(
						name = "UnknownSource",
						explicitly_provided = FALSE,
						source = "Unknown"
					)
					osl <- list(UnknownSource = os)
					self$ontology_source_references$add_ontology_sources(osl)
				}
				self$term_source <-
					self$ontology_source_references$ontology_source_references[[
						"UnknownSource"
					]]
				# self$ontology_source_references$get_ontology_sources(
				# 	"UnknownSource"
				# )
			}
			if(term %in% names(self$term_source$terms)) {# get_?
				self$term <- term
				self$term_accession <- term_accession
			} else {
				#browser()
				warning("Term not in source! Attempting to add...")
				if (checkmate::test_string(term_accession, min.chars = 1)) {
					self$term_source$add_terms(
						purrr::set_names(list(term_accession), term)
					)
				} else {
					warning("Missing term accession! using term as accession...")
					self$term_source$add_terms(
						purrr::set_names(list(term), term)
					)
				}
				self$term <- term
				#self$set_term(term)
				#self$set_term_accession(term)
			}

		},
		# getters

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
		#' generate a tabular representation of an ontology annotation object
		#' @return a Tibble 
		to_table = function() {
			tibble::tibble_row(
				term = ifelse(
					self$term == "" || self$term == "Unspecified Term",
					NA_character_, self$term
				),
				source = ifelse(
					self$term_source$name == "" ||
						self$term_source$name == "UnknownSource",
					NA_character_, self$term_source$name
				),
				accession = ifelse(
					self$term_accession == "",
					NA_character_ , self$term_accession
				)
			)
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @param recursive call to_list methods of any objects within this object (default FALSE)
		to_list = function(ld = FALSE, recursive = TRUE) {
			lst <- list()

			if (private$null_accession) { } else if(
				self$term_source$name == "UnknownSource"
			) {
				lst[["termAccession"]] <- ""
			} else {
				lst[["termAccession"]] <- self$term_accession
			}

			lst[["annotationValue"]] <- self$term

			if (private$null_source) { } else if(
				self$term_source$name == "UnknownSource"
			) {
				lst[["termSource"]] <- ""
			} else {
				lst[["termSource"]] <- self$term_source$name
			}
			return(lst)
			# ontology_annotation = list(
			# 	"id" = private$id,
			# 	"annotation_value" = self$term,
			# 	"term_source" = switch(
			# 		as.character(recursive),
			# 		"TRUE" = self$term_source$to_list(),
			# 		"FALSE" = switch(
			# 			as.character(is.null(self$term_source)),
			# 			"TRUE" = NULL, "FALSE" = self$term_source$name
			# 		)
			# 	),
			# 	"term_accession" = self$term_accession,
			# 	"comments" = self$comments ## !!
			# )
			# return(ontology_annotation)
		},

		#' @details
		#' Make [OntologyAnnotation] from list
		#' @param lst an ontology source object serialized to a list
		#' @param recursive call to_list methods of any objects within this object (default FALSE)
		#' @param json json  (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				# remediate ontology annotation lists with missing members :(
				# such as: BII_I_1_jsonlite[["studies"]][[1]][["protocols"]][[1]][["protocolType"]]
				if(is.null(lst[["termAccession"]])) {
					lst[["termAccession"]] <- ""
					private$null_accession <- TRUE
				}
				if(is.null(lst[["termSource"]])) {
					lst[["termSource"]] <- ""
					private$null_source <- TRUE
				}
				# recursive?
				#self$term_source$name <- lst[["termSource"]]
				self$term_accession <- lst[["termAccession"]]
				self$term <- lst[["annotationValue"]]

				if(!is.null(lst[["comments"]])) {
					self$comments <- lst[["comments"]]
				}
				self$`@id` <- lst[["@id"]]

				self$set_valid_annotation(
					lst[["annotationValue"]], lst[["termAccession"]],
					lst[["termSource"]]
				)

			} else {
				# private$id <- lst[["id"]]
				self$term <- lst[["annotation_value"]]
				if(recursive) {
					self$term_source <- OntologySource$new()
					if(!is.null(lst[["term_source"]])) {
						self$term_source$from_list(lst[["term_source"]])
					}
				} else {
					if(checkmate::test_r6(
						lst[["term_source"]], "OntologySource"
					)) {
						stop("not a list contains raw OntologySource object")
					} else if(is.null(lst[["term_source"]])) {
						self$term_source <- NULL
					} else {
						self$term_source$name <- lst[["term_source"]]
					}
				}
				self$term_accession <- lst[["term_accession"]]
				self$comments <- lst[["comments"]]
			}
		},

		#' @details
		#' Pretty Prints [OntologyAnnotation] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Ontology Annotation"))
			green_bold_name_plain_content("Term", self$term)
			green_bold_name_plain_content("Term Accession", self$term_accession)
			green_bold_name_plain_content("Term Source", self$term_source$name)
			green_bold_name_plain_content("@id", self$`@id`)
			# green_bold_name_plain_content("ID", private$id)
			pretty_print_comments(self$comments)
		}
	),
	private = list(
		# record if in the input the source or accession where missing
		# entirely as opposed to empty so that this can be preserved in output
		#!! TODO add flag to make this optional? is it really a desirable
		# behaviour to not have have explicit nulls?
		null_accession = FALSE,
		null_source = FALSE
	)
)
