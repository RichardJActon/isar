#' R6 class for a Publication
#'
#' A publication associated with an investigation or study.
#'
#' possible use of  https://docs.ropensci.org/rcrossref/index.html
#'
#' consider a strict mode where the ID must be able to be identified in suitable databases
#'
#' @field pubmed_id The PubMed IDs of the described publication(s) associated with this investigation.
#' @field doi A Digital Object Identifier (DOI) for that publication (where available).
#' @field author_list The list of authors associated with that publication.
#' @field title The title of publication associated with the investigation.
#' @field status A term describing the status of that publication (i.e.submitted, in preparation, published).
#' @field comments Comments associated with instances of this class.
#' @field ontology_source_references an [OntologySourceReferences] object
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate qtest test_list test_r6
#' @importFrom purrr map_lgl
#'
#' @export
Publication <- R6::R6Class(
	"Publication",
	list(
		pubmed_id = NULL, # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
		doi = character(),
		author_list = NULL,
		title = character(),
		status = NULL, #  https://sparontologies.github.io/pso/current/pso.html
		comments = NULL,
		ontology_source_references = NULL,
		#' @details
		#' Create a new [Publication Object
		#' @param pubmed_id pubmed_id The PubMed IDs of the described publication(s) associated with this investigation.
		#' @param doi doi A Digital Object Identifier (DOI) for that publication (where available).
		#' @param author_list author_list The list of authors associated with that publication.
		#' @param title title The title of publication associated with the investigation.
		#' @param status status A term describing the status of that publication (i.e.submitted, in preparation, published).
		#' @param comments comments Comments associated with instances of this class.
		#' @param ontology_source_references an [OntologySourceReferences] object
		initialize = function(
			pubmed_id = NULL, # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
			doi = character(),
			author_list = NULL,
			title = character(),
			status = NULL, #  https://sparontologies.github.io/pso/current/pso.html
			comments = NULL,
			ontology_source_references = NULL
		) {
			if(is.null(pubmed_id)) { self$pubmed_id <- pubmed_id } else { self$set_pubmed_id(pubmed_id) }
			if(checkmate::qtest(doi, "S[0]")) { self$doi <- doi } else { self$set_doi(doi) }
			if(is.null(author_list)) { self$author_list <- author_list } else { self$set_author_list(author_list) }
			if(checkmate::qtest(title, "S[0]")) { self$title <- title } else { self$set_title(title) }
			self$status <- status
			self$comments <- comments
			self$ontology_source_references <- ontology_source_references
		},
		#' @details
		#' Check for a valid PubMed ID
		#' @param pubmed_id an identifier from the PubMed database
		check_pubmed_id = function(pubmed_id) {
			# Allow PMC? - this should not be allowed these are different identifiers
			# but for some reason this is in the spec https://isa-specs.readthedocs.io/en/latest/isajson.html#content-rules

			pmidint <- as.integer(pubmed_id)
			if(is.na(pmidint)) {
				stop("PUBMED ID is not an integer and cannot be coerced to one!")
			} else { return(TRUE) }

			# if(is.integer(pubmed_id)) { return(TRUE) } else {
			# 	stop("PUBMED IDs must be integers!")
			# }
		},
		#' @details
		#' Set PubMed ID if valid
		#' @param pubmed_id an identifier from the PubMed database
		set_pubmed_id = function(pubmed_id) {
			# tryCatch(
			# 	self$check_pubmed_id(pubmed_id),
			# 	error = function(res) {
			# 		pmidint <- as.integer(pubmed_id)
			# 		if(is.na(pmidint)) {
			# 			stop("PUBMED ID is not an integer and cannot be coerced to one!")
			# 		} else {
			# 			warning(res)
			# 			self$pubmed_id <- pmidint
			# 		}
			# 	}
			# )

			if (self$check_pubmed_id(pubmed_id)) {
				self$pubmed_id <- as.integer(pubmed_id)
			}
		},
		#' @details
		#' Check for a valid DOI (Digital Object Identifier)
		#' @param doi a valid DOI (Digital Object Identifier)
		check_doi = function(doi) {
			#doi_regex = '^(10[.][0-9]{2,}(?:[.][0-9]+)*/(?:(?![%"#? ])\\S)+)$'
			# allow 'doi:' prefix
			doi_regex = '^(?:doi:)?(10[.][0-9]{2,}(?:[.][0-9]+)*/(?:(?![%"#? ])\\S)+)$'
			# https://github.com/regexhq/doi-regex/blob/main/index.js
			if(grepl(doi_regex, doi, perl = TRUE)) {
				return(TRUE)
			} else if (doi == "") {
				warning("Empty DOI!")
				return(TRUE)
			} else {
				stop("Invalid DOI!")
			}
		},
		#' @details
		#' Set the DOI (Digital Object Identifier) if valid
		#' @param doi a valid DOI (Digital Object Identifier)
		set_doi = function(doi) {
			if(self$check_doi(doi)) { self$doi <- doi }
		},
		#' @details
		#' Check that all Authors listed are represented with [Person objects
		#' @param author_list A list of authors of the Investigation or Study (list of Person objects)
		check_author_list = function(author_list) {
			if(
				checkmate::test_list(author_list, min.len = 1) &&
				all(purrr::map_lgl(
					author_list, ~checkmate::test_r6(.x, "Person")
				))
			) { return(TRUE) } else if (checkmate::test_string(author_list)) {
				## !!! strict mode which stops this?
				return(TRUE)
			} else {
				#stop("All authors must be 'Person' objects")
				stop("authors must be a single string or a list of 'Person' objects")
			}
		},
		#' @details
		#' Set the list of authors of the Investigation or Study if they are valid
		#' @param author_list a list of authors
		set_author_list = function(author_list) {
			if(self$check_author_list(author_list)) {
				self$author_list <- author_list
			}
		},
		#' @details
		#' Check the the title has a non-zero length
		#' @param title of the publication
		check_title = function(title) {
			check <- checkmate::check_string(title, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set the title of the [Publication
		#' @param title of the publication
		set_title = function(title) {
			if (self$check_title(title)) { self$title <- title }
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
		#' convert the Publication object to a list
		#' @return list
		to_list = function() {
			lst <- list(
				"doi" = self$doi,
				"pubMedID" = as.character(self$pubmed_id), # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
				"title" = self$title,
				"authorList" = self$author_list#,
				#"comments" = self$comments
			)
			# if(!is.null(self$author_list)) {
			# 	publication[["author_list"]] <- self$author_list$to_list()
			# }
			if(is.null(self$status)) {
				lst[["status"]] <- self$status
			} else {
				lst[["status"]] <- self$status$to_list()
			}
			#  https://sparontologies.github.io/pso/current/pso.html
			return(lst)
		},
		#' @details
		#' Generate a [Publication object from a list
		#' @param lst a list suitable for conversion to a Publication Object
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				self$set_pubmed_id(lst[["pubMedID"]])
				self$set_doi(lst[["doi"]])
				# if (recursive) {
				# 	self$author_list <- purrr::map(lst["authorList"], ~{
				# 		a <- Person$new(
				# 			ontology_source_references =
				# 				self$ontology_source_references
				# 		)
				# 		a$from_list(.x)
				# 	})
				# } else {
				# 	self$author_list <- lst[["authorList"]]
				# }
				self$author_list <- lst[["authorList"]]
				self$title <- lst[["title"]]

				self$status <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
				self$status$from_list(lst[["status"]])

				self$set_comments(lst[["comments"]])
			} else {
				self$set_pubmed_id(lst[["pubmed_id"]])
				self$set_doi(lst[["doi"]])
				# if (recursive) {
				# 	self$author_list <- purrr::map(lst["author_list"], ~{
				# 		a <- Person$new(
				# 			ontology_source_references =
				# 				self$ontology_source_references
				# 		)
				# 		a$from_list(.x)
				# 	})
				# } else {
				# 	self$author_list <- lst[["author_list"]]
				# }
				self$author_list <- lst[["author_list"]]
				self$title <- lst[["title"]]
				self$status <- lst[["status"]]
				self$set_comments(lst[["comments"]])
			}
		},

		#' @details
		#' Pretty prints [Publication] objects
		#' @return none
		print = function() {
			cli::cli_h1(cli::col_blue("Publication 📖️️"))
			green_bold_name_plain_content("Title", self$title)
			green_bold_name_plain_content("pubmed id", self$pubmed_id)
			green_bold_name_plain_content("DOI", self$doi)
			pretty_print_comments(self$comments)
		}
	)
)
