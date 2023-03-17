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
		#' @details
		#' Create a new \code{[Publication]} Object
		#' @param pubmed_id pubmed_id The PubMed IDs of the described publication(s) associated with this investigation.
		#' @param doi doi A Digital Object Identifier (DOI) for that publication (where available).
		#' @param author_list author_list The list of authors associated with that publication.
		#' @param title title The title of publication associated with the investigation.
		#' @param status status A term describing the status of that publication (i.e.submitted, in preparation, published).
		#' @param comments comments Comments associated with instances of this class.
		initialize = function(
			pubmed_id = NULL, # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
			doi = character(),
			author_list = NULL,
			title = character(),
			status = NULL, #  https://sparontologies.github.io/pso/current/pso.html
			comments = NULL
		) {
			if(is.null(pubmed_id)) { self$pubmed_id <- pubmed_id } else { self$set_pubmed_id(pubmed_id) }
			if(checkmate::qtest(doi, "S[0]")) { self$doi <- doi } else { self$set_doi(doi) }
			if(is.null(author_list)) { self$author_list <- author_list } else { self$set_author_list(author_list) }
			if(checkmate::qtest(title, "S[0]")) { self$title <- title } else { self$set_title(title) }
			self$status <- status
			self$comments <- comments
		},
		#' @details
		#' Check for a valid PubMed ID
		#' @param pubmed_id an identifier from the PubMed database
		check_pubmed_id = function(pubmed_id) {
			if(is.integer(pubmed_id)) { return(TRUE) } else {
				stop("PUBMED IDs must be integers!")
			}
		},
		#' @details
		#' Set PubMed ID if valid
		#' @param pubmed_id an identifier from the PubMed database
		set_pubmed_id = function(pubmed_id) {
			if (self$check_pubmed_id(pubmed_id)) {
				self$pubmed_id <- pubmed_id
			}
		},
		#' @details
		#' Check for a valid DOI (Digital Object Identifier)
		#' @param doi a valid DOI (Digital Object Identifier)
		check_doi = function(doi) {
			doi_regex = '^(10[.][0-9]{2,}(?:[.][0-9]+)*/(?:(?![%"#? ])\\S)+)$'
			# https://github.com/regexhq/doi-regex/blob/main/index.js
			if(grepl(doi_regex, doi, perl = TRUE)) { return(TRUE) } else {
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
		#' Check that all Authors listed are represented with \code{[Person]} objects
		#' @param author_list A list of authors of the Investigation or Study (list of Person objects)
		check_author_list = function(author_list) {
			if(
				checkmate::test_list(author_list, min.len = 1) &&
				all(
					purrr::map_lgl(author_list, ~checkmate::test_r6(.x, "Person"))
				)
			) { return(TRUE) } else {
				stop("All authors must be 'Person' objects")
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
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#' Set the title of the \code{[Publication]}
		#' @param title of the publication
		set_title = function(title) {
			if (self$check_title(title)) { self$title <- title }
		},
		#' @details
		#' convert the Publication object to a list
		#' @param ld linked data
		to_list = function(ld = FALSE) {
			publication = list(
				"pubmed_id" = self$pubmed_id, # https://www.ncbi.nlm.nih.gov/pmc/tools/id-converter-api/
				"doi" = self$doi,
				"author_list" = NULL,
				"title" = self$title,
				"status" = self$status, #  https://sparontologies.github.io/pso/current/pso.html
				"comments" = self$comments
			)
			if(!is.null(self$author_list)) {
				publication[["author_list"]] <- self$author_list$to_list()
			}
			return(publication)
		},
		#' @details
		#' Generate a \code{[Publication]} object from a list
		#' @param lst a list suitable for conversion to a Publication Object
		from_list = function(lst) {
			self$pubmed_id <- lst[["pubmed_id"]]
			self$doi <- lst[["doi"]]
			self$author_list <- lst[["author_list"]]
			self$author_list <- purrr::map(lst["author_list"], ~{
				a <- Person$new()
				a$from_list(.x)
			})
			self$title <- lst[["title"]]
			self$status <- lst[["status"]]
			self$comments <- lst[["comments"]]
		}
	)
)
