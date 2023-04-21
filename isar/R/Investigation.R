#' R6 class for Investigation
#'
#' Represents an investigation from the ISA model
#'
#' @details
#' An investigation maintains metadata about the project context and links to one or more studies. There can only be 1 Investigation in an ISA descriptor.
#'
# #' @field id ...
#' @field filename ...
# #' @field identifier A locally unique identifier or an accession number provided by a repository.
#' @field title A concise name given to the investigation.
#' @field description A textual description of the investigation.
#' @field submission_date date on which the investigation was reported to the repository. This should be ISO8601 formatted.
#' @field public_release_date The date on which the investigation should be released publicly. This should be ISO8601 formatted.
#' @field ontology_source_references \code{[OntologySources]} to be referenced by \code{[OntologyAnnotations]} used in this ISA descriptor.
#' @field publications a list of \code{[Publication]} objects
#' @field contacts A list of People/contacts associated with an Investigation.
#' @field studies \code{[Study]} is the central unit, containing information on the subject under study.
#' @field comments comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_date
#'
#' @export
Investigation <- R6::R6Class(
	"Investigation",
	public = list(
		filename = '',
		# identifier = '',
		title = character(),
		description = character(),
		submission_date = NULL,
		public_release_date = NULL,
		ontology_source_references = NULL,
		publications = NULL,
		contacts = NULL,
		studies = NULL,
		comments = NULL,

		#' @details
		#' Create a new investigation object
		#' @param filename ...
		# #' @param identifier A locally unique identifier or an accession number provided by a repository.
		#' @param title A concise name given to the investigation.
		#' @param description A textual description of the investigation.
		#' @param submission_date date on which the investigation was reported to the repository. This should be ISO8601 formatted.
		#' @param public_release_date The date on which the investigation should be released publicly. This should be ISO8601 formatted.
		#' @param ontology_source_references ontology_source_references \code{[OntologySource]}s to be referenced by \code{[OntologyAnnotation]}s used in this ISA descriptor.
		#' @param publications publications A list of Publications associated with an Investigation. (format?)
		#' @param contacts contacts A list of People/contacts associated with an Investigation.
		#' @param studies studies \code{[Study]} is the central unit, containing information on the subject under study.
		#' @param comments comments comments associated with instances of this class.
		#'
		initialize = function(
			filename = '',
			title = '',
			description = '',
			submission_date = NULL,
			public_release_date = NULL,
			ontology_source_references = NULL,
			publications = NULL,
			contacts = NULL,
			studies = NULL,
			comments = NULL
		) {
			self$filename <- filename
			# self$identifier <- identifier

			if (checkmate::qtest(title, "S[0]")) {
				self$title <- title
			} else {
				self$set_title(title)
			}
			if (checkmate::qtest(description, "S[0]")) {
				self$description <- description
			} else {
				self$set_description(description)
			}

			if (is.null(submission_date)) {
				self$submission_date <- submission_date
			} else {
				self$set_submission_date(submission_date)
			}
			if (is.null(public_release_date)) {
				self$public_release_date <- public_release_date
			} else {
				self$set_public_release_date(public_release_date)
			}
			# may need to be private and inferred from child structures?
			self$ontology_source_references <- ontology_source_references
			if(is.null(publications)) {
				self$publications <- publications
			} else {
				self$set_publications(publications)
			}
			if(is.null(contacts)) { self$contacts <- contacts } else {
				self$set_contacts(contacts)
			}
			if(is.null(studies)) { self$studies <- studies } else {
				self$set_studies(studies)
			}
			self$comments <- comments
		},
		#' @details
		#' Check if the title of the investigation is a string
		#' @param title The title of the investigation
		check_title = function(title) {
			check <- checkmate::check_string(title, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the title of the investigation if valid
		#' @param title The title of the investigation
		set_title = function(title) {
			if (self$check_title(title)) { self$title <- title }
		},
		#' @details
		#' Check if the description of the investigation is a string
		#' @param description The description of the investigation
		check_description = function(description) {
			check <- checkmate::check_string(description, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the description of the investigation if valid
		#' @param description The description of the investigation
		set_description = function(description) {
			if (self$check_description(description)) {
				self$description <- description
			}
		},
		#' @details
		#' check studies is a list of \code{[Study]} objects
		#' @param studies a list of \code{[Study]} objects
		check_studies = function(studies) {
			if(
				checkmate::test_list(studies, min.len = 1) &&
				all(
					purrr::map_lgl(studies, ~checkmate::test_r6(.x, "Study"))
				)
			) { return(TRUE) } else {
				stop("All studies must be Study objects")
			}
		},
		#' @details
		#' set studies if studies is a list of \code{[Study]} objects
		#' @param studies a list of \code{[Study]} objects
		set_studies = function(studies) {
			if (self$check_studies(studies)) { self$studies <- studies }
		},

		#' @details
		#' check contacts is a list of \code{[Person]} objects
		#' @param contacts a list of \code{[Person]} objects
		check_contacts = function(contacts) {
			if(
				checkmate::test_list(contacts, min.len = 1) &&
				all(
					purrr::map_lgl(contacts, ~checkmate::test_r6(.x, "Person"))
				)
			) { return(TRUE) } else {
				stop("All contacts must be Person objects")
			}
		},
		#' @details
		#' set contacts if contacts is a list of \code{[Person]} objects
		#' @param contacts a list of \code{[Person]} objects
		set_contacts = function(contacts) {
			if (self$check_contacts(contacts)) { self$contacts <- contacts }
		},

		#' @details
		#' check publications is a list of \code{[Publication]} objects
		#' @param publications a list of \code{[Publication]} objects
		check_publications = function(publications) {
			if(
				checkmate::test_list(publications, min.len = 1) &&
				all(purrr::map_lgl(
					publications, ~checkmate::test_r6(.x, "Publication")
				))
			) { return(TRUE) } else {
				stop("All publications must be Publication objects")
			}
		},
		#' @details
		#' Check publications is a list of \code{[Publication]} objects
		#' @param publications a list of \code{[Publication]} objects
		set_publications = function(publications) {
			if (self$check_contacts(publications)) {
				self$publications <- publications
			}
		},

		#' @details
		#' Check public_release_date is a Date object
		#' @param public_release_date  a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		check_public_release_date = function(public_release_date) {
			if (is.character(public_release_date)) {
				public_release_date <- as.Date.character(
					public_release_date, tryFormats = c("%Y-%m-%d")
				)
			} else {
				check <- checkmate::check_date(public_release_date)
				error_with_check_message_on_failure(check)
			}
		},
		#' @details
		#' Set public_release_date to a Date object
		#' @param public_release_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		set_public_release_date = function(public_release_date) {
			if(self$check_public_release_date(public_release_date)) {
				self$public_release_date <- public_release_date
			}
		},

		#' @details
		#' Check submission_date is a Date object
		#' @param submission_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		check_submission_date = function(submission_date) {
			if (is.character(submission_date)) {
				submission_date <- as.Date.character(
					submission_date, tryFormats = c("%Y-%m-%d")
				)
			} else {
				check <- checkmate::check_date(submission_date)
				error_with_check_message_on_failure(check)
			}
		},
		#' @details
		#' Set submission_date to a Date object
		#' @param submission_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		set_submission_date = function(submission_date) {
			if(self$check_submission_date(submission_date)) {
				self$submission_date <- submission_date
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
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		to_list = function(ld = FALSE) {
			investigation = list(
				"filename" = self$filename,
				"id" = private$id,
				"title" = self$title,
				"description" = self$description,
				"submission_date" = self$submission_date,
				"public_release_date" = self$public_release_date,
				"ontology_source_references" = self$ontology_source_references,
				"publications" = self$publications$to_list(),
				"contacts" = self$contacts$to_list(),
				"studies" = self$studies$to_list(),
				"comments" = self$comments
			)
			return(investigation)
		},

		#' @details
		#'
		#' Make \code{[Investigation]} from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst) {
			self$filename <- lst[["filename"]]
			private$id <- lst[["id"]]
			self$title <- lst[["title"]]
			self$description <- lst[["description"]]
			self$submission_date <- lst[["submission_date"]]
			self$public_release_date <- lst[["public_release_date"]]
			self$ontology_source_references <- lst[["ontology_source_references"]]
			self$publications <- purrr::map(lst[["publications"]], ~{
				p <- Publication$new()
				p$from_list(.x)
				p
			})
			self$contacts <- purrr::map(lst[["contacts"]], ~{
				p <- Person$new()
				p$from_list(.x)
				p
			})
			self$studies <- purrr::map(lst[["studies"]], ~{
				s <- Study$new()
				s$from_list(.x)
				s
			})
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
		}
	),
	private = list(
		id = generate_id()
	)
)

#' identical.Investigation
#'
#' Allows checking for the identity of \code{[Investigation]} objects
#'
#' @param x a \code{[Investigation]} object
#' @param y a \code{[Investigation]} object
#' @export
identical.Investigation <- s3_identical_maker(c(
	"filename",
	"title",
	"description",
	"submission_date",
	"public_release_date",
	"ontology_source_references",
	"publications",
	"contacts",
	"studies",
	"comments"
))

