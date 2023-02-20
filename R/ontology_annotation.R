# commentable, identifiable?

#' R6 class for an experimental factor value
#'
#' @details
#'  [ontology_source].
#'
#' @section Public fields:
#' @field term  the name of ontology term
#' @field term_source the ontology that is the source of the term represented by an [ontology_source] object.
#' @field term_accession the unique identifier of the ontology term
#' @field comments comments
#' @field id unique identifier...
#'
ontology_annotation <- R6Class(
	"ontology_annotation",
	public = list(
		term = '', # str
		term_source = NULL, # ontology_source
		term_accession = '', # str
		comments = list(), # comment
		id = '', #

		#' @details
		#' create a new factor value
		#' @param term the name of ontology term
		#' @param term_source the ontology that is the source of the term represented by an [ontology_source] object.
		#' @param term_accession the unique identifier of the ontology term
		#' @param comments comments
		#' @param id a unique identifier ...
		#'
		#' @examples
		#' ontology_annotation$new(
		#'     term = "agrees with",
		#'     term_source = CiTO,
		#'     term_accession = "agreesWith"
		#' )
		initialize = function(
			term = '', # str
			term_source = NULL, # ontology_source
			term_accession = '', # str
			comments = list(), # comment
			id = '' #
		) {
			self$term_source <- term_source # ontology_source
			if(!is.null(term_source)){
				# handling on not explicitly enumerated lists?
				# - check a remote resource with an API call?
				if(term %in% names(self$term_source$terms_list)) {
					self$term <- term # str
				} else {
					stop("term is not in term source")
				}
				if(term_accession %in% unlist(self$term_source$terms_list)) {
					self$term_accession <- term_accession # str
				} else {
					stop("term accession is not in the term source")
				}
			}
			self$comments <- comments # comment
			self$id <- id #
		},

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' ontology_annotation$to_list()
		to_list = function(ld = FALSE) {
			ontology_annotation = list(
				"@id" = self$id,
				"annotationValue" = self$term,
				"termSource" = self$term_source$name,
				"termAccession" = self$term_accession,
				"comments" = self$comments ## !!
			)
			return(ontology_annotation)
		}
	)
)
