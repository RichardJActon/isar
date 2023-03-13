#' R6 class for Investigation
#'
#' Represents an investigation from the ISA model
#'
#' @details
#' An investigation maintains metadata about the project context and links to one or more studies. There can only be 1 Investigation in an ISA descriptor.
#'
#' @field id ...
#' @field filename ...
#' @field identifier A locally unique identifier or an accession number provided by a repository.
#' @field title A concise name given to the investigation.
#' @field description A textual description of the investigation.
#' @field submission_date date on which the investigation was reported to the repository. This should be ISO8601 formatted.
#' @field public_release_date The date on which the investigation should be released publicly. This should be ISO8601 formatted.
#' @field ontology_source_references \code{[ontology_sources]} to be referenced by \code{[ontology_annotations]} used in this ISA descriptor.
#' @field publications A list of Publications associated with an Investigation. (format?)
#' @field contacts A list of People/contacts associated with an Investigation.
#' @field studies \code{[study]} is the central unit, containing information on the subject under study.
#' @field comments comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#'
#' @export
Investigation <- R6::R6Class(
	"Investigation",
	public = list(
		id = '',
		filename = '',
		identifier = '',
		title = '',
		description = '',
		submission_date = '',
		public_release_date = '',
		ontology_source_references = NULL,
		publications = NULL,
		contacts = NULL,
		studies = NULL,
		comments = NULL,

		#' @details
		#' Create a new investigation object
		#' @param id ...
		#' @param filename ...
		#' @param identifier A locally unique identifier or an accession number provided by a repository.
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
			id = '',
			filename = '',
			identifier = '',
			title = '',
			description = '',
			submission_date = '',
			public_release_date = '',
			ontology_source_references = NULL,
			publications = NULL,
			contacts = NULL,
			studies = NULL,
			comments = NULL
		) {
			self$id <- id
			self$filename <- filename
			self$identifier <- identifier
			self$title <- title
			self$description <- description
			self$submission_date <- submission_date
			self$public_release_date <- public_release_date
			self$ontology_source_references <- ontology_source_references
			self$publications <- publications
			self$contacts <- contacts
			self$studies <- studies
			self$comments <- comments
		}
	)
)

