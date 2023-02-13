investigation <- R6Class(
	"investigation",
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
