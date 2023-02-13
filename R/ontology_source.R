ontology_source <- R6Class(
	"ontology_source",
	public = list(
		name = '',
		file = '',
		version = '',
		description = '',
		comments = list(),
		
		initialize = function(
			name = '',
			file = '',
			version = '',
			description = '',
			comments = list()
		) {
			self$name <- name
			self$file <- file
			self$version <- version
			self$description <- description
			self$comments <- comments
		}
	)
)
