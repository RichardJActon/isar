# commentable, identifiable?

ontology_annotation <- R6Class(
	"ontology_annotation",
	public = list(
		term = '', # str
		term_source = NULL, # ontology_source
		term_accession = '', # str
		comments = list(), # comment
		id = '', # 
		
		initialize = function(
			term = '', # str
			term_source = ontology_source$new(), # ontology_source
			term_accession = '', # str
			comments = list(), # comment
			id = '' # 
		) {
			self$term <- term # str
			self$term_source <- term_source # ontology_source
			self$term_accession <- term_accession # str
			self$comments <- comments # comment
			self$id <- id # 
		}
	)
)
