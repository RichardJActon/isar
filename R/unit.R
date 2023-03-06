# alternative to ontology anotation when using a quantity not a controled vocab
# value handling? - where to put the vaidation?

# hard code units of measurement ontology?
# unit annotation
# unit source

Unit <- R6::R6Class(
	"Unit",
	public = list(
		unit = NULL,


		initialize = function() {
			if (!is.null(unit)) {
				self$unit = ontology_source$new("OM")

			}
		}
	)
)

