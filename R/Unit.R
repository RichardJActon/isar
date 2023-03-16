# alternative to ontology annotation when using a quantity not a controlled vocab
# value handling? - where to put the validation?
#
# have a warnings / messages field that can indicate if you are not using an ontology and
# show this in the UI?

# hard code units of measurement ontology?
# unit annotation
# unit source

Unit <- R6::R6Class(
	"Unit",
	public = list(
		unit = NULL,


		initialize = function() {
			if (!is.null(unit)) {
				self$unit = OntologySource$new("OM")

			}
		}
	)
)

