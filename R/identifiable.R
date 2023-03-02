identifiable <- R6::R6Class(
	"identifiable",
	public = list(
		id = NULL,
		initialize = function(id = NULL) {
			self$id <- id
		},
		set_id = function() {
			self$id <- uuid::UUIDgenerate()
		}
	)
)
