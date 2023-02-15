sample <- R6Class(
	"sample",
	public = list(
		id = '',
		name = '',
		factor_values = NULL,
		characteristics = NULL,
		derives_from = NULL,
		comments = NULL,

		initialize = function(
			id = '',
			name = '',
			factor_values = NULL,
			characteristics = NULL,
			derives_from = NULL,
			comments = NULL
		) {
			self$id <- id
			self$set_name(name)
			self$factor_values <- factor_values
			self$characteristics <- characteristics
			self$derives_from <- derives_from
			self$comments <- comments
		},
		check_name = function(name) {
			tryCatch(
				{
					if (is.character(name) && length(name) == 1L) {
						return(TRUE)
					} else {
						stop()
					}
				},
				error = function(e) {
					stop("Name was not a string!")
					#message("Name was not a string!")
				}
			)
		},
		set_name = function(name) {
			if(self$check_name(name)) {
				self$name <- name
			}
		}
	)
)
