isa_person <- R6Class(
	"isa_person",
	public = list(
		id = '',
		last_name = '',
		first_name = '',
		mid_initials = '',
		email = '',
		phone = '',
		fax = '',
		address = '',
		affiliation = '',
		roles = NULL,
		comments = NULL,

		initialize = function(
			id = '',
			last_name = '',
			first_name = '',
			mid_initials = '',
			email = '',
			phone = '',
			fax = '',
			address = '',
			affiliation = '',
			roles = NULL,
			comments = NULL
		) {
			self$id <- id
			self$last_name <- last_name
			self$first_name <- first_name
			self$mid_initials <- mid_initials
			self$email <- email
			self$phone <- phone
			self$fax <- fax
			self$address <- address
			self$affiliation <- affiliation
			self$roles <- roles
			self$comments <- comments
		}
	)
)
