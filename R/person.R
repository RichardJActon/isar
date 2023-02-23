# use FOAF for this? http://xmlns.com/foaf/0.1/
# or schema.org person https://schema.org/Person

#' R6 class for person
#'
#' Represents person
#'
#'
#' @section Public fields:
#' @field id ...
#' @field last_name The last name of a person.
#' @field first_name The first name of a person.
#' @field mid_initials The middle initials of a person.
#' @field email The email address of a person.
#' @field phone The telephone number.
#' @field fax The fax number.
#' @field address The address of a person.
#' @field affiliation The organization affiliation for a person.
#' @field orcid Open Researcher and Contributor ID https://orcid.org/
#' @field roles A list of role(s) performed by this person. Roles reported here need not correspond to roles held withing their affiliated organization.
#' @field comments comments associated with instances of this class.
#'
#' @importFrom checkmate qtest
#'
#' @export
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
		orcid = '',
		roles = NULL,
		comments = NULL,

		#' @details
		#' person
		#' @param id ...
		#' @param last_name The last name of a person.
		#' @param first_name The first name of a person.
		#' @param mid_initials The middle initials of a person.
		#' @param email The email address of a person.
		#' @param phone The telephone number.
		#' @param fax The fax number.
		#' @param address The address of a person.
		#' @param affiliation The organization affiliation for a person.
		#' @param orcid Open Researcher and Contributor ID https://orcid.org/
		#' @param roles A list of role(s) performed by this person. Roles reported here need not correspond to roles held withing their affiliated organization.
		#' @param comments comments associated with instances of this class.
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
			orcid = '',
			roles = NULL,
			comments = NULL
		) {
			self$id <- id
			self$last_name <- last_name
			self$first_name <- first_name
			self$mid_initials <- mid_initials
			self$email <- email
			if(is.null(email)) {
				self$email <- email
			} else {
				self$set_email(email)
			}
			self$phone <- phone
			self$fax <- fax
			self$address <- address
			self$affiliation <- affiliation
			if (!checkmate::qtest(orcid, "S[0]")) {
				self$orcid <- orcid
			} else {
				self$set_orcid(orcid)
			}
			self$roles <- roles
			self$comments <- comments
		},
		# Checks

		#' @details
		#' check if email address is valid
		#'
		#' NB the regex used to validate emails is ... difficult to represent in roxygen syntax
		#' @param email an email address
		check_email = function(email) {
			# "^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
			# "/^[-!#-'*+\/-9=?^-~]+(?:\.[-!#-'*+\/-9=?^-~]+)*@[-!#-'*+\/-9=?^-~]+(?:\.[-!#-'*+\/-9=?^-~]+)+$/i"
			if (grepl(
				"^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$",
				email
				)
			) {
				return(TRUE)
			} else {
				stop("Invalid email address!")
			}
		},

		#' @details
		#' check if the ORCID is valid i.e. 4 lots of 4 digits separated by hyphens
		#'
		#' @param orcid an ORCID string
		check_orcid = function(orcid) {
			if (grepl("\\d{4}-\\d{4}-\\d{4}-\\d{4}", orcid)) {
				return(TRUE)
			} else {
				stop("Invalid ORCID! (4 lots of 4 digits seperated by -)")
			}
		},

		# Setters

		#' @details
		#' sets the email address field
		#' email addresses ar e first checked for validity by \code{[check_email]}
		#'
		#' @param email an email address
		set_email = function(email) {
			if(self$check_email(email)) {
				self$email <- email
			}
		},

		#' @details
		#' sets the orcid field
		#' ORCID's are first checked for validity by \code{[check_orcid]}
		#'
		#' @param orcid an ORCID
		set_orcid = function(orcid) {
			if(self$check_orcid(orcid)) {
				self$orcid <- orcid
			}
		},

		# Getters


		#

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' person$to_list()
		to_list = function(ld = FALSE) {
			person = list(
				"id" = self$id,
				"last_name" = self$last_name,
				"first_name" = self$first_name,
				"mid_initials" = self$mid_initials,
				"email" = self$email,
				"phone" = self$phone,
				"fax" = self$fax,
				"address" = self$address,
				"affiliation" = self$affiliation,
				"orcid" = self$orcid,
				"roles" = self$roles,
				"comments" = self$comments
			)
			return(person)
		},

		#' @details
		#'
		#' Make \code{[person]} from list
		#'
		#' @param lst an \code{[person]} object serialized to a list
		from_list = function(lst) {
			self$id <- lst[["id"]]
			self$last_name <- lst[["last_name"]]
			self$first_name <- lst[["first_name"]]
			self$mid_initials <- lst[["mid_initials"]]
			self$set_email(lst[["email"]])
			self$phone <- lst[["phone"]]
			self$fax <- lst[["fax"]]
			self$address <- lst[["address"]]
			self$affiliation <- lst[["affiliation"]]
			self$set_orcid(lst[["orcid"]])
			self$roles <- lst[["roles"]]
			self$comments <- lst[["comments"]]
		}
	)
)
