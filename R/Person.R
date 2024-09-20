# use FOAF for this? http://xmlns.com/foaf/0.1/
# or schema.org person https://schema.org/Person

# use CRediT for roles? https://credit.niso.org/

#' R6 class for Person
#'
#' Represents a person
#'
#'
#' @field last_name The last name of a person.
#' @field first_name The first name of a person.
#' @field mid_initials The middle initials of a person.
#' @field email The email address of a person.
#' @field phone The telephone number.
#' @field fax The fax number.
#' @field address The address of a person.
#' @field affiliation The organization affiliation for a person.
#' @field orcid Open Researcher and Contributor ID https://orcid.org/
#' @field roles A list of role(s) performed by this person.
#' Roles reported here need not correspond to roles held withing their affiliated organization.
#' @field comments comments associated with instances of this class.
#' @field @id identifier
#' @field ontology_source_references an [OntologySourceReferences] object
#'
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate qtest
#' @importFrom purrr map
#' @importFrom cli cli_h1 col_blue cli_h2 col_cyan
#' @importFrom emo ji
#'
#' @export
Person <- R6::R6Class(
	"Person",
	public = list(
		last_name = character(),
		first_name = character(),
		mid_initials = character(),
		email = character(),
		phone = character(),
		fax = character(),
		address = character(),
		affiliation = character(),
		orcid = character(),
		roles = NULL,
		comments = NULL,
		`@id` = character(),
		ontology_source_references = NULL,
		#' @details
		#' person
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
		#' @param @id identifier
		#' @param ontology_source_references an [OntologySourceReferences] object
		initialize = function(
			last_name = character(),
			first_name = character(),
			mid_initials = character(),
			email = character(),
			phone = character(),
			fax = character(),
			address = character(),
			affiliation = character(),
			orcid = character(),
			roles = NULL,
			comments = NULL,
			`@id` = character(),
			ontology_source_references = NULL
		) {
			self$last_name <- last_name
			self$first_name <- first_name
			self$mid_initials <- mid_initials
			self$email <- email
			if (checkmate::qtest(email, "S[0]")) {
				self$email <- email
			} else {
				self$set_email(email)
			}
			self$phone <- phone
			self$fax <- fax
			self$address <- address
			self$affiliation <- affiliation
			if (checkmate::qtest(orcid, "S[0]")) {
				self$orcid <- orcid
			} else {
				self$set_orcid(orcid)
			}
			self$roles <- roles
			self$comments <- comments
			self$`@id` <- paste0("#person/", self$last_name)
			self$ontology_source_references <- ontology_source_references
		},
		# Checks

		#' @details
		#' check if email address is valid
		#'
		#' NB the regex used to validate emails is ... difficult to represent in roxygen syntax
		#' @param email an email address
		check_email = function(email) {
			# "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
			# "/^[-!#-'*+\/-9=?^-~]+(?:\.[-!#-'*+\/-9=?^-~]+)*@[-!#-'*+\/-9=?^-~]+(?:\.[-!#-'*+\/-9=?^-~]+)+$/i"
			if (grepl(
				"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$",
				email
				)
			) {
				return(TRUE)
			} else if(checkmate::qtest(email, "S[0]")) {
				warning("Empty email field")
				return(TRUE)
			} else {
				stop(
					"Probable invalid email address!\n",
					"at least according to this regex: ",
					"'^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$'",
					"(RFC 5322 is very permissive, if the email failed",
					"this check it is likely to fail others)"
				)
			}
		},

		# Consider a web-lookup check

		#' @details
		#' sets the email address field
		#' email addresses ar e first checked for validity by check_email
		#'
		#' @param email an email address
		set_email = function(email) {
			if(self$check_email(email)) {
				self$email <- email
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

		#' @details
		#' sets the orcid field
		#' ORCID's are first checked for validity by check_orcid
		#'
		#' @param orcid an ORCID
		set_orcid = function(orcid) {
			if(self$check_orcid(orcid)) {
				self$orcid <- orcid
			}
		},
		#' @details
		#' checks if comments are a named list of character vectors
		#' @param comments comments
		check_comments = function(comments) { check_comments(comments) },
		#' @details
		#' Sets comments if they are in a valid format
		#' @param comments a list of comments
		set_comments = function(comments) {
			if(self$check_comments(comments)) { self$comments <- comments }
		},
		#' @details
		#' Add comment if it is in a valid format
		#' @param comment a list of comments
		add_comment = function(comment) {
			if(self$check_comments(comment)) {
				self$comments <- c(comments, comment)
			}
		},

		#' @details
		#' generate a tabular representation of a person object for the
		#' investigation file.
		#' person is used in several places and therefore needs to have
		#' different prefixes in different blocks which can be supplied via
		#' the prefix argument
		#' @param prefix string to prepend to person information default = ""
		header_table = function(prefix = "") {
			dplyr::bind_cols(
				tibble::tibble(
					self$last_name,
					self$first_name,
					self$mid_initials,
					self$email,
					self$phone,
					self$fax,
					self$address,
					self$affiliation
				) %>% set_names(
					paste(prefix, "Person Last Name"),
					paste(prefix, "Person First Name"),
					paste(prefix, "Person Mid Initials"),
					paste(prefix, "Person Email"),
					paste(prefix, "Person Phone"),
					paste(prefix, "Person Fax"),
					paste(prefix, "Person Address"),
					paste(prefix, "Person Affiliation")
				),
				self$roles %>%
					purrr::map_dfr(~.x$to_table()) %>%
					purrr::map_dfc(
						~paste(ifelse(is.na(.x), "", .x), collapse = ";")
					) %>%
					purrr::set_names(
						paste(prefix, "Person Roles"),
						paste(prefix, "Person Roles Term Accession Number"),
						paste(prefix, "Person Roles Term Source REF")
					),
				comment_to_table_wide(self$comments)
			)
		},

		#' @details
		#' generate a tabular representation of a person object
		#' @return a Tibble
		to_table = function() {

			tibble::tibble(
				"Person Last Name" = self$last_name,
				"Person First Name" = self$first_name,
				"Person Mid Initials" = self$mid_initials,
				"Person Email" = self$email,
				"Person Phone" = self$phone,
				"Person Fax"= self$fax# ,
			)


			# dplyr::bind_rows(
			# 	tibble::tribble(
			# 		~name, ~value,
			# 		"Person Last Name", self$last_name,
			# 		"Person First Name", self$first_name,
			# 		"Person Mid Initials", self$mid_initials,
			# 		"Person Email", self$email,
			# 		"Person Phone", self$phone,
			# 		"Person Fax", self$fax# ,
			#
			# 		##!!! requires index
			# 		# "Person Roles", self$roles$term,
			# 		# "Person Roles Term Accession Number",
			# 		# 	self$roles$term_accession,
			# 		# "Person Roles Term Source REF",
			# 		# 	self$roles$term_source$name,
			# 	),
			# 	# multiple comment handling!!
			# 	comment_to_table(self$comments)
			# )
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' Person$new()
		to_list = function(ld = FALSE) {
			person = list(
				#"id" = private$id,
				"phone" = self$phone,
				"firstName" = self$first_name,
				"address" = self$address,
				"email" = self$email,
				"lastName" = self$last_name,
				"midInitials" = self$mid_initials,
				"@id" = self$`@id`,
				"fax" = self$fax,
				#"orcid" = self$orcid,
				"comments" = self$comments,
				"roles" = purrr::map(self$roles, ~.x$to_list()),
				"affiliation" = self$affiliation
			)
			return(person)
		},

		#' @details
		#'
		#' Make [Person] from list
		#'
		#' @param lst an [Person] object serialized to a list
		#' @param json default TRUE
		from_list = function(lst, json = TRUE) {
			if(json) {
				#private$id <- lst[["id"]]
				self$`@id` <- lst[["@id"]]
				self$last_name <- lst[["lastName"]]
				self$first_name <- lst[["firstName"]]
				self$mid_initials <- lst[["midInitials"]]
				self$set_email(lst[["email"]])
				self$phone <- lst[["phone"]]
				self$fax <- lst[["fax"]]
				self$address <- lst[["address"]]
				self$affiliation <- lst[["affiliation"]]

				# self$roles <- lst[["roles"]]
				# roles <- enumerate_roles(lst[["roles"]])

				self$roles <- purrr::map(
					lst[["roles"]], ~{
						oa <- OntologyAnnotation$new(
							ontology_source_references =
								self$ontology_source_references
						)
						oa$from_list(.x, json = json)
						oa
					}
				)

				# ontology_annotation_with_undefined_source_handler(
				# 	self = self, lst = lst[["roles"]], member = "roles",
				# 	ontology_source_name = "rolesOntologySource",
				# 	source_description = "Roles for Persons which lacked a source for the term"
				# )

				self$comments <- lst[["comments"]]
			} else {
				private$id <- lst[["id"]]
				self$last_name <- lst[["last_name"]]
				self$first_name <- lst[["first_name"]]
				self$mid_initials <- lst[["mid_initials"]]
				self$set_email(lst[["email"]])
				self$phone <- lst[["phone"]]
				self$fax <- lst[["fax"]]
				self$address <- lst[["address"]]
				self$affiliation <- lst[["affiliation"]]
				if (!is.null(lst[["orcid"]])) {
					self$set_orcid(lst[["orcid"]])
				}
				self$roles <- lst[["roles"]]
				self$comments <- lst[["comments"]]
			}
		},
		# #' @details
		# #' Get the uuid of this object
		# #' @return a uuid
		# get_id = function() {
		# 	private$id
		# },
		# #' @details
		# #' set the uuid of this object
		# #' @param id a uuid
		# #' @param suffix a human readable suffix
		# set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
		# 	private$id <- generate_id(id, suffix)
		# },
		#' @details
		#' Combine first name, middle initials, & last name
		#' @return string of full name
		get_full_name = function() { paste(
			c(self$first_name, self$mid_initials, self$last_name),
			collapse = " "
		) },
		#' @details
		#' Pretty Prints [Person] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Person ", emo::ji("user")))
			green_bold_name_plain_content("Name", self$get_full_name())
			#green_bold_name_plain_content("Name", paste(self$first_name, self$mid_initials, self$last_name))
			green_bold_name_plain_content("orcid", self$orcid)
			green_bold_name_plain_content(paste0("email ", emo::ji("email")), self$email) #
			green_bold_name_plain_content(paste0("phone ", emo::ji("telephone_receiver")), self$phone) #
			# green_bold_name_plain_content("id", private$id)
			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("affiliation", self$affiliation) # allow multiple...
			green_bold_name_plain_content(paste0("fax", emo::ji("fax")), self$fax) #
			green_bold_name_plain_content(paste0("address ", emo::ji("office")), self$address) #
			cli::cli_h2(cli::col_cyan("Roles"))
			purrr::walk(
				self$roles, ~green_bold_name_plain_content(
					.x$term_source$name, .x$term
				)
			)
			pretty_print_comments(self$comments)
		}
	)# ,
	# private = list(
	# 	id = generate_id()
	# )
)

