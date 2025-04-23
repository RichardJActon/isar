#' ProtocolReferences
#'
#' @field protocols A names list of [Protocol] objects
#' @field ontology_source_references ontology_source_references
#'
#' @importFrom R6 R6Class
#'
#' @export
ProtocolReferences <- R6::R6Class(
	"ProtocolReferences",
	public = list(
		protocols = list(),
		ontology_source_references = NULL,
		#' @details
		#'
		#' Create a new [ProtocolReferences] object
		#' @param protocols A names list of [Protocol] objects
		#' @param ontology_source_references ontology_source_references
		initialize = function(
			protocols = list(),
			ontology_source_references = NULL
		) {
			self$protocols <- protocols
			self$ontology_source_references <- ontology_source_references
		},
		#' @details
		#' check protocols is a list of [Protocols] objects
		#' @param protocols a list of [Protocol] objects
		check_protocols = function(protocols) {
			if(all(purrr::map_lgl(protocols, ~checkmate::check_r6(
				.x, "Protocol"
			)))) { return(TRUE) } else {
				stop("All protocols must be Protocol Objects!")
			}
		},
		#' @details
		#'
		#' Add [Protocols] to the [ProtocolsReferences] object
		#' @param protocols a list of [Protocols] to add to the
		#' [ProtocolReferences] object
		add_protocols = function(protocols) {
			if (self$check_protocols(protocols)) {
				self$protocols <- c(self$protocols, protocols)
			}
		},
		#' @details
		#'
		#' Set the [Protocols] to the [ProtocolsReferences] object
		#' @param protocols a list of [Protocols] to add to the
		#' [ProtocolReferences] object
		set_protocols = function(protocols) {
			if (self$check_protocols(protocols)) {
				self$protocols <- protocols
			}
		},
		#' @details
		#' get the IDs of the protocols in the reference
		#' @return a character vector of protocol IDs
		get_protocol_ids = function() {
			names(self$protocols)
		},
		#' @details
		#' get the names of the protocols in the reference
		#' @return a character vector of protocol namees
		get_protocol_names = function() {
			purrr::map_chr(self$protocols, ~.x$name)
		},
		#' @details
		#' get the origins of the protocols in the reference
		#' @return a character vector of the origins of the protocols in the
		#'  reference
		get_protocol_origins = function() {
			purrr::map(self$protocols, ~.x$origin)
		},
		#' @details
		#' get the number of protocols in the reference
		#' @return an integer
		n_protocols = function() {
			length(self$protocols)
		},
		#' @details
		#' serialise the ProtocolReferences object to a list
		#' @return a list
		to_list = function() {
			self$protocols %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
		},
		#' @details
		#' Populate the fields of the [ProtocolReferences] object using a list
		#' @param lst a list
		#' @param origin the id of the
		from_list = function(lst, origin = NA) {# , add = FALSE
			lst %>% purrr::map(~{
				p <- Protocol$new(
					ontology_source_references =
						self$ontology_source_references,
					origin = origin
				)
				p$from_list(.x)
				p
			}) %>%
				purrr::set_names(., purrr::map_chr(., ~.x[["@id"]])) %>%
				self$set_protocols()
		},
		#' @details
		#' Pretty prints the [ProtocolReferences] object
		print = function() {
			cli::cli_h1(cli::col_blue("Protocol References"))
			purrr::walk(self$protocols, ~.x$print())
		}
	)
)
#' check_protocol_references
#'
#' returns TRUE if protocol_references is an [ProtocolReferences]
#' object and throws an error if it is not
#'
#' @param protocol_references something you want to check is an
#' [ProtocolReferences] object.
#'
#' @export
#'
check_protocol_references <- function(protocol_references) {
	check <- checkmate::check_r6(protocol_references, "ProtocolReferences")
	error_with_check_message_on_failure(check)
}

#' set_protocol_references
#'
#' sets protocol_references attribute if
#' protocol_reference is an [ProtocolReferences] object
#'
#' @param self an object with an protocol_references attribute
#' @param protocol_references an [ProtocolReferences] object
#' @param null.action how to handle NULLs:
#' - "error" thow an error
#' - "passthrough" set to NULL
#' - "create" set to an empty  [ProtocolReferences] object
set_protocol_references <- function(
	self, protocol_references, null.action = "error"
) {
	if (is.null(protocol_references)) { switch(null.action,
		"error" = { stop("protocol_references must not be NULL!") },
		"passthrough" = { self$protocol_references <- protocol_references },
		"create" = { self$protocol_references <- ProtocolReferences$new(
				ontology_source_references = self$ontology_source_references
		) }
	)} else if(check_protocol_references(protocol_references)) {
		self$protocol_references <- protocol_references
	}
}
