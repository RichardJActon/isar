#' R6 class for a Source
#'
#' @field name A name/reference for the source material.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#'
#' @export
Source <- R6::R6Class(
	"Source",
	# inherit = Material,
	public = list(
		name = character(),
		characteristics = NULL,
		comments = NULL,
		characteristic_category_references = NULL,
		`@id` =  character(),

		#' @param name A name/reference for the source material.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = character(),
			characteristics = NULL,
			comments = NULL,
			characteristic_category_references = NULL,
			`@id` = character()
		) {
			self$name <- name
			if(is.null(characteristics)) {
				self$characteristics <- characteristics
			} else {
				self$set_characteristics(characteristics)
			}
			self$comments <- comments
			self$`@id` <- `@id`
			self$characteristic_category_references <-
				characteristic_category_references
		},

		#' @details
		#'
		#' make an R list convertible to json
		#'
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			source = list(
				"id" = private$id,
				"name" = self$name,
				"characteristics" = self$characteristics,
				"comments" = self$comments
			)
			return(source)
		},

		#' #' @details
		#'
		#' Make \code{[Source]} from list
		#'
		#' @param lst a source object serialized to a list
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if (!json) {
				private$id <- lst[["id"]]
			}
			self$`@id` <- lst[["@id"]]
			self$name <- lst[["name"]]
			self$characteristics <- purrr::map(lst[["characteristics"]], ~{
				ch <- Characteristic$new(
					category_references =
						self$characteristic_category_references
				)
				ch$from_list(.x, recursive = recursive, json = json)
				ch
			})
			self$comments <- lst[["comments"]]
		},

		print = function() {
			cli::cli_h1(cli::col_blue("Source"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("ID", private$id)
			cli::cli_h1(cli::col_green("Characteristics"))
			cli::cli_ul(purrr::map_chr(
				self$characteristics, ~.x$category[["@id"]]
			))
			pretty_print_comments(self$comments)
		}
	),
	private = list(
		id = generate_id()
	)
)
