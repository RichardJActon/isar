#' R6 class for a Source
#'
#' @field name A name/reference for the source material.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field comments Comments associated with instances of this class.
#' @field category_references an [CharacteristicCategoryReferences] object
#' @field @id identifier
#' @field unit_references an [UnitReferences] object
#'
#'
#' @importFrom R6 R6Class
#' @importFrom  purrr map
#'
#' @export
Source <- R6::R6Class(
	"Source",
	public = list(
		name = character(),
		characteristics = NULL,
		comments = NULL,
		category_references = NULL,
		`@id` =  character(),
		unit_references = NULL,
		#' @param name A name/reference for the source material.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param comments Comments associated with instances of this class.
		#' @param category_references an [CharacteristicCategoryReferences] object
		#' @param @id identifier
		#' @param unit_references an [UnitReferences] object
		initialize = function(
			name = character(),
			characteristics = NULL,
			comments = NULL,
			category_references = NULL,
			`@id` = character(),
			unit_references = NULL
		) {
			self$name <- name
			if(is.null(characteristics)) {
				self$characteristics <- characteristics
			} else {
				self$set_characteristics(characteristics)
			}
			self$comments <- comments
			self$`@id` <- `@id`
			if(is.null(category_references)) {
				self$category_references <-
					CharacteristicCategoryReferences$new()
			} else {
				self$category_references <- category_references
			}

			if(is.null(unit_references)) {
				self$unit_references <- UnitReferences$new()
			} else {
				self$unit_references <- unit_references
			}
		},

		to_table = function() {
			c(
				list(tibble::tibble_row("Source Name" = self$name)),
				purrr::map(self$characteristics, ~.x$to_table()) %>%
					purrr::set_names(NULL)
			) %>% purrr::list_cbind(name_repair = "minimal")
		},

		#' @details
		#'
		#' make an R list convertible to json
		#'
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			lst <- list()
			lst[["@id"]] <- self$`@id`
			lst[["name"]] <- sub("#.*?/(.*)", "\\1", self$`@id`)
			lst[["characteristics"]] <- self$characteristics %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			# lst[["comments"]] <- self$comments
			return(lst)
		},

		#' #' @details
		#'
		#' Make [Source] from list
		#'
		#' @param lst a source object serialized to a list
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		#' @param json json  (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if (!json) {
				private$id <- lst[["id"]]
			}
			self$`@id` <- lst[["@id"]]
			self$name <- sub(".*?-(.*)", "\\1", lst[["name"]])
			self$characteristics <-
				lst[["characteristics"]] %>%
				purrr::map(~{
					ch <- Characteristic$new(
						category_references =
							self$category_references,
						unit_references = self$unit_references
					)
					ch$from_list(.x, recursive = recursive, json = json)
					ch
				}) %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`))
			self$comments <- lst[["comments"]]
		},

		#' @details
		#' Pretty prints [Source] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Source"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("@id", self$`@id`)
			# green_bold_name_plain_content("ID", private$id)
			cli::cli_h1(cli::col_green("Characteristics"))
			cli::cli_ul(purrr::map_chr(
				self$characteristics, ~.x$category[["@id"]]
			))
			pretty_print_comments(self$comments)
		}
	)# ,
	# private = list(
	# 	id = generate_id()
	# )
)
