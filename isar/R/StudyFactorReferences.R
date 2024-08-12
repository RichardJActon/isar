#' R6 Class for StudyFactorReferences
#'
#'
#' @field study_factor_references a list [StudyFactor] objects
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
#' @field unit_references A list of units used as a [UnitReferences] object
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate test_list test_r6 check_logical
#' @importFrom purrr map_lgl map set_names map_chr
#' @importFrom cli cli_h1 col_blue
#'
#' @export
StudyFactorReferences <- R6::R6Class(
	"StudyFactorReferences",
	public = list(
		study_factor_references = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		#' @param study_factor_references a list [StudyFactor] objects
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
		#' @param unit_references A list of units used as a [UnitReferences] object
		initialize = function(
			study_factor_references = NULL,
			ontology_source_references = NULL,
			unit_references = NULL
		) {
			if (is.null(study_factor_references)) {
				self$study_factor_references <- NULL
			} else {
				# currently adding by default not setting (overwriting) ?
				self$add_study_factors(study_factor_references)
			}
			# check and set these?
			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references
		},
		#' @details
		#' Check if this input is a list of [StudyFactors] objects
		#' @param study_factors  a list of [StudyFactor] objects
		check_study_factors = function(study_factors) {
			if(
				checkmate::test_list(
					study_factors, min.len = 1, names = "unique"
				) &&
				all(purrr::map_lgl(
					study_factors, ~checkmate::test_r6(.x, "StudyFactor")
				))
			) {
				return(TRUE)
			} else {
				stop("All Study Factors must be StudyFactor objects!")
			}
		},
		#' @details
		#' Add new study factors to the study factor reference
		#' @param study_factors a list of [StudyFactor] objects
		add_study_factors = function(study_factors) {
			if(
				self$check_study_factors(
					c(self$study_factor_references, study_factors)
				)
			) {
				self$study_factor_references <- c(
					self$study_factor_references, study_factors
				)
			}
		},
		header_table = function() {
			self$study_factor_references %>%
				purrr::set_names(NULL) %>%
				purrr::map(~{
					.x$to_table() %>%
						dplyr::rename(
							"Study Factor Name" = rowname,
							"Study Factor Type" = term,
							"Study Factor Type Term Accession Number" =
								accession,
							"Study Factor Type Term Source REF" = source
						)
				}) %>%
				purrr::list_rbind() %>%
				t() %>%
				as.data.frame() %>%
				tibble::rownames_to_column() %>%
				tibble::as_tibble()
		},
		#' @details
		#' Generate an R list representation of a [StudyFactorReferences] object
		#' @param name The name of the material
		to_list = function() {
			self$study_factor_references %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
		},
		#' @details
		#' Populate a [StudyFactorReferences] object from a list
		#' @param lst a list
		#' @param explicitly_provided (logical) used to indicate if the study
		#' factor was explicitly listed in the input as opposed to being
		#' inferred to exist.
		from_list = function(lst, origin = NA) {
			study_factors <- purrr::map(lst, ~{
				sf <- StudyFactor$new(
					origin = origin,
					ontology_source_references =
						self$ontology_source_references#,
					#unit_references = self$unit_references
				)
				sf$from_list(.x)
				sf
			})

			names(study_factors) <- purrr::map_chr(study_factors, ~.x$`@id`)
			self$study_factor_references <- study_factors
		},
		#' @details
		#' Get the names of the study factor references
		#' @return character vector of study factor reference names
		get_study_factor_names = function() {
			self$study_factor_references %>% purrr::map_chr(~.x$factor_name)
		},
		#' @details
		#' Get the @ids of the study factor references
		#' @return character vector of study factor reference @ids
		get_study_factor_ids = function() {
			names(self$study_factor_references)
		},
		#' @details
		#' Pretty prints [StudyFactorReferences] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Study Factor References"))
			purrr::walk(self$study_factor_references, ~.x$print())
		}
	)
)
