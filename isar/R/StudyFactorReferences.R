#' R6 Class for StudyFactorReferences
#'
#' @details
#'
#'
#'
#' @field study_factor_references
#'
#' @importFrom R6 R6Class
#'
#' @export
StudyFactorReferences <- R6::R6Class(
	"StudyFactorReferences",
	public = list(
		study_factor_references = NULL,
		initialize = function(study_factor_references = NULL) {
			if (is.null(study_factor_references)) {
				self$study_factor_references <- NULL
			} else {
				self$add_study_factors(study_factor_references)
			}
		},
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
		add_study_factors = function(study_factors) {
			if(self$check_study_factors(study_factors)) {
				self$study_factor_references <- c(
					self$study_factor_references, study_factors
				)
			}
		},
		from_list = function(lst, explicitly_provided = logical()) {
			study_factors <- purrr::map(lst,~{
				sf <- StudyFactor$new(
					explicitly_provided = explicitly_provided
				)
				sf$from_list(.x)
				sf
			})

			names(study_factors) <- purrr::map_chr(study_factors, ~.x$`@id`)
			# names(study_factors) <- purrr::map_chr(
			# 	study_factors, ~.x$factor_name
			# )
			self$study_factor_references <- study_factors
		},
		get_study_factor_names = function() {
			self$study_factor_references %>% purrr::map_chr(~.x$factor_name)
		},
		get_study_factor_ids = function() {
			names(self$study_factor_references)
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Study Factor References"))
			purrr::walk(self$study_factor_references, ~.x$print())
		}
	)
)
