#' R6 class for ProcessSequenceNode
#' @importFrom R6 R6Class
ProcessSequenceNode <- R6::R6Class(
	"ProcessSequenceNode",
	public = list(
		#' @details
		#' new ProcessSequenceNode
		initialize = function() {
			private$sequence_identifier <- private$sequence_identifier + 1
		},
		#' @details
		#' increment ProcessSequenceNode
		assign_identifier = function() {
			private$sequence_identifier <- private$sequence_identifier + 1
		},
		#' @details
		#' get ProcessSequenceNode identifier
		get_identifier =function(){ private$sequence_identifier }
	),
	private = list(sequence_identifier = 0)
)

