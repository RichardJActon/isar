# file_path ?
# checksum computation ?
# MIME types? - see mime package for guessing from extension

#' Represents a data file in an experimental graph.
#' @field filename A name/reference for the data file.
#' @field label The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'
#' @field generated_from Reference to Sample(s) the DataFile is generated from
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
#' @importFrom uuid UUIDgenerate
DataFile <- R6::R6Class(
	"DataFile",
	public = list(
		filename = NULL,
		label = NULL,
		generated_from = NULL,
		comments = NULL,
		#' @details
		#' Make a new DataFile object
		#' @param filename A name/reference for the data file.
		#' @param label The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'
		#' @param generated_from Reference to Sample(s) the DataFile is generated from
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			filename = NULL,
			label = NULL,
			generated_from = NULL,
			comments = NULL
		) {
			self$filename <- filename
			self$label <- label
			if (is.null(generated_from)) {
				self$generated_from <- generated_from
			} else {
				self$set_generated_from(generated_from)
			}
			self$comments <- comments
		},
		#' @details
		#' Check that generated_from is an \code{[Sample]} object
		#' @param generated_from an \code{[Sample]} object
		check_generated_from = function(generated_from) {
			check <- checkmate::check_r6(generated_from, "Sample")
			error_with_check_message_on_failure(
				check, nextline = "Class: Sample"
			)
		},
		#' @details
		#' Set generated_from if input is valid
		#' @param generated_from an \code{[Sample]} object
		set_generated_from = function(generated_from) {
			if(self$check_generated_from(generated_from)) {
				self$generated_from <- generated_from
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
		#' An R list representation of a \code{[DataFile]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE){
			characteristic <- list(
				"id" = private$id,
				"filename" = self$filename,
				"label" = self$label,
				"generated_from" = self$generated_from,
				"comments" = self$comments
			)
			return(characteristic)
		},
		#' @details
		#' Make \code{[DataFile]} object from list
		#' @param lst an Characteristic object serialized to a list
		from_list = function(lst) {
			private$id <- lst[["id"]]
			self$filename <- lst[["filename"]]
			self$label <- lst[["label"]]
			self$generated_from <- lst[["generated_from"]]
			self$comments <- lst[["comments"]]
		},
		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		}
	),
	private = list(
		id = uuid::UUIDgenerate()
	)
)
