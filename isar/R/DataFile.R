# file_path ?
# checksum computation ?
# MIME types? - see mime package for guessing from extension

#' Represents a data file in an experimental graph.
#' @field filename A name/reference for the data file.
#' @field file_path the path to the file
#' @field check_file_exists check that the file exists
#' @field compute_hash should the hash of the file be computed?
#' @field hash_algo hashing algorithm to use (default md5)
#' @field label The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'
#' @field generated_from Reference to Sample(s) the DataFile is generated from
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
#' @importFrom uuid UUIDgenerate
#' @importFrom digest digest
DataFile <- R6::R6Class(
	"DataFile",
	public = list(
		filename = character(),
		file_path = character(),
		check_file_exists = FALSE,
		compute_hash = FALSE,
		hash_algo = "md5",
		label = NULL,
		generated_from = NULL,
		comments = NULL,
		#' @details
		#' Make a new DataFile object
		#' @param filename A name/reference for the data file.
		#' @param file_path the path to the file
		#' @param check_file_exists check that the file exists
		#' @param compute_hash should the hash of the file be computed?
		#' @param hash_algo hashing algorithm to use (default md5)
		#' @param label The data file type, as indicated by a label such as 'Array Data File' or 'Raw Data File'
		#' @param generated_from Reference to Sample(s) the DataFile is generated from
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			filename = character(),
			file_path = character(),
			check_file_exists = FALSE,
			compute_hash = FALSE,
			hash_algo = "md5",
			label = NULL,
			generated_from = NULL,
			comments = NULL
		) {
			if (checkmate::qtest(filename, "S[0]")) {
				self$filename <- filename
			} else {
				self$set_filename(filename)
			}

			if (checkmate::qtest(file_path, "S[0]")) {
				self$file_path <- file_path
			} else {
				self$set_file_path(file_path)
			}
			if (
				checkmate::qtest(filename, "S[0]") &&
				!checkmate::qtest(self$file_path, "S[0]")
			) {
				self$set_filename(fs::path_file(self$file_path))
				message(paste0("Setting filename from path: ", self$filename))
			}
			if (
				!checkmate::qtest(filename, "S[0]") &&
				!checkmate::qtest(self$file_path, "S[0]")
			) {
				if (fs::path_file(self$file_path) != filename) {
					stop("filename does not match path")
				}
			}
			# self$file_path <- file_path

			if(isTRUE(check_file_exists)) {
				if(!fs::file_exists(file_path)) {
					stop(paste0("Could not find: ", file_path))
				}
			}
			self$check_file_exists <- check_file_exists

			if (isTRUE(compute_hash)) {
				private$hash <- digest::digest(
					object = file_path, algo = hash_algo, file = TRUE
				)
			}
			self$compute_hash <- compute_hash
			self$hash_algo <- hash_algo


			self$label <- label
			if (is.null(generated_from)) {
				self$generated_from <- generated_from
			} else {
				self$set_generated_from(generated_from)
			}
			self$comments <- comments
		},
		#' @details
		#' Check if the filename of the \code{[DataFile]} is a string
		#' @param filename The filename of the \code{[DataFile]}
		check_filename = function(filename) {
			check <- checkmate::check_string(filename, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the filename of the \code{[DataFile]} if valid
		#' @param filename The filename of the \code{[DataFile]}
		set_filename = function(filename) {
			if (self$check_filename(filename)) { self$filename <- filename }
		},

		#' @details
		#' Check if the file_path of the \code{[DataFile]} is a string
		#' @param file_path The file_path of the \code{[DataFile]}
		check_file_path = function(file_path) {
			check <- checkmate::check_string(file_path, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the file_path of the \code{[DataFile]} if valid
		#' @param file_path The file_path of the \code{[DataFile]}
		set_file_path = function(file_path) {
			if (self$check_file_path(file_path)) { self$file_path <- file_path }
		},
		#'
		#' @details
		#' checks that hash algo is valid hash algorithm name
		#' @return logical
		check_hash_algo = function() {
			all(self$hash_algo %in% c(
				"md5", "sha1", "crc32", "sha256", "sha512", "xxhash32",
				"xxhash64", "murmur32", "spookyhash", "blake3"
			))
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
		#'
		#' Checks that the hash of the file at filepath matches the stored
		#' hash of the file. Returns TRUE if they match and FALSE if not.
		#'
		#' validate_file
		#' @return logical
		validate_file = function() {
			all(private$hash == digest::digest(
				object = self$file_path, algo = self$hash_algo, file = TRUE
			))
		},

		#' @details
		#' An R list representation of a \code{[DataFile]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE){
			date_file <- list(
				"id" = private$id,
				"filename" = self$filename,
				"file_path" = self$file_path,
				"check_file_exists" = self$check_file_exists,
				"compute_hash" = self$compute_hash,
				"hash_algo" = self$hash_algo,
				"label" = self$label,
				"generated_from" = self$generated_from,
				"comments" = self$comments
			)
			return(date_file)
		},
		#' @details
		#' Make \code{[DataFile]} object from list
		#' @param lst an Characteristic object serialized to a list
		from_list = function(lst) {
			private$id <- lst[["id"]]
			self$filename <- lst[["filename"]]
			self$file_path <- lst[["file_path"]]
			self$check_file_exists <- lst[["check_file_exists"]]
			self$compute_hash <- lst[["compute_hash"]]
			self$hash_algo <- lst[["hash_algo"]]
			self$label <- lst[["label"]]
			self$generated_from <- lst[["generated_from"]]
			self$comments <- lst[["comments"]]
		},
		#' @details
		#' Get the hash of the file
		#' @return a uuid
		get_hash = function() { private$hash },
		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() { private$id },
		#' @details
		#' set the uuid of this object
		#' @param id a uuid
		#' @param suffix a human readable suffix
		set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
			private$id <- generate_id(id, suffix)
		}
	),
	private = list(
		id = generate_id(),
		hash = NULL
	)
)

#' identical.DataFile
#'
#' Allows checking for the identity of \code{[DataFile]} objects
#'
#' @param x a \code{[DataFile]} object
#' @param y a \code{[DataFile]} object
#' @export
identical.DataFile <- s3_identical_maker(c(
	"filename",
	"label",
	"generated_from",
	"comments"
))

