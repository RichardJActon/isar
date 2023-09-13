#' Assay
#'
#' An assay represents a test performed either on material taken from a
#' subject or on a whole initial subject, producing qualitative or
#' quantitative measurements.
#'
#' @details
#' An assay groups descriptions of provenance of sample processing for
#' related tests. Each test typically follows the steps of one particular
#' experimental workflow described by a particular protocol.
#'
#'
#' @field measurement_type An \code{[OntologyAnnotation]} to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
#' @field technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
#' @field technology_platform Manufacturer and platform name, e.g. Bruker AVANCE.
#' @field filename  A field to specify the name of the assay file for compatibility with ISA-Tab.
#' @field materials Materials associated with the assay, lists of '\code{[Sample]}s' and other_material'.
#' @field units A list of units used in the annotation of material units ? \code{[OntologyAnnotation]} !!.
#' @field characteristic_categories A list of \code{[OntologyAnnotation]} used in the annotation of material characteristics in the Assay.
#' @field process_sequence A list of Process objects representing the experimental graphs at the Assay level.
#' @field comments Comments associated with instances of this class.
#' @field graph A graph representation of the assay graph.
#'
#' @importFrom glue glue
# #' @importFrom shinyWidgets pickerInput
#' @importFrom checkmate check_r6
#' @importFrom R6 R6Class
#'
#' @export
Assay <- R6::R6Class(
	"Assay",
	public = list(
		measurement_type = NULL,
		technology_type = NULL,
		technology_platform = character(),
		filename = character(),
		materials = NULL,
		units = NULL,
		characteristic_categories = NULL,
		process_sequence = NULL,
		comments = NULL,
		graph = NULL,

		#' @details
		#' Create a new assay
		#'
		#' @param measurement_type An \code{[OntologyAnnotation]} to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		#' @param technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
		#' @param technology_platform Manufacturer and platform name, e.g. Bruker AVANCE.
		#' @param filename  A field to specify the name of the assay file for compatibility with ISA-Tab.
		#' @param materials Materials associated with the assay, lists of '\code{[Sample]}s' and other_material'.
		#' @param units A list of units used in the annotation of material units ? \code{[OntologyAnnotation]} !!.
		#' @param characteristic_categories A list of \code{[OntologyAnnotation]} used in the annotation of material characteristics in the Assay.
		#' @param process_sequence A list of Process objects representing the experimental graphs at the Assay level.
		#' @param comments Comments associated with instances of this class.
		#' @param graph A graph representation of the assay graph.
		initialize = function(
			measurement_type = NULL,
			technology_type = NULL,
			technology_platform = character(),
			filename = character(),
			materials = NULL,
			units = NULL,
			characteristic_categories = NULL,
			process_sequence = NULL,
			comments = NULL,
			graph = NULL,
			data_files = NULL
		) {
			if (is.null(measurement_type)) {
				self$measurement_type <- measurement_type
			} else {
				self$set_measurement_type(measurement_type)
			}
			if (is.null(technology_type)) {
				self$technology_type <- technology_type
			} else {
				self$set_technology_type(technology_type)
			}
			if (checkmate::qtest(technology_platform, "S[0]")) {
				self$technology_platform <- technology_platform
			} else {
				self$set_technology_platform(technology_platform)
			}
			if (checkmate::qtest(filename, "S[0]")) {
				self$filename <- filename
			} else {
				self$set_filename(filename)
			}
			self$materials <- materials
			self$units <- units
			self$characteristic_categories <- characteristic_categories
			self$process_sequence <- process_sequence
			self$comments <- comments
			self$graph <- graph
			self$data_files <- data_files
			# self$string()
		},
		#' @details
		#' checks the  measurement_type is an instance of \code{[OntologyAnnotation]}
		#'
		#' @param measurement_type An \code{[OntologyAnnotation]} to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		check_measurement_type = function(measurement_type) {
			check <- checkmate::check_r6(measurement_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#'
		#' set the measurement type
		#'
		#' @param measurement_type An \code{[OntologyAnnotation]} to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		set_measurement_type = function(measurement_type) {
			if (self$check_measurement_type(measurement_type)) {
				self$measurement_type <- measurement_type
			}
		},
		#' @details
		#' checks that technology_type an instance of \code{[OntologyAnnotation]}
		#'
		#' @param technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
		check_technology_type = function(technology_type) {
			check <- checkmate::check_r6(technology_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#'
		#' set the technology type
		#'
		#' @param technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
		set_technology_type = function(technology_type) {
			if (self$check_technology_type(technology_type)) {
				self$technology_type <- technology_type
			}
		},
		#' @details
		#' Check if the technology_platform of the assay is a string
		#' @param technology_platform The technology_platform of the assay
		check_technology_platform = function(technology_platform) {
			check <- checkmate::check_string(technology_platform, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the technology_platform of the assay if valid
		#' @param technology_platform The technology_platform of the assay
		set_technology_platform = function(technology_platform) {
			if (self$check_technology_platform(technology_platform)) {
				self$technology_platform <- technology_platform
			}
		},
		#' @details
		#' Check if the filename of the assay is a string
		#' @param filename The filename of the assay
		check_filename = function(filename) {
			check <- checkmate::check_string(filename, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the filename of the assay if valid
		#' @param filename The filename of the assay
		set_filename = function(filename) {
			if (self$check_filename(filename)) { self$filename <- filename }
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
		# # Getters
		# ## Shiny
		# 		# #' @details
		# #'
		# #' shiny UI element for picking a measurement type
		# #' @param id Shiny module namespace
		# #' @return a shiny UI element
		# get_assay_ui = function(id = "assay") {
		# 	self$measurement_type$get_OntologyAnnotation_ui(id)
		# 	# ns <- shiny::NS(id)
		# 	# shinyWidgets::pickerInput(
		# 	# 	ns(namespace), "Measurement Type",
		# 	# 	choices = names(self$measurement_type$term_source$terms_list),
		# 	# 	selected = self$measurement_type$term,
		# 	# 	multiple = FALSE,
		# 	# 	options = shinyWidgets::pickerOptions(
		# 	# 		actionsBox = TRUE, liveSearch = TRUE#, size = 5
		# 	# 	)
		# 	# )
		# },
		# 		# #' @details
		# #'
		# #' shiny server element for picking a measurement type
		# #' @param id Shiny module namespace
		# #' @return a shiny server function
		# get_assay_server = function(id) {
		# 	self$measurement_type$get_OntologyAnnotation_server(id)
		# 	# shiny::moduleServer(id, function(input, output, session) {
		# 	#
		# 	# })
		# },
		## conversions

		#' @details
		#'
		#' make an R list convertible to json
		#'
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			assay = list(
				"measurement_type" = self$measurement_type$to_list(),
				"technology_type" = self$technology_type$to_list(),
				"technology_platform" = self$technology_platform,
				"filename" = self$filename,
				"materials" = self$materials,
				"units" = self$units$to_list(),
				"characteristic_categories" = self$characteristic_categories$to_list(),
				"process_sequence" = self$process_sequence,
				"comments" = self$comments,
				"graph" = self$graph
			)
			return(assay)
		},
		#' @details
		#'
		#' Make \code{[Assay]} from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst, recursive = TRUE, json = FALSE) {
			if (json) {
				# if (recursive) {}
				self$measurement_type <- OntologyAnnotation$new()
				self$measurement_type$from_list(
					lst[["measurementType"]], recursive = TRUE, json = TRUE
				)
				self$technology_type <- OntologyAnnotation$new()
				self$technology_type$from_list(
					lst[["technologyType"]], recursive = TRUE, json = TRUE
				)
				#self$data_files <- lst[["dataFiles"]]
				self$data_files <- purrr::map(lst[["dataFiles"]], ~{
					df <- DataFile$new()
					df$from_list(.x, recursive = TRUE, json = TRUE)
				})

				self$technology_platform <- lst[["technologyPlatform"]]
				self$filename <- lst[["filename"]]
				self$materials <- lst[["materials"]]

				self$units <- lst[["unitCategories"]]
				self$characteristic_categories <- lst[["characteristicCategories"]]

				self$process_sequence <- lst[["processSequence"]]
				self$comments <- lst[["comments"]]
				self$graph <- lst[["graph"]]
			} else {
				self$measurement_type <- lst[["measurement_type"]]
				self$technology_type <- lst[["technology_type"]]

				self$technology_platform <- lst[["technology_platform"]]
				self$filename <- lst[["filename"]]
				self$materials <- lst[["materials"]]

				self$units <- lst[["units"]]
				self$characteristic_categories <- lst[["characteristic_categories"]]

				self$process_sequence <- lst[["process_sequence"]]
				self$comments <- lst[["comments"]]
				self$graph <- lst[["graph"]]
			}
		}

		# #' @details
		# #'
		# #' stringify
		# #'
		# #' @return a string
		# string = function() {
		# 	glue::glue(
		# 		.sep = "\n",
		#
		# 		# String template
		# 		## direct
		# 		"Assay(",
		# 		"	measurement_type={measurement_type}",
		# 		"	technology_type={technology_type}",
		# 		"	technology_platform={technology_platform}",
		# 		"	filename={filename}",
		# 		## number of
		# 		"	data_files={num_datafiles} DataFile objects",
		# 		"	samples={num_samples} Sample objects",
		# 		"	process_sequence={num_processes} Process objects",
		# 		"	other_material={num_other_material} Material objects",
		# 		"	characteristic_categories={num_characteristic_categories} OntologyAnnots",
		# 		"	comments={num_comments} Comment objects",
		# 		"	units={num_units} Unit objects",
		# 		")",
		#
		# 		# Values
		# 		## direct
		# 		measurement_type				=	self$measurement_type,
		# 		technology_type					=	self$technology_type,
		# 		technology_platform				=	self$technology_platform,
		# 		filename						=	self$filename,
		# 		## number of
		# 		num_datafiles					=	length(self$num_datafiles),
		# 		num_samples						=	length(self$num_samples),
		# 		num_processes					=	length(self$num_processes),
		# 		num_other_material				=	length(self$num_other_material),
		# 		num_characteristic_categories	=	length(self$num_characteristic_categories),
		# 		num_comments					=	length(self$num_comments),
		# 		num_units						=	length(self$num_units)
		# 	)
		# }
	)
)

#' identical.Assay
#'
#' Allows checking for the identity of \code{[Assay]} objects
#'
#' @param x a \code{[Assay]} object
#' @param y a \code{[Assay]} object
#' @export
identical.Assay <- s3_identical_maker(c(
	"measurement_type",
	"technology_type",
	"technology_platform",
	"filename",
	"materials",
	"units",
	"characteristic_categories",
	"process_sequence",
	"comments",
	"graph"
), get_id = FALSE)
