# datafile
# material
# process
# ontology annotation

# inherits from Commentable, StudyAssayMixin ?

#' R6 class for an assay
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
#' @importFrom shinyWidgets pickerInput
#' @importFrom checkmate check_r6
#'
#' @export
Assay <- R6Class(
	"Assay",
	public = list(
		measurement_type = NULL,
		technology_type = NULL,
		technology_platform = NULL,
		filename = NULL,
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
			technology_platform = NULL,
			filename = NULL,
			materials = NULL,
			units = NULL,
			characteristic_categories = NULL,
			process_sequence = NULL,
			comments = NULL,
			graph = NULL
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
			self$technology_platform <- technology_platform
			self$filename <- filename
			self$materials <- materials
			self$units <- units
			self$characteristic_categories <- characteristic_categories
			self$process_sequence <- process_sequence
			self$comments <- comments
			self$graph <- graph

			self$string()
		},

		# Checkers

		#' @details
		#' checks the  measurement_type is an instance of \code{[OntologyAnnotation]}
		#'
		#' @param measurement_type An \code{[OntologyAnnotation]} to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		check_measurement_type = function(measurement_type) {
			check <- checkmate::check_r6(measurement_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#' checks that technology_type an instance of \code{[OntologyAnnotation]}
		#'
		#' @param technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
		check_technology_type = function(technology_type) {
			check <- checkmate::check_r6(technology_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},

		# Setters

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
		#'
		#' set the technology type
		#'
		#' @param technology_type An \code{[OntologyAnnotation]} to identify the technology  used to perform the measurement.
		set_technology_type = function(technology_type) {
			if (self$check_technology_type(technology_type)) {
				self$technology_type <- technology_type
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
		from_list = function(lst) {
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
		},

		#' @details
		#'
		#' stringify
		#'
		#' @return a string
		string = function() {
			glue::glue(
				.sep = "\n",

				# String template
				## direct
				"Assay(",
				"	measurement_type={measurement_type}",
				"	technology_type={technology_type}",
				"	technology_platform={technology_platform}",
				"	filename={filename}",
				## number of
				"	data_files={num_datafiles} DataFile objects",
				"	samples={num_samples} Sample objects",
				"	process_sequence={num_processes} Process objects",
				"	other_material={num_other_material} Material objects",
				"	characteristic_categories={num_characteristic_categories} OntologyAnnots",
				"	comments={num_comments} Comment objects",
				"	units={num_units} Unit objects",
				")",

				# Values
				## direct
				measurement_type				=	self$measurement_type,
				technology_type					=	self$technology_type,
				technology_platform				=	self$technology_platform,
				filename						=	self$filename,
				## number of
				num_datafiles					=	length(self$num_datafiles),
				num_samples						=	length(self$num_samples),
				num_processes					=	length(self$num_processes),
				num_other_material				=	length(self$num_other_material),
				num_characteristic_categories	=	length(self$num_characteristic_categories),
				num_comments					=	length(self$num_comments),
				num_units						=	length(self$num_units)
			)
		}
	)
)
