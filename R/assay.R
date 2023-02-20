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
#' An assay groups descriptions of provenance of sample processing for
#' related tests. Each test typically follows the steps of one particular
#' experimental workflow described by a particular protocol.
#'
#' @details
#'
#' @section Public fields:
#'
#' @field measurement_type An [ontology_annotation] to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
#' @field technology_type An [ontology_annotation] to identify the technology  used to perform the measurement.
#' @field technology_platform Manufacturer and platform name, e.g. Bruker AVANCE.
#' @field filename  A field to specify the name of the assay file for compatibility with ISA-Tab.
#' @field materials Materials associated with the assay, lists of '[sample]s' and [other_material]'.
#' @field units A list of units used in the annotation of material units ? [ontology_annotation] !!.
#' @field characteristic_categories A list of [ontology_annotation] used in the annotation of material characteristics in the Assay.
#' @field process_sequence A list of Process objects representing the experimental graphs at the Assay level.
#' @field comments Comments associated with instances of this class.
#' @field graph A graph representation of the assay graph.
#'

assay <- R6Class(
	"assay",
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
		#' @param measurement_type An [ontology_annotation] to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		#' @param technology_type An [ontology_annotation] to identify the technology  used to perform the measurement.
		#' @param technology_platform Manufacturer and platform name, e.g. Bruker AVANCE.
		#' @param filename  A field to specify the name of the assay file for compatibility with ISA-Tab.
		#' @param materials Materials associated with the assay, lists of '[sample]s' and [other_material]'.
		#' @param units A list of units used in the annotation of material units ? [ontology_annotation] !!.
		#' @param characteristic_categories A list of [ontology_annotation] used in the annotation of material characteristics in the Assay.
		#' @param process_sequence A list of Process objects representing the experimental graphs at the Assay level.
		#' @param comments Comments associated with instances of this class.
		#' @param graph A graph representation of the assay graph.
		initialize = function(
			measurement_type = NA,
			technology_type = NA,
			technology_platform = NA,
			filename = NA,
			materials = NA,
			units = NA,
			characteristic_categories = NA,
			process_sequence = NA,
			comments = NA,
			graph = NA
		) {
			self$measurement_type <- measurement_type
			self$technology_type <- technology_type
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

		# Setters

		#' @details
		#'
		#' set the measurement type
		#'
		#' @param measurement_type An [ontology_annotation] to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		set_measurement_type = function(measurement_type) {
			self$measurement_type = measurement_type
		},

		# Getters
		## Shiny

		#' @details
		#'
		#' shiny UI element for picking a measurement type
		#' @return a shiny UI element
		get_measurement_type_input = function() {
			shinyWidgets::pickerInput(
				ns("assay"), "Measurement Type",
				choices = NA,
				# selected = ,
				multiple = FALSE,
				options = shinyWidgets::pickerOptions(
					actionsBox = TRUE, liveSearch = TRUE#, size = 5
				)
			)
		},
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
