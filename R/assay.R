library(R6)

# datafile
# material
# process
# ontology annotation

# inherits from Commentable, StudyAssayMixin ?

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

			self$str()
		},
		set_measurement_type = function(measurement_type) {
			self$measurement_type = measurement_type
		},
		get_measurement_type_input = function() {
			shinyWidgets::pickerInput(
				ns("measurement_type"), "Measurement Type",
				choices = NA,
				# selected = ,
				multiple = FALSE,
				options = shinyWidgets::pickerOptions(
					actionsBox = TRUE, liveSearch = TRUE, size = 5
				)
			)
		}
		str = function() {
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
