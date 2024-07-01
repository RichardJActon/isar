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
#' @field measurement_type An [OntologyAnnotation] to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification)
#' @field technology_type An [OntologyAnnotation] to identify the technology  used to perform the measurement
#' @field technology_platform Manufacturer and platform name, e.g. Bruker AVANCE
#' @field filename  A field to specify the name of the assay file for compatibility with ISA-Tab
#' @field other_materials Materials associated with the assay
#' @field samples Samples associated with the assay
#' @field unit_references A list of units used as a [UnitReferences] object
#' @field characteristic_categories A list of [OntologyAnnotation] used in the annotation of material characteristics in the Assay
#' @field process_sequence A list of Process objects representing the experimental graphs at the Assay level
#' @field comments Comments associated with instances of this class
#' @field data_files [DataFile] objects
#' @field ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used
#' @field protocols [Protocols] objects
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate qtest check_r6 check_string
#' @importFrom purrr map set_names map_chr
#' @importFrom glue glue
#'
# #' @importFrom shinyWidgets pickerInput
#'
#' @export
Assay <- R6::R6Class(
	"Assay",
	public = list(
		measurement_type = NULL,
		technology_type = NULL,
		technology_platform = character(),
		filename = character(),
		other_materials = NULL,
		samples = NULL,
		characteristic_categories = NULL,
		process_sequence = NULL,
		comments = NULL,
		# graph = NULL,
		data_files = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		protocols = NULL,
		`@id` = character(),
		#' @details
		#' Create a new assay
		#'
		#' @param measurement_type An [OntologyAnnotation] to qualify the endpoint, or  what is being measured (e.g. gene expression profiling or protein  identification).
		#' @param technology_type An [OntologyAnnotation] to identify the technology  used to perform the measurement.
		#' @param technology_platform Manufacturer and platform name, e.g. Bruker AVANCE.
		#' @param filename  A field to specify the name of the assay file for compatibility with ISA-Tab.
		#' @param other_materials Materials associated with the assay.
		#' @param samples Samples associated with the assay.
		#' @param unit_references A list of units used as a [UnitReferences] object.
		#' @param characteristic_categories A list of [OntologyAnnotation] used in the annotation of material characteristics in the Assay.
		#' @param process_sequence A list of Process objects representing the experimental graphs at the Assay level.
		#' @param comments Comments associated with instances of this class.
		#' @param data_files [DataFile] objects.
		#' @param ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used.
		#' @param protocols [Protocols] objects
		#' @param @id identifier
		initialize = function(
			measurement_type = NULL,
			technology_type = NULL,
			technology_platform = character(),
			filename = character(),
			other_materials = NULL,
			samples = NULL,
			characteristic_categories = NULL,
			process_sequence = NULL,
			comments = NULL,
			# graph = NULL,
			data_files = NULL,
			ontology_source_references = NULL,
			unit_references = NULL,
			protocols = NULL,
			`@id` = character()
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
			self$other_materials <- other_materials
			self$samples <- samples
			self$characteristic_categories <- characteristic_categories
			self$process_sequence <- process_sequence
			self$comments <- comments
			#self$graph <- graph
			self$data_files <- data_files
			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references
			self$protocols <- protocols
			self$`@id` <- `@id`
		},
		#' @details
		#' checks the  measurement_type is an instance of [OntologyAnnotation]
		#'
		#' @param measurement_type An [OntologyAnnotation] to qualify the
		#' endpoint, or  what is being measured
		#' (e.g. gene expression profiling or protein  identification).
		check_measurement_type = function(measurement_type) {
			check <- checkmate::check_r6(measurement_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#'
		#' set the measurement type
		#'
		#' @param measurement_type An [OntologyAnnotation] to qualify the
		#' endpoint, or  what is being measured
		#' (e.g. gene expression profiling or protein  identification).
		set_measurement_type = function(measurement_type) {
			if (self$check_measurement_type(measurement_type)) {
				self$measurement_type <- measurement_type
			}
		},
		#' @details
		#' checks that technology_type an instance of [OntologyAnnotation]
		#'
		#' @param technology_type An [OntologyAnnotation] to identify the
		#' technology  used to perform the measurement.
		check_technology_type = function(technology_type) {
			check <- checkmate::check_r6(technology_type, "OntologyAnnotation")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#'
		#' set the technology type
		#'
		#' @param technology_type An [OntologyAnnotation] to identify the
		#' technology  used to perform the measurement.
		set_technology_type = function(technology_type) {
			if (self$check_technology_type(technology_type)) {
				self$technology_type <- technology_type
			}
		},
		#' @details
		#' Check if the technology_platform of the assay is a string
		#' @param technology_platform The technology_platform of the assay
		check_technology_platform = function(technology_platform) {
			check <- checkmate::check_string(
				technology_platform, min.chars = 1L
			)
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
			lst <- list()
			lst[["measurementType"]] <- self$measurement_type$to_list()
			lst[["technologyType"]] <- self$technology_type$to_list()
			lst[["technologyPlatform"]] <- self$technology_platform
			lst[["filename"]] <- self$filename
			lst[["materials"]][["otherMaterials"]] <- purrr::map(
				self$other_materials, ~.x$to_list()
			)
			lst[["materials"]][["samples"]] <- purrr::map(
				self$samples, ~.x$to_list()
			)
			lst[["unitCategories"]] <- purrr::map(self$unit_references, ~{
				c(list(`@id` = .x$`@id`), .x$to_list())
			})
			lst[["characteristicCategories"]] <-
				self$characteristic_categories$to_list()
			lst[["processSequence"]] <- purrr::map(
				self$process_sequence, ~.x$to_list()
			)
			lst[["comments"]] <- self$comments
			# lst[["graph"]] <- self$graph
			return(lst)
		},
		#' @details
		#'
		#' Make [Assay] from list
		#'
		#' @param lst an ontology source object serialized to a list
		#' @param recursive recursively parse
		#' @param json reading from json
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			self$`@id` <- lst$`@id`

			self$measurement_type <- OntologyAnnotation$new(
				ontology_source_references = self$ontology_source_references
			)

			if (!is.null(self$unit_references)) {
				self$unit_references <- UnitReferences$new(
					ontology_source_references =
						self$ontology_source_references#,
					#unit_references = self$unit_references
				)
			}

			if (!checkmate::test_list(lst[["unitCategories"]], len = 0)) {
				#browser()
				self$unit_references$from_list(
					lst[["unitCategories"]], source = self$`@id`,
					add = TRUE#,
					#recursive = recursive, json = json
				)
			}

			if (is.null(self$characteristic_categories)) {
				self$characteristic_categories <-
					CharacteristicCategoryReferences$new(
						ontology_source_references =
							self$ontology_source_references,
						unit_references = self$unit_references,
						source = self$`@id`
					)
			}
			# add any categories not found in the supplied reference
			self$characteristic_categories$from_list(
				lst[["characteristicCategories"]], # source ?
				explicitly_provided = TRUE, add = TRUE, source = self$`@id`
			)

			self$measurement_type$from_list(
				lst[["measurementType"]], recursive = recursive, json = json
			)
			self$technology_type <- OntologyAnnotation$new(
				ontology_source_references = self$ontology_source_references
			)
			self$technology_type$from_list(
				lst[["technologyType"]], recursive = recursive, json = json
			)
			#self$data_files <- lst[["dataFiles"]]
			self$data_files <-
				lst[["dataFiles"]] %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				purrr::map( ~{
					df <- DataFile$new()
					df$from_list(.x, json = json)
					df
				})

			self$technology_platform <- lst[["technologyPlatform"]]
			self$filename <- lst[["filename"]]

			#self$materials <- lst[["materials"]]
			self$other_materials <-
				lst[["materials"]][["otherMaterials"]] %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				purrr::map(~{
					m <- Material$new(
						ontology_source_references =
							self$ontology_source_references,
						characteristic_categories =
							self$characteristic_categories,
						unit_references = self$unit_references
					)
					m$from_list(.x, recursive = recursive, json = json)
					m
				})

			self$samples <- self$samples[
				purrr::map_chr(lst[["materials"]][["samples"]], ~.x$`@id`)
			]

			# process_sequence_order <- get_process_sequence_order_from_json(
			# 	lst[["processSequence"]]
			# )

			self$process_sequence <- purrr::map(
				lst[["processSequence"]], ~{
					ps <- Process$new(protocols = self$protocols)
					ps$from_list(.x, recursive = recursive, json = json) # recursive!
					ps
				}
			)#[order(process_sequence_order)]

			self$comments <- lst[["comments"]]
			# self$graph <- lst[["graph"]]
		},
		#' @details
		#' Pretty prints [Assay] objects
		print = function() { # ⚖️
			cli::cli_h1(cli::col_blue("Assay ⚖️️"))

			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("Measurement Type", self$measurement_type)
			green_bold_name_plain_content("Technology Type", self$technology_type)
			# green_bold_name_plain_content("", self$technology_platform)
			green_bold_name_plain_content("Filename", self$filename)
			# green_bold_name_plain_content("", self$other_materials)
			# green_bold_name_plain_content("", self$samples)
			# green_bold_name_plain_content("", self$unit_references)
			# green_bold_name_plain_content("", self$characteristic_categories)
			# green_bold_name_plain_content("", self$process_sequence)
			#
			# green_bold_name_plain_content("", self$data_files)
			# green_bold_name_plain_content("", self$ontology_source_references)
			# green_bold_name_plain_content("", self$protocols)

			pretty_print_comments(self$comments)
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
