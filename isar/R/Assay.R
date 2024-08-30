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
		#' @details
		#' Check that all samples are [Sample] objects
		#' @param samples a list of [Sample] objects
		check_samples = function(samples) {
			if(
				checkmate::test_list(samples, min.len = 1) &&
				all(
					purrr::map_lgl(samples, ~checkmate::test_r6(.x, "Sample"))
				)
			) { return(TRUE) }
			stop("All samples must be Sample objects")
		},
		#' @details
		#' Check that all sample ids reference one of the supplied sample objects
		#' @param sample_ids a vector of sample IDs
		check_sample_ids = function(sample_ids) {
			lgl <- sample_ids %in% names(self$samples)
			if(all(lgl)) { return(TRUE) } else {
				if (any(lgl)) {
					stop(
						"These samples are present:\n",
						paste0(sample_ids[lgl], collapse = ", "),
						"\n but these are missing!: \n",
						paste0(sample_ids[!lgl], collapse = ", ")
					)
				}
			}
			stop("None of the samples with the supplied IDs were found!")
		},
		#' @details
		#' Set samples if samples is a list of [Sample] objects
		#' @param samples a list of [Sample] objects
		set_samples = function(samples) {
			if (self$check_samples(samples)) {
				self$samples <- samples
			}
		},
		set_samples_by_id = function(sample_ids) {
			lgl <- sample_ids %in% names(self$samples)
			if(all(lgl)) {
				self$samples <- self$samples[sample_ids]
				# update samples with new characteristics
				# samples objects are created before the
				# characteristics from assays have been registered in
				# the shared characteristic category reference object
				# so to correctly reference the characteristic categories
				# within samples they must be prompted to update
				#
				# if all categories of child objects were present in the
				# parents this should not be necessary
				purrr::walk(self$samples, ~.x$update_characteristics())
			} else {
				if (any(lgl)) {
					stop(
						"These samples are present:\n",
						paste0(sample_ids[lgl], collapse = ", "),
						"\n but these are missing!: \n",
						paste0(sample_ids[!lgl], collapse = ", ")
					)
				}
				warning(
					"None of the samples with the supplied IDs were found!\n",
					"Recording IDs in place referencing appropriate sample objects!"
				)
				self$samples <- sample_ids
				names(self$samples) <- sample_ids
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
			lst[["@id"]] <- self$`@id`
			lst[["measurementType"]] <- self$measurement_type$to_list()
			lst[["technologyType"]] <- self$technology_type$to_list()
			lst[["technologyPlatform"]] <- self$technology_platform
			lst[["filename"]] <- self$filename
			lst[["dataFiles"]] <- self$data_files %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["materials"]][["otherMaterials"]] <- self$other_materials %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			if(is.character(self$samples)) {
				lst[["materials"]][["samples"]] <- self$samples %>%
					purrr::map(~list(`@id` = .x)) %>%
					purrr::set_names(NULL)
			} else {
				lst[["materials"]][["samples"]] <- self$samples %>%
					#purrr::map(~.x$to_list()) %>%
					purrr::map(~list(`@id` = .x$`@id`)) %>%
					purrr::set_names(NULL)
			}
			if(is.null(self$unit_references)) {
				lst[["unitCategories"]] <- list()
			} else {
				lst[["unitCategories"]] <- self$unit_references$to_list(
					source = self$`@id`
				)
			}
			# purrr::map(self$unit_references, ~{
			# 	c(list(`@id` = .x$`@id`), .x$to_list())
			# })
			lst[["characteristicCategories"]] <-
				self$characteristic_categories$to_list(source = self$`@id`)
			lst[["processSequence"]] <- self$process_sequence %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["comments"]] <- self$comments
			# lst[["graph"]] <- self$graph
			return(lst)
		},

		header_table = function() {
			dplyr::bind_cols(
				self$measurement_type$to_table() %>% purrr::set_names(
					"Study Assay Measurement Type",
					"Study Assay Measurement Type Term Accession Number",
					"Study Assay Measurement Type Term Source REF"
				),
				self$technology_type$to_table() %>% purrr::set_names(
					"Study Assay Technology Type",
					"Study Assay Technology Type Term Accession Number",
					"Study Assay Technology Type Term Source REF"
				),
				tibble::tibble_row(
					"Study Assay Technology Platform" =
						self$technology_platform, # isn't in spec but should be ontology
					"Study Assay File Name" = self$filename
				)
			)
		},

		# path_finder
		# matrix with a row for each unique path
		# for each 'group' collect
		# first inputs
		# only outputs which are also inputs to the next step
		# - unless it is the last step in which case keep all outputs
		# 'fill'

		# expand.grid(list("a","b"),list("c","d","e")) %>% t() %>% unlist() %>% igraph::make_directed_graph()
		# %>% igraph::all_simple_paths(from = "a") %>% purrr::keep(\(x)(all(length(x) > 2)))

		to_table = function() {

		# node types:
		# sample / assay name
		# ontology annotation
		#	- X[type], term source ref, term accession number
		#	- characteristic, factor value
		# 	- (optionally) unit special case Unit col is the term, Param Value[type]
		# process (protocol ref)
		# factor value

		# source, protocol, sample

		# sample, protocols, extracts, assay/process, data, process/transforms, derived data

		# not add generated from to data files based on process paths
		# out side of this function
			# browser()

			# process_paths[[1]] %>% purrr::discard(\(x){length(x) > 1}) %>% purrr::map(~.x %>% purrr::map(~combine_io_tables(combined_proc_io[[.x]])))

			# proteome testing
			# pool 3 has both iTRAQ 117 and 114 in the table
			# but this is inconsistent with the json source
			# factor value limiting nutrient is rolled over to
			# pool values with current approach should only show up
			# with specific value and not in pools.

			# factor value rate and non-pooled samples are together.

			private$process_paths() %>%
				purrr::map(
					~.x %>%
						purrr::discard(~all(is.null(.x))) %>%
						purrr::map(
							~to_table_by_process_io_type(
								private$combined_process_io()[.x]
							)
						) %>%
						purrr::list_cbind(name_repair = "minimal")
				) %>% do.call("rbind", .)
				# %>% purrr::list_rbind()

			# unique_proc_io_tables <- process_paths %>%
			# 	unlist() %>%
			# 	unique() %>%
			# 	purrr::set_names() %>%
			# 	purrr::map(~combine_io_tables(combined_proc_io[[.x]]))
			# 	# purrr::map(~combined_proc_io[[.x]]$to_table())

			# self$samples %>%
			# 	purrr::set_names(NULL) %>%
			# 	purrr::map_chr(~.x$name) %>%
			# 	tibble::tibble("Sample Name" = .)

			# characteristics <- self$samples %>%
			# 	purrr::map(~{
			# 		.x$characteristics %>%
			# 			purrr::map(~.x$to_table()) %>%
			# 			purrr::list_cbind()
			# 	}) %>%
			# 	purrr::list_rbind()
#
			# dplyr::bind_cols(characteristics, processess)
			# # dplyr::left_join(characteristics, processess, by = "Sample Name")
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

			if (is.null(self$unit_references)) {
				self$unit_references <- UnitReferences$new(
					ontology_source_references =
						self$ontology_source_references#,
					#unit_references = self$unit_references
				)
			}

			if (!checkmate::test_list(lst[["unitCategories"]], len = 0)) {
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
							self$ontology_source_references
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
			# see below for assignment generated_from values for data files
			# must occur after processes have been processed
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

			lst[["materials"]][["samples"]] %>%
				purrr::map_chr(~.x$`@id`) %>%
				self$set_samples_by_id()

			self$process_sequence <- lst[["processSequence"]] %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				purrr::map(~{
						ps <- Process$new(
							protocols = self$protocols,
							sources = self$sources,
							samples = self$samples,
							materials = self$other_materials,
							data_files = self$data_files,
							ontology_source_references = self$recursive,
							unit_references = self$unit_references
						)
						ps$from_list(.x, recursive = recursive, json = json)
						ps
					}
				)

			purrr::walk2(lst[["processSequence"]], self$process_sequence, ~{
				.y$add_process_order(.x, names(self$process_sequence))
			})

			# automatically populate data file generated_from by inferring the
			# samples files from process order
			# !!! dynamic update handling ?
			datafile_lgl <- private$process_io_path_types() == "DataFile"
			sample_lgl <- private$process_io_path_types() == "Sample"
			private$process_paths() %>%
				purrr::walk(~{
					x <- .x %>% unlist()
					# print(.x)
					self$data_files[x[datafile_lgl]] %>% purrr::walk(~{
						.x$add_generated_from(self$samples[x[sample_lgl]])
					})
				})

			self$comments <- lst[["comments"]]
			# self$graph <- lst[["graph"]]
		},
		#' @details
		#' Pretty prints [Assay] objects
		print = function() { # ⚖️
			cli::cli_h1(cli::col_blue("Assay ⚖️️"))

			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("Measurement Type", self$measurement_type$term)
			green_bold_name_plain_content("Technology Type", self$technology_type$term)
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
		},
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
		get_process_order = function() { private$process_order() },
		get_process_paths = function() { private$process_paths() },
		get_process_io_paths = function() { private$process_io_paths() }
	),
	private = list(
		process_order = function() {
			process_order(self$process_sequence)
		},
		process_paths = function() {
			process_paths(self$process_sequence)
		},
		process_io_paths = function() {
			process_io_paths(self$process_sequence)
		},
		combined_process_io = function() {
			c(
				self$process_sequence, self$samples, self$other_materials,
				self$data_files
			)
		},
		process_io_path_types = function() {
			private$process_paths()[[1]] %>%
			unlist() %>%
			purrr::map_chr(~get_r6_class(private$combined_process_io()[[.x]]))
		}
	)
)
