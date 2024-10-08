#' An R6 Object to represent a study from the ISA model
#'
#' study is the central unit, containing information on the subject under study, its characteristics and any treatments applied.
#'
#' @field filename A field to specify the name of the Study file corresponding the definition of that Study.
#' @field identifier A unique identifier: either a temporary identifier supplied by users or one generated by a repository or other database.
#' @field @id identifier
#' @field title A concise phrase used to encapsulate the purpose and goal of the study.
#' @field description A textual description of the study, with components such as objective or goals.
#' @field submission_date The date on which the study was reported to the repository. This should be ISO8601 formatted.
#' @field public_release_date The date on which the study should be released publicly. This should be ISO8601 formatted.
#' @field design_descriptors Classifications of the study based on the overall experimental design.
#' @field publications A list of Publications associated with the Study.
#' @field contacts A list of People/contacts associated with the Study.
#' @field factors A factor corresponds to an independent variable manipulated by the experimentalist with the intention to affect biological systems in a way that can be measured by an assay.
#' @field protocols Protocols used within the ISA artifact.
#' @field assays An Assay represents a portion of the experimental design.
#' @field sources Sources associated with the study, is equivalent to materials sources.
#' @field samples samples associated with the study, is equivalent to materials samples.
#' @field other_materials Other Materials associated with the study, is equivalent to materials other_material.
#' @field characteristic_categories Annotations of material characteristics used in the study.
#' @field process_sequence A list of Process objects representing the experimental graphs at the study level.
#' @field comments Comments associated with instances of this class.
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
#' @field unit_references A list of units used as a [UnitReferences] object.
#'
#' #field materials Materials associated with the study, contains lists of 'sources', 'samples' and 'other_material'. DEPRECATED.
#' #field graph Graph representation of the study graph.
#'
#' @importFrom R6 R6Class
#' @importFrom cli cli_h1 col_blue col_green cli_h2
#' @importFrom purrr set_names map
#' @importFrom emo ji
#'
#' @export
Study <- R6::R6Class(
	"Study",
	public = list(
		filename = '',
		identifier = character(),
		`@id` = character(),
		title = character(),
		description = character(),
		submission_date = NULL,
		public_release_date = NULL,
		design_descriptors = NULL,
		contacts = NULL,
		publications = NULL,
		factors = NULL,
		protocols = NULL,
		assays = NULL,
		sources = NULL,
		samples = NULL,
		process_sequence = NULL,
		other_materials = NULL,
		characteristic_categories = NULL,
		comments = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		#' @details
		#'
		#' Create a new study object
		#'
		#' @param filename A field to specify the name of the Study file corresponding the definition of that Study.
		#' @param identifier A unique identifier: either a temporary identifier supplied by users or one generated by a repository or other database.
		#' @param @id A unique identifier: either a temporary identifier supplied by users or one generated by a repository or other database.
		#' @param title A concise phrase used to encapsulate the purpose and goal of the study.
		#' @param description A textual description of the study, with components such as objective or goals.
		#' @param submission_date The date on which the study was reported to the repository. This should be ISO8601 formatted.
		#' @param public_release_date The date on which the study should be released publicly. This should be ISO8601 formatted.
		#' @param design_descriptors Classifications of the study based on the overall experimental design.
		#' @param publications A list of Publications associated with the Study.
		#' @param contacts A list of People/contacts associated with the Study.
		#' @param factors A factor corresponds to an independent variable manipulated by the experimentalist with the intention to affect biological systems in a way that can be measured by an assay.
		#' @param protocols Protocols used within the ISA artifact.
		#' @param assays An Assay represents a portion of the experimental design.
		#' @param sources Sources associated with the study, is equivalent to materials sources.
		#' @param samples samples associated with the study, is equivalent to materials [Sample].
		#' @param other_materials Other Materials associated with the study, is equivalent to materials other_material.
		#' @param characteristic_categories Annotations of material characteristics used in the study.
		#' @param process_sequence A list of Process objects representing the experimental graphs at the study level.
		#' @param comments Comments associated with instances of this class.
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
		#' @param unit_references A list of Units used in the annotation of material units in the study.
		initialize = function(
			filename = '',
			identifier = character(),
			`@id` = character(),
			title = character(),
			description = character(),
			submission_date = NULL,
			public_release_date = NULL,
			design_descriptors = NULL,
			contacts = NULL,
			publications = NULL,
			factors = NULL,
			protocols = NULL,
			assays = NULL,
			sources = NULL,
			samples = NULL,
			process_sequence = NULL,
			other_materials = NULL,
			characteristic_categories = NULL,
			comments = NULL,
			ontology_source_references = NULL,
			unit_references = NULL
		) {
			self$filename <- filename
			identifier <- self$identifier
			self$`@id` <- `@id`
			if (checkmate::qtest(title, "S[0]")) {
				self$title <- title
			} else {
				self$set_title(title)
			}
			if (checkmate::qtest(description, "S[0]")) {
				self$description <- description
			} else {
				self$set_description(description)
			}
			if (is.null(submission_date)) {
				self$submission_date <- submission_date
			} else {
				self$set_submission_date(submission_date)
			}
			if (is.null(public_release_date)) {
				self$public_release_date <- public_release_date
			} else {
				self$set_public_release_date(public_release_date)
			}
			if(is.null(contacts)) { self$contacts <- contacts } else {
				self$set_contacts(contacts)
			}
			self$design_descriptors <- design_descriptors
			if(is.null(publications)) {
				self$publications <- publications
			} else {
				self$set_publications(publications)
			}
			self$factors <- factors
			self$protocols <- protocols
			self$assays <- assays
			self$sources <- sources
			self$samples <- samples
			self$process_sequence <- process_sequence
			self$other_materials <- other_materials
			self$characteristic_categories <- characteristic_categories
			self$comments <- comments
			# self$units <- units
			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references
		},
		#' @details
		#' Check if the title of the study is a string
		#' @param title The title of the study
		check_title = function(title) {
			check <- checkmate::check_string(title, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the title of the study if valid
		#' @param title The title of the study
		set_title = function(title) {
			if (self$check_title(title)) { self$title <- title }
		},
		#' @details
		#' Check if the description of the study is a string
		#' @param description The description of the study
		check_description = function(description) {
			check <- checkmate::check_string(description, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the description of the study if valid
		#' @param description The description of the study
		set_description = function(description) {
			if (self$check_description(description)) {
				self$description <- description
			}
		},
		#' @details
		#' Set public_release_date to a Date object
		#' @param public_release_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		#' @param null.ok accept NULL dates (boolean) default = FALSE
		set_public_release_date = function(
			public_release_date, null.ok = FALSE
		) {
			self$public_release_date <- date_input_handling(
				public_release_date, null.ok = null.ok
			)
		},

		#' @details
		#' Set submission_date to a Date object
		#' @param submission_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		#' @param null.ok accept NULL dates (boolean) default = FALSE
		set_submission_date = function(submission_date, null.ok = FALSE) {
			self$submission_date <- date_input_handling(
				submission_date , null.ok = null.ok
			)
		},
		#' @details
		#' check contacts is a list of [Person] objects
		#' @param contacts a list of [Person] objects
		check_contacts = function(contacts) {
			if(
				checkmate::test_list(contacts, min.len = 1) &&
				all(
					purrr::map_lgl(contacts, ~checkmate::test_r6(.x, "Person"))
				)
			) { return(TRUE) } else {
				stop("All contacts must be Person objects")
			}
		},
		#' @details
		#' check publications is a list of [Publication] objects
		#' @param publications a list of [Publication] objects
		check_publications = function(publications) {
			if(
				checkmate::test_list(publications, min.len = 1) &&
				all(purrr::map_lgl(
					publications, ~checkmate::test_r6(.x, "Publication")
				))
			) { return(TRUE) } else {
				stop("All publications must be Publication objects")
			}
		},
		#' @details
		#' Check publications is a list of [Publication] objects
		#' @param publications a list of [Publication] objects
		set_publications = function(publications) {
			if (self$check_contacts(publications)) {
				self$publications <- publications
			}
		},
		#' @details
		#' set contacts if contacts is a list of [Person] objects
		#' @param contacts a list of [Person] objects
		set_contacts = function(contacts) {
			if (self$check_contacts(contacts)) { self$contacts <- contacts }
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
		#' get the ids of [Sample]s
		#' @return character vector of sample IDs
		get_sample_ids = function() {
			names(self$samples)
		},
		#' @details
		#' get the ids of [Source]s
		#' @return character vector of sources IDs
		get_source_ids = function() {
			names(self$sources)
		},
		#' @details
		#' get the ids of [Process]es
		#' @return character vector of sources IDs
		get_process_ids = function() {
			names(self$process_sequence)
		},
		#' @details
		#' get the names of [Sample]s
		#' @return character vector of sample names
		get_sample_names = function() {
			self$samples %>% purrr::map_chr(~.x$name)
		},
		#' @details
		#' get the ids of samples listed in the study not used in any assays
		#' associated with the study.
		#' @return a character vector if sample ids
		get_unused_samples = function() {
			all_provided_sample_names <- self$samples %>% names()
			all_assay_sample_names <- self$assays %>%
				purrr::map(~.x$samples %>% names()) %>%
				unlist(use.names = FALSE)
			names_of_unused_samples <- all_provided_sample_names[
				!all_provided_sample_names %in% all_assay_sample_names
			]
			self$samples[names_of_unused_samples]
		},

		#' @details
		#' Checks if there are any samples in the study which are not used in 
		#' any assays. Returns a logical and throws a warning if unused samples
		#' are found
		#' @param quiet if TRUE suppress the warning message. default = FALSE
		#' @return boolean
		check_for_unused_samples = function(quiet = FALSE) {
			unused_samples <- self$get_unused_samples() %>% names()
			if(length(unused_samples) > 0) {
				if (!quiet) {
					warning(
						"There are ", length(unused_samples),
						" samples in this Study not used in",
						" any of the assays associated with it!\n",
						"You can use the `get_unused_samples` method on this",
						" object to get a list of the unused samples."
					)
				}
				return(TRUE)
			} else {
				message("No unused samples found")
				return(FALSE)
			}
		},
		#' @details
		#' get the names of [Source]s
		#' @return character vector of sources names
		get_source_names = function() {
			self$sources %>% purrr::map_chr(~.x$name)
		},
		#' @details
		#' get the names of [Process]es
		#' @return character vector of sources names
		get_process_names = function() {
			self$process_sequence %>% purrr::map_chr(~.x$name)
		},

		# #' get_processes_by_inputs
		# #'
		# #' vectors of the processes associated with unique inputs
		# #'
		# #' @return named list of character vectors where the names are input
		# #'  ids and the values are vectors of process names
		# get_processes_by_inputs = function() {
		# 	inputs <- list()
		# 	for (i in seq_along(self$process_sequence)) {
		# 		input_names <- names(self$process_sequence[[i]]$inputs)
		# 		for(j in seq_along(input_names)) {
		# 			inputs[[input_names[[j]]]] <- c(
		# 				inputs[[input_names[[j]]]],
		# 				self$process_sequence[[i]]$`@id`
		# 			)
		# 		}
		# 	}
		# 	inputs
		# },

		#' @details
		#' generate a tabular summary of the study object for inclusion in the 
		#' ISA investigation file.
		#' @param index an integer to indicate which study this is when there
		#' is more than one study in the investigation, default = 1
		header_table = function(index = 1) {
			#list(
			dplyr::bind_rows(
				tibble::tibble(
					section = "STUDY", index = index,
					data = list(dplyr::bind_rows(
						tibble::tribble(
							~rowname, ~value,
							"Study Identifier", self$identifier,
							"Study Title", self$title,
							"Study Description", self$description
						),
						comment_to_table(self$comments),
						tibble::tribble(
							~rowname, ~value,
							"Study Submission Date",
								as.character(self$submission_date),
							"Study Public Release Date",
								as.character(self$public_release_date),
							"Study File Name", ifelse(
								is.null(self$filename), NA_character_,
								self$filename
							)
						)
					))
				),

				tibble::tibble(
					section = "STUDY DESIGN DESCRIPTORS", index = index,
					data = self$design_descriptors %>%
							purrr::map(
								~.x$to_table() %>%
									dplyr::relocate(term, accession, source)
							) %>%
							purrr::list_rbind() %>%
							t() %>%
							as.data.frame() %>%
							tibble::rownames_to_column() %>%
							tibble::as_tibble() %>%
							dplyr::mutate(rowname = c(
								"Study Design Type",
								"Study Design Type Term Accession Number",
								"Study Design Type Term Source REF"
							)) %>%
							list()
				),

				purrr::map(self$publications, ~{
					tibble::tibble_row(
						section = "STUDY PUBLICATIONS",
						index = index, data = list(.x$to_table(
							prefix = "Study"
						))
					)
				}) %>% purrr::list_rbind(),

				# factors
				tibble::tibble_row(
					section = "STUDY FACTORS", index = index,
					data = list(self$factors$header_table())
				),
				# assays
				tibble::tibble_row(
					section = "STUDY ASSAYS", index = index,
					data = self$assays %>%
						purrr::map(~.x$header_table()) %>%
						purrr::list_rbind() %>%
						t() %>%
						as.data.frame() %>%
						tibble::rownames_to_column() %>%
						tibble::as_tibble() %>%
						list()
				),
				# protocols
				tibble::tibble_row(
					section = "STUDY PROTOCOLS", index = index,
					data = self$protocols %>%
						#purrr::discard(~.x$name == "unknown") %>%
						purrr::discard(
							~.x$protocol_type$term == "Unspecified Term"
						) %>%
						purrr::map(~.x$header_table()) %>%
						purrr::list_rbind() %>%
						t() %>%
						as.data.frame() %>%
						tibble::rownames_to_column() %>%
						tibble::as_tibble() %>%
						list()
				),
				# contacts
				tibble::tibble_row(
					section = "STUDY CONTACTS", index = index,
					data = self$contacts %>%
						purrr::map(~.x$header_table(prefix = "Study")) %>%
						purrr::list_rbind() %>%
						t() %>%
						as.data.frame() %>%
						tibble::rownames_to_column() %>%
						tibble::as_tibble() %>%
						list()
				)
			)
		},

		#' @details
		#' generate a tabular representation of the Study object
		#' @return a Tibble 
		to_table = function() {
			# processes_by_input <- self$get_processes_by_inputs()
			# self$sources %>%
			# 	purrr::map_dfr(~{
			# 		dplyr::bind_cols(
			# 			.x$to_table(),
			# 			self$process_sequence[
			# 				processes_by_input[[.x$`@id`]]
			# 			] %>% purrr::map(~.x$to_table())#,
			# 			#.name_repair = "minimal"
			# 		)
			# 	})

			private$process_paths() %>%
				purrr::map(
					~.x %>%
						purrr::map(
							~to_table_by_process_io_type(
								private$combined_process_io()[.x]
							)
						) %>%
						 purrr::list_cbind(name_repair = "minimal")
				) %>%
				do.call("rbind", .) %>%
				dplyr::select(-`Process Name`)
		},

		#' @details
		#' serialise the tabular representation of the study to an isa-tab 
		#' study file
		#' @param path the path/filename to which to write the output
		#' @param overwrite should any existing files at path be overwritten? 
		#' (boolean) Default: FALSE 
		cat_table = function(path = stdout(), overwrite = FALSE) {
			if (is.character(path)) {
				if(fs::file_exists(path)) {
					if (overwrite) { fs::file_delete(path) } else {
						stop(
							path,
							" already exists!",
							" set `overwrite = TRUE` to replace it."
						)
					}

				}
			}
			tbl <- self$to_table()

			colnames(tbl) <- colnames(tbl) %>% sub(
				"((?:Term Source REF)|(?:Term Accession Number)|(?:Unit)|(?:Protocol REF))\\[.*\\]",
				"\\1",.
			)

			tbl %>% readr::write_tsv(
				file = path, na = "", append = TRUE, progress = FALSE,
				quote = "all", col_names = TRUE
			)
		},

		#' @details
		#' An R list representation of a [Study] object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			lst <- list()
			# lst[["id"]] <- private$id
			lst[["submissionDate"]] <- self$submission_date
			if(checkmate::test_list(
				self$process_sequence, len = 0, null.ok = TRUE
			)) {
				lst[["processSequence"]] <- self$process_sequence
			} else {
				lst[["processSequence"]] <- self$process_sequence %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			}
			lst[["people"]] <- self$contacts %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["comments"]] <- self$comments
			lst[["description"]] <- self$description
			lst[["unitCategories"]] <- self$unit_references$to_list(
				source = self$`@id`
			)
			lst[["studyDesignDescriptors"]] <- purrr::map(
				self$design_descriptors, ~.x$to_list()
			)
			lst[["publicReleaseDate"]] <- self$public_release_date
			lst[["characteristicCategories"]] <-
				self$characteristic_categories$to_list(origin = self$`@id`)
			lst[["assays"]] <- self$assays %>% purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["filename"]] <- self$filename
			lst[["factors"]] <- self$factors$to_list()
			lst[["publications"]] <- purrr::map(
				self$publications, ~.x$to_list()
			)
			lst[["@id"]] <- self$`@id`
			if (checkmate::test_list(self$other_materials, len = 0)) {
				lst[["materials"]][["otherMaterials"]] <- self$other_materials
			} else {
				lst[["materials"]][["otherMaterials"]] <-
					self$other_material$to_list()
			}
			lst[["materials"]][["samples"]] <- self$samples %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["materials"]][["sources"]] <- self$sources %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["identifier"]] <- self$identifier
			lst[["title"]] <- self$title
			lst[["protocols"]] <- self$protocols %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			# lst[["units"]] <- self$units$to_list()
			return(lst)
		},
		#' @details
		#' Make [Material] object from list
		#' @param lst an Material object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				# private$id <- lst[["id"]]
				self$identifier <- lst[["identifier"]]
				self$`@id` <- lst[["@id"]]
				self$filename <- lst[["filename"]]
				self$title <- lst[["title"]]
				self$description <- lst[["description"]]

				# self$submission_date <- lst[["submissionDate"]]
				self$set_submission_date(
					lst[["submissionDate"]], null.ok = TRUE
				)
				#self$public_release_date <- lst[["publicReleaseDate"]]
				self$set_public_release_date(
					lst[["publicReleaseDate"]], null.ok = TRUE
				)

				self$contacts <- lst[["people"]] %>%
					purrr::map(~{
						p <- Person$new(
							ontology_source_references =
								self$ontology_source_references
						)
						p$from_list(.x, json = json)
						p
					}) %>%
					purrr::set_names(purrr::map_chr(., ~.x$`@id`))
				self$design_descriptors <- purrr::map(
					lst[["studyDesignDescriptors"]], ~{
						sdd <- OntologyAnnotation$new(
							ontology_source_references =
								self$ontology_source_references
						)
						sdd$from_list(.x, recursive = recursive, json = json)
						sdd
					}
				)
				self$unit_references <- UnitReferences$new(
					ontology_source_references = self$ontology_source_references
				)
				if (!checkmate::test_list(lst[["unitCategories"]], len = 0)) {
					self$unit_references$from_list(
						lst[["unitCategories"]], source = self$`@id`
					)
				}
				self$publications <- purrr::map(lst[["publications"]], ~{
					p <- Publication$new(
						ontology_source_references =
							self$ontology_source_references
					)
					p$from_list(.x, recursive = recursive, json = json)
					p
				})

				# super$references_from_list(
				# 	lst[["factors"]], "StudyFactor", explicitly_provided = TRUE
				# )
				#
				# self$factors <- super$get_references(
				# 	"StudyFactor"#, explicit_only = TRUE
				# )

				self$factors <- StudyFactorReferences$new(
					ontology_source_references =
						self$ontology_source_references,
					unit_references = self$unit_references
				)
				self$factors$from_list(
					lst[["factors"]], origin = self$`@id`# explicitly_provided = TRUE
				)

				# self$factors <- lst[["factors"]] %>%
				# 	purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				# 	purrr::map(~{
				# 		sf <- StudyFactor$new()
				# 		sf$from_list(.x, recursive = recursive, json = json)
				# 		sf
				# 	})
				if(is.null(self$characteristic_categories)) {
					self$characteristic_categories <-
						CharacteristicCategoryReferences$new(
							ontology_source_references =
								self$ontology_source_references#,
							#unit_references = self$unit_references
						)
				}
				self$characteristic_categories$from_list(
					lst[["characteristicCategories"]], origin = self$`@id`,
					add = TRUE
				)

				self$protocols <-
					lst[["protocols"]] %>%
					purrr::set_names(purrr::map_chr(., ~.x[["@id"]])) %>%
					purrr::map(~{
						pc <- Protocol$new(
							origin = self$`@id`,
							ontology_source_references =
								self$ontology_source_references
						)
						pc$from_list(.x, recursive = recursive, json = json)
						pc
					})
				# self$assays <- lst[["assays"]]
				if (
					!checkmate::test_list(
						lst[["materials"]][["otherMaterials"]], len = 0
					)
				) {
					self$other_materials <- Material$new(
						characteristic_categories =
							self$characteristic_categories,
						ontology_source_references =
							self$ontology_source_references,
						unit_references = self$unit_references
					)
					self$other_materials$from_list(
						lst[["materials"]][["otherMaterials"]]
					)
				} else {
					self$other_materials <-
						lst[["materials"]][["otherMaterials"]]
				}


				self$sources <- lst[["materials"]][["sources"]] %>%
					purrr::set_names(purrr::map_chr(., ~.x[["@id"]])) %>%
					purrr::map(~{
						src <- Source$new(
							category_references =
								self$characteristic_categories,
							unit_references = self$unit_references
						)
						src$from_list(.x, recursive = recursive, json = json)
						src
					})

				#super$add_materials(self$sources)

				self$samples <- lst[["materials"]][["samples"]] %>%
					purrr::set_names(purrr::map_chr(., ~.x[["@id"]])) %>%
					purrr::map(~{
						smpl <- Sample$new(
							study_factor_references = self$factors,
							ontology_source_references =
								self$ontology_source_references,
							category_references =
								self$characteristic_categories,
							sources = self$sources,
							unit_references = self$unit_references
						)
						smpl$from_list(.x, recursive = recursive, json = json)
						smpl
					})

				self$process_sequence <-
					lst[["processSequence"]] %>%
					purrr::set_names(purrr::map_chr(., ~.x[["@id"]])) %>%
					purrr::map(~{
						ps <- Process$new(
							protocols = self$protocols,
							sources = self$sources,
							samples = self$samples,
							ontology_source_references = self$recursive,
							unit_references = self$unit_references
						)
						ps$from_list(.x, recursive = recursive, json = json) # recursive!
						ps
					}
				)#[order(process_sequence_order)]

				# self$other_material <- lst[["otherMaterial"]]
				# self$characteristic_categories <- lst[["characteristicCategories"]]
				# self$units <- purrr::map(lst[["unitCategories"]],~{
				# 	u <- OntologyAnnotation$new(
				# 		ontology_source_references =
				# 			self$ontology_source_references
				# 	)
				# 	u$from_list(.x, recursive = recursive, json = json)
				# 	u
				# })
				self$assays <- lst[["assays"]] %>%
					purrr::map(~{
						a <- Assay$new(
							samples = self$samples,
							ontology_source_references =
								self$ontology_source_references,
							characteristic_categories =
								self$characteristic_categories,
							protocols = self$protocols,
							unit_references = self$unit_references
						)
						a$from_list(.x, recursive = recursive, json = json)
						a
					}) %>%
					purrr::set_names(purrr::map_chr(., ~.x$`@id`))

				self$comments <- lst[["comments"]]
			} else {
				# private$id <- lst[["id"]]
				self$filename <- lst[["filename"]]
				self$title <- lst[["title"]]
				self$description <- lst[["description"]]
				self$submission_date <- lst[["submission_date"]]
				self$public_release_date <- lst[["public_release_date"]]
				self$contacts <- lst[["contacts"]]
				self$design_descriptors <- lst[["design_descriptors"]]
				if (recursive) {
					self$publications <- purrr::map(lst[["publications"]], ~{
						p <- Publication$new()
						p$from_list(.x, recursive = FALSE, json = FALSE)
						p
					})
				}
				self$factors <- lst[["factors"]]
				self$protocols <- lst[["protocols"]]
				self$assays <- lst[["assays"]]
				self$sources <- lst[["sources"]]
				self$samples <- lst[["samples"]]
				self$process_sequence <- lst[["process_sequence"]]
				self$other_materials <- lst[["other_materials"]]

				self$characteristic_categories <- lst[[
					"characteristic_categories"
				]]

				self$comments <- lst[["comments"]]
				# self$units <- lst[["units"]]
			}
		},

		#' @details
		#' Pretty prints [Study] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Study ", emo::ji("search")))
			green_bold_name_plain_content("Title", self$title)
			# green_bold_name_plain_content("ID", private$id)
			green_bold_name_plain_content(
				"Submission Date", self$submission_date
			)
			green_bold_name_plain_content(
				"Public Release Date", self$public_release_date
			)

			cli::cli_h2(cli::col_green("Description"))
			cli::cli_text(self$description)

			cli::cli_h2(cli::col_green(
				"Contacts (",length(self$contacts),") ", emo::ji("user")
			))
			cli::cli_ul(purrr::map_chr(self$contacts, ~{
				paste0(.x$get_full_name(), cli::col_grey(" (", .x$`@id`, ")"))
			}))

			cli::cli_h2(cli::col_green(
				"Publications (", length(self$publications), ") ",
				emo::ji("book")
			))
			purrr::walk(
				self$publications, ~cli::cli_text(
					"    ", cli::style_bold("Title: "), .x$title
				)
			)

			cli::cli_h2(cli::col_green(
				"Factors (", length(self$factors$get_study_factor_names()),")"
			))
			cli::cli_ul(paste0(
				self$factors$get_study_factor_names(), cli::col_grey(" (",
				self$factors$get_study_factor_ids(), ")")
			))

			cli::cli_h2(cli::col_green("Design Descriptors"))
			cli::cli_ul(purrr::map_chr(self$design_descriptors, ~.x$term))

			cli::cli_h2(cli::col_green("Units"))
# 			cli::cli_ul(purrr::map_chr(self$units, ~.x$term))
			cli::cli_ul(paste0(
				self$unit_references$get_unit_types(source = self$`@id`),
				cli::col_grey(
					" (",
					self$unit_references$get_unit_ids(source = self$`@id`),
					")"
				)
			))
			cli::cli_h2(cli::col_green(
				"Protocols (", length(self$protocols), ") ",
				emo::ji("clipboard")
			))
			cli::cli_ul(paste0(
				purrr::map_chr(self$protocols, ~.x$name),
				cli::col_grey(" (", names(self$protocols), ")")
			))

			cli::cli_h2(cli::col_green(
				"Processes (", length(self$process_sequence) ,") ",
				emo::ji("gear")
			))
			cli::cli_ol(paste0(
				self$get_process_names(),
				cli::col_grey(" (", self$get_process_ids(), ")")
			))

			cli::cli_h2(cli::col_green("Sources (", length(self$sources), ")"))
			cli::cli_ol(paste0(
				self$get_source_names(),
				cli::col_grey(" (", self$get_source_ids(), ")")
			))

			cli::cli_h2(cli::col_green("Samples (", length(self$samples), ")"))
			cli::cli_ol(paste0(
				# show unused samples in red?
				self$get_sample_names(),
				cli::col_grey(" (", self$get_sample_ids(), ")")
			))

			pretty_print_comments(self$comments)
		},

		#' @details
		#' get the order of the processes in process sequence 
		#' @return list of vectors of process ids
		get_process_order = function() { private$process_order() },

		#' @details
		#' get the order of the processes in process sequence and their inputs
		#' and outputs 
		#' @return list of vectors of ids of processes and and their inputs / 
		#' outputs
		get_process_paths = function() { private$process_paths() },

		#' @details
		#' get the inputs and outputs of the processes in process sequence in 
		#' order
		#' @return list of vectors of process input / output ids
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
				self$sources
			)
		},
		process_io_path_types = function() {
			private$process_paths()[[1]] %>%
				unlist() %>%
				purrr::map_chr(~get_r6_class(private$combined_process_io()[[.x]]))
		}
	)
)

