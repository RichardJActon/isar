#' R6 object for Process
#' @details
#' Process nodes represent the application of a protocol to some input material
#'  (e.g. a [Source]) to produce some output (e.g.a [Sample]).
#' @field name If relevant, a unique name for the process to disambiguate it from other processes.
#' @field executes_protocol A reference to the [Protocol] that this process executes.
#' @field date A date formatted as an ISO8601 string corresponding to when the process event occurred.
#' @field performer The name of the person or organisation that carried out the process.
#' @field parameter_values A list of [ParameterValue]s relevant to the executing protocol.
#' @field inputs A list of input materials, possibly [Source]s, [Sample]s, [Material]s, [DataFile]s
#' @field outputs A list of output materials, possibly [Sample]s, [Material]s, [DataFile]s
#' @field comments Comments associated with instances of this class.
#' @field @id identifier
#' @field protocols list of available [Protocol]s
#' @field sources list of available [Source]s
#' @field samples list of available [Sample]s
#'
#' @importFrom checkmate check_string test_list test_r6 check_date
#' @importFrom purrr map map_lgl
#' @importFrom cli cli_h1 col_blue cli_h2 cli_ul
#'
#' @export
Process <- R6::R6Class(
	"Process",
	public = list(
		name = character(),
		executes_protocol = NULL,
		date = NULL,
		performer = NULL,
		parameter_values = NULL,
		inputs = NULL,
		outputs = NULL,
		comments = NULL,
		`@id` = NULL,
		protocols = NULL,
		sources = NULL,
		samples = NULL,
		next_process = NULL,
		previous_process = NULL,
		materials = NULL,
		data_files = NULL,
		#' @details
		#' Create a new [Process]
		#' @param name If relevant, a unique name for the process to disambiguate it from other processes.
		#' @param executes_protocol A reference to the Protocol that this process executes.
		#' @param date A date formatted as an ISO8601 string corresponding to when the process event occurred.
		#' @param performer The name of the person or organisation that carried out the process.
		#' @param parameter_values A list of [ParameterValue]s relevant to the executing protocol.
		#' @param inputs A list of input materials, possibly [Source]s, [Sample]s, [Material]s, [DataFile]s
		#' @param outputs A list of output materials, possibly [Sample]s, [Material]s, [DataFile]s
		#' @param comments Comments associated with instances of this class.
		#' @param @id identifier
		#' @param protocols list of available [Protocol]s
		#' @param sources list of available [Source]s
		#' @param samples list of available [Sample]s
		initialize = function(
			name = character(),
			executes_protocol = NULL,
			date = NULL,
			performer = NULL,
			parameter_values = NULL,
			inputs = NULL,
			outputs = NULL,
			comments = NULL,
			`@id` = NULL,
			protocols = NULL,
			sources = NULL,
			samples = NULL,
			next_process = NULL,
			previous_process = NULL,
			materials = NULL,
			data_files = NULL
		) {
			self$name <- name
			self$executes_protocol <- executes_protocol
			self$date <- date
			self$performer <- performer
			self$parameter_values <- parameter_values
			self$inputs <- inputs
			self$outputs <- outputs
			self$comments <- comments
			self$`@id` <- `@id`
			self$protocols <- protocols
			self$sources <- sources
			self$samples <- samples
			self$next_process <- next_process
			self$previous_process <- previous_process
			self$materials <- materials
			self$data_files <- data_files
		},
		#' @details
		#' Check the the name has a non-zero length
		#' @param name of the process
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set the name of the [Process]
		#' @param name of the process
		set_name = function(name) {
			if(is.null(name)) {
				# warning("Process Name Missing!")
				# self$name <- ""# character()
				#
				if(!is.null(self$`@id`)) {
					warning("Process Name Missing! attempting to infer from @id...")
					self$name <- sub("#process/(.*)", "\\1", self$`@id`)
					private$infered_name <- TRUE
				} else {
					warning("Process Name Missing!")
					self$name <- ""# character()
				}
			} else if (self$check_name(name)) {
				self$name <- name
			}
		},
		#' @details
		#' check performer is a list of [Person] objects
		#' @param performer a list of [Person] objects
		check_performer = function(performer) {
			if(
				checkmate::test_list(performer, min.len = 1) &&
				all(
					purrr::map_lgl(performer, ~checkmate::test_r6(.x, "Person"))
				)
			) { return(TRUE) } else {
				stop("All performer must be Person objects")
			}
		},
		#' @details
		#' Check date is a Date object
		#' @param date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		check_date = function(date) {
			if (is.character(date)) {
				date <- as.Date.character(date, tryFormats = c("%Y-%m-%d"))
			} else {
				check <- checkmate::check_date(date)
				error_with_check_message_on_failure(check)
			}
		},
		#' @details
		#' Set date to a Date object
		#' @param date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		set_date = function(date) {
			if(self$check_submission_date(date)) { self$date <- date }
		},
		#' @details
		#' set performer if performer is a list of [Person] objects
		#' @param performer a list of [Person] objects
		set_performer = function(performer) {
			if (self$check_performer(performer)) { self$performer <- performer }
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
		to_table = function() {
			inputs <- NULL
			outputs <- NULL
			protocols <- NULL

			# conditional on in/out type ?
			# ioclass <- x %>% class() %>% `[`(1)
			# switch(ioclass,"DataFile" = {})
			# ?
			# split comments row bind
			# col_bind files / rest
			# col_bind comments and files/ rest?

			#
			getR6class <- function(x) { class(x)[1] }
			combine_io_tables <- function(x){
				switch(
					getR6class(x[[1]]),
					"DataFile" = {
						tabs <- purrr::map(x, ~.x$to_table())
						all_colnames <- tabs %>%
							purrr::map(colnames) %>% unlist()
						dupe_cols <- all_colnames[duplicated(all_colnames)]
						names(dupe_cols) <- NULL
						dupe_cols <- unique(dupe_cols)
						tabs %>%
							purrr::reduce(\(x, y){
								dplyr::full_join(x, y, by = dupe_cols)
							})
					},
					"Sample" = {
						purrr::map_dfr(x, ~.x$to_table())
					},
					"Material" = {
						purrr::map_dfr(x, ~.x$to_table())
					}
				)
			}

			if (!any(purrr::map_lgl(self$inputs, is.null))) {
				inputs <- self$inputs %>% combine_io_tables()
				#inputs <- self$inputs %>% purrr::map(~.x$to_table())
				# inputs <- self$inputs %>% purrr::map(~.x$to_table()) %>%
				# 	purrr::reduce(function(x, y){dplyr::inner_join(x, y)})
			}
			if (!any(purrr::map_lgl(self$outputs, is.null))) {
				#outputs <- self$outputs %>% purrr::map_dfr(~.x$to_table())
				# outputs <- self$outputs %>% purrr::map(~.x$to_table())
				outputs <- self$outputs %>% combine_io_tables()
			}


			if (!is.null(self$executes_protocol)) {
				protocols <- tibble::tibble(self$executes_protocol$name) %>%
					purrr::set_names(paste0(
						"Protocol REF[", self$executes_protocol$name, "]"
					))
			}
			tab <- dplyr::bind_cols(
				inputs, outputs, protocols, .name_repair = "minimal"
			)

			tab %>% select(unique(colnames(.)))
			#list(inputs, outputs, protocols)

			# %>%
			# 	dplyr::relocate(
			# 		`Sample Name`, .before = tidyr::matches("Protocol")
			# 	)
		},
		#' @details
		#' An R list representation of a [Process] object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE){
			lst <- list()
			# handling names when reading without parent objects to supply
			# sample and source objects
			# if (is.character(self$outputs)) {
			# 	lst[["outputs"]] <- self$outputs %>%
			# 		purrr::map(~list("@id" = .x)) %>% purrr::set_names(NULL)
			# } else {
			# 	lst[["outputs"]] <- self$outputs %>%
			# 		purrr::map(~.x$to_list()) %>% purrr::set_names(NULL)
			# }
			# if (is.character(self$inputs)) {
			# 	lst[["inputs"]] <- self$inputs %>%
			# 		purrr::map(~list("@id" = .x)) %>% purrr::set_names(NULL)
			# } else {
			# 	lst[["inputs"]] <- self$inputs %>%
			# 		purrr::map(~.x$to_list()) %>% purrr::set_names(NULL)
			# }
			if (checkmate::test_list(self$outputs, len = 0)) {
				lst[["outputs"]] <- list()
			} else if (
				checkmate::test_list(self$outputs, types = "character")
			) {
				lst[["outputs"]] <- self$outputs %>%
					purrr::map(~list("@id" = .x)) %>% purrr::set_names(NULL)
			} else {
				lst[["outputs"]] <- self$outputs %>%
					purrr::map(~list("@id" = .x$`@id`)) %>%
					purrr::set_names(NULL)
			}

			if (checkmate::test_list(self$inputs, len = 0)) {
				lst[["inputs"]] <- list()
			} else if (
				checkmate::test_list(self$inputs, types = "character")
			) {
				lst[["inputs"]] <- self$inputs %>%
					purrr::map(~list("@id" = .x)) %>% purrr::set_names(NULL)
			} else {
				lst[["inputs"]] <- self$inputs %>%
					purrr::map(~list("@id" = .x$`@id`)) %>%
					purrr::set_names(NULL)
			}

			lst[["parameterValues"]] <- self$parameter_values
			lst[["@id"]] <- self$`@id`

			lst[["date"]] <- self$date
			lst[["comments"]] <- self$comments
			lst[["performer"]] <- self$performer# purrr::map(self$performer, ~.x$to_list),
			# lst[["name"]] <- self$name
			# lst[["executesProtocol"]] <- purrr::map(
			# 	self$executes_protocol, ~.x$to_list()
			# )
			if(is.null(self$executes_protocol)) {
				# When no protocol is listed
				lst[["executesProtocol"]] <- list()
			} else if (is.character(self$executes_protocol)) {
				# When referencing a protocol by its ID
				lst[["executesProtocol"]] <- list(
					"@id" = self$executes_protocol
				)
			} else {
				# When a protocol object is present
				lst[["executesProtocol"]] <- list(
					"@id" = self$executes_protocol$`@id`
					# "@id" = self$executes_protocol$to_list()
				)
			}
			# } else if(is.character(self$executes_protocol)) {
			# 	lst[["executesProtocol"]] <- list("@id" = self$executes_protocol)
			# } else {
			# 	lst[["executesProtocol"]] <- self$executes_protocol$to_list()
			# }

			if(!is.null(self$next_process)) {
				lst[["nextProcess"]][["@id"]] <- self$next_process
			}
			if(!is.null(self$previous_process)) {
				lst[["previousProcess"]][["@id"]] <- self$previous_process
			}
			# only ~~last processes~~ non-empty named processes have name keys for some reason?
			name_tmp <- self$name
			# ? make conditional on some kind of preserve exact input flag?
			if (private$infered_name) { name_tmp <- "" }
			if(!(
				test_empty(name_tmp, mode = "character") || name_tmp == ''
			)) { lst[["name"]] <- name_tmp }
			# if(is.null(self$next_process) && !is.null(self$previous_process)) {
			# 	lst[["name"]] <- self$name
			# }

			return(lst)
		},
		#' @details
		#' Make [Process] object from list
		#' @param lst an Process object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			# browser()
			if(json) {
				if(is.null(self$protocols)) {
					self$executes_protocol <- lst[["executesProtocol"]][["@id"]]
				} else {
					self$executes_protocol <- self$protocols[[ # handle possible missing here?
						lst[["executesProtocol"]][["@id"]]
					]]
				}
				self$parameter_values <- lst[["parameterValues"]]

				inputs_and_outputs <- c(
					self$samples, self$materials, self$data_files, self$sources
				)

				if(checkmate::test_list(lst[["outputs"]], len = 0)) {
					self$outputs <- list()
				} else if(is.null(inputs_and_outputs[lst[["outputs"]][[1]][["@id"]]])) { # better checks?
					self$outputs <- purrr::map_chr(lst[["outputs"]], ~.x$`@id`)
				} else {
					self$outputs <- inputs_and_outputs[
						purrr::map_chr(lst[["outputs"]], ~.x$`@id`)
					]
				}
				# if (is.null(self$sources)) {

				## !!! NULL values appearing in some but not all process sequence input IDs
				# happend after changing this to look up more types of input
				# for(i in 1:25){if(!identical(unlist_sort_by_name(obj$to_list()$processSequence[[i]]), unlist_sort_by_name(ex$processSequence[[i]]))){print(i)}}
				if(checkmate::test_list(lst[["inputs"]], len = 0)) {
					self$inputs <- list()
				} else if (
					is.null(inputs_and_outputs[lst[["inputs"]][[1]][["@id"]]])
				) { # better checks?
					self$inputs <- purrr::map_chr(lst[["inputs"]], ~.x$`@id`)
				} else {
					self$inputs <- inputs_and_outputs[
						purrr::map_chr(lst[["inputs"]], ~.x$`@id`)
					]
				}
			} else {
				# self$executes_protocol <- lst[["executes_protocol"]] # protocol object
				# self$parameter_values <- lst[["parameter_values"]] # ont anno?
				# self$outputs <- lst[["outputs"]] # sample obj ?
				# self$inputs <- lst[["inputs"]] # source obj
			}
			self$`@id` <- lst[["@id"]]
			self$set_name(lst[["name"]])
			self$date <- lst[["date"]]
			if(recursive && !json) {
				self$performer <- purrr::map(lst[["performer"]], ~{
					p <- Person$new()
					p$from_list(.x)
					p
				})
			} else {
				self$performer <- lst[["performer"]]
			}
			self$comments <- lst[["comments"]]
		},

		# #' @details
		# #' Get the uuid of this object
		# #' @return a uuid
		# get_id = function() {
		# 	private$id
		# },
		# #' @details
		# #' set the uuid of this object
		# #' @param id a uuid
		# #' @param suffix a human readable suffix
		# set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
		# 	private$id <- generate_id(id, suffix)
		# },

		add_process_order = function(lst, available_processes) {
			next_id <- lst[["nextProcess"]][["@id"]]
			if(!is.null(next_id)) {
				if(next_id %in% available_processes) {
					self$next_process <- next_id
				}
			}
			previous_id <- lst[["previousProcess"]][["@id"]]
			if (!is.null(previous_id)) {
				if (previous_id %in% available_processes) {
					self$previous_process <- previous_id
				}

			}
		},

		#' @details
		#' Pretty Prints [Process] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Process ⚙️"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("Date", self$date)
			green_bold_name_plain_content("@id", self$`@id`)
			if (checkmate::test_r6(self$performer, "Person")) {
				green_bold_name_plain_content(
					"Performer", self$performer$get_full_name()
				)
			} else {
				green_bold_name_plain_content("Performer", self$performer)
			}
			cli::cli_h2("Inputs")
			if (is.character(self$inputs)) {
				cli::cli_ul(self$inputs)
			} else {
				cli::cli_ul(names(self$inputs))
			}

			cli::cli_h2("Outputs")
			if (is.character(self$outputs)) {
				cli::cli_ul(self$outputs)
			} else {
				cli::cli_ul(names(self$outputs))
			}

			cli::cli_h2("Previous Process")
			cli::cli_ul(self$previous_process)

			cli::cli_h2("Next Process")
			cli::cli_ul(self$next_process)

			if(checkmate::test_r6(self$executes_protocol, "Protocol")){
				green_bold_name_plain_content(
					"Executes Protocol", self$executes_protocol$name
				)
			} else {
				green_bold_name_plain_content(
					"Executes Protocol", self$executes_protocol
				)
			}

			cli::cli_h2("Parameter Values")
			if(checkmate::test_r6(self$parameter_values, "ParameterValue")) {
				self$parameter_values$get_value_in_units()
			} else {
				cli::cli_ul(unlist(self$parameter_values))
			}

			pretty_print_comments(self$comments)
		}
	),
	private = list(
		# id = generate_id()
		infered_name = FALSE
	)
)

# first has no preceding step
get_fist_processes <- function(x) {
	x %>%
		purrr::map(~{ if(is.null(.x$previous_process)) { .x } }) %>%
		purrr::discard(is.null)
}
# first_processes <- get_fist_processes(processes)

get_next_processes <- function(previous_processes, processes) {
	processes[previous_processes %>% purrr::map_chr(~.x$next_process)]
}
# get_next_processes(first_processes, processes) %>% length()

# last has no next step
get_last_processes <- function(processes) {
	processes %>%
		purrr::map(~{if(is.null(.x$next_process)){.x}}) %>%
		purrr::discard(is.null)
}

#' get_process_order
#'
#' @param processes list of [Process] objects as in the process_sequence
#' sequence field of an [Assay] object
get_process_order <- function(processes) {
	process_order <- list()
	i <- 1
	first_processes_ids <- processes %>% get_fist_processes() %>% names()
	last_processes_ids <- processes %>% get_last_processes() %>% names()
	process_order[[i]] <- first_processes_ids
	while (all(!process_order[[i]] %in% last_processes_ids)) {
		i <- i + 1
		process_order[[i]] <- get_next_processes(
			processes[process_order[[i - 1]]], processes
		) %>% names()
	}
	process_order
}
