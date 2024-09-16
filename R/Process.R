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
		ontology_source_references = NULL,
		unit_references = NULL,
		protocol_parameters = NULL,
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
			data_files = NULL,
			ontology_source_references = NULL,
			unit_references = NULL,
			protocol_parameters = NULL
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
			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references
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
				stop("All performers must be Person objects")
			}
		},

		#' @details
		#' Set date to a Date object
		#' @param public_release_date a Date Object or ISO8601 formatted data string i.e. YYYY-mm-dd
		set_date = function(date, null.ok = FALSE) {
			self$date <- date_input_handling(date, null.ok = null.ok)
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
			# inputs <- NULL
			# outputs <- NULL
			protocols <- NULL

			# conditional on in/out type ?

			# split comments row bind
			# col_bind files / rest
			# col_bind comments and files/ rest?

			# if (!any(purrr::map_lgl(self$inputs, is.null))) {
			# 	inputs <- self$inputs %>% combine_io_tables()
			# 	#inputs <- self$inputs %>% purrr::map(~.x$to_table())
			# 	# inputs <- self$inputs %>% purrr::map(~.x$to_table()) %>%
			# 	# 	purrr::reduce(function(x, y){dplyr::inner_join(x, y)})
			# } else { warning("process ", self$`@id` ," has NULL outputs!") }
#
			# if(is.null(self$next_process)){
			# 	if (!any(purrr::map_lgl(self$outputs, is.null))) {
			# 		#outputs <- self$outputs %>% purrr::map_dfr(~.x$to_table())
			# 		# outputs <- self$outputs %>% purrr::map(~.x$to_table())
			# 		outputs <- self$outputs %>% combine_io_tables()
			# 	} else {
			# 		warning("process ", self$`@id` ," has NULL outputs!")
			# 	}
			# }

			if (!is.null(self$executes_protocol)) {
				protocols <- tibble::tibble(self$executes_protocol$name) %>%
					purrr::set_names(paste0(
						"Protocol REF[", self$executes_protocol$name, "]"
					))
			} else {
				warning("process ", self$`@id` ," executes a NULL protocol!")
			}
			if(self$executes_protocol$name == "unknown") {
				protocols <- NULL
			}

			parameter_values <- NULL
			if (!checkmate::test_list(
				self$parameter_values, len = 0, null.ok = TRUE
			)) {
				parameter_values <- self$parameter_values %>%
					purrr::map(~.x$to_table()) %>%
					purrr::list_cbind(name_repair = "minimal")
			}

			# io <- dplyr::full_join(inputs, outputs, by = "Extract Name")

			# tab <- dplyr::full_join(inputs, outputs)

			performer <- NULL
			if(!test_empty(
				self$performer, null.ok = TRUE, zero.len.string.ok = TRUE)
			) {
				performer <- tibble::tibble_row(
					Performer = self$performer$get_full_name()
				)
			}
			date <- NULL
			if(!test_empty(
				self$date, null.ok = TRUE, zero.len.string.ok = TRUE)
			) {
				date <- tibble:tibble_row(self$date)
			}


			if (is.null(
				self$executes_protocol$protocol_type$term_source$isa_process_type
			)) {
				process_colname_prefix <- "Process"
			} else {
				process_colname_prefix <- self$executes_protocol$protocol_type$term_source$isa_process_type
			}

			# In specific cases the below are indented to replace Process Name
			# the criteria for when to do this working from isa-json are unclear.
			# - Assay Name ()
			# - Data Transformation Name
			# - Normalization Name
			# one approach could be based off of executed protocol
			# if the protocol executed is of a type that is classified as a
			# transformation or normalisation (somewhere, perhaps user specified?)
			# then the custom column name could be used.
			tab <- dplyr::bind_cols(
				tibble::tibble(self$name) %>%
					purrr::set_names(paste(process_colname_prefix, "Name")),
				protocols, parameter_values,
				# inputs %>% dplyr::select(-`Extract Name`),
				performer, date,
				## inputs,
				# io,
				# outputs %>% dplyr::select(-`Extract Name`),
				## outputs,
				# self$samples %>%
				# 	purrr::map(~.x$to_table()) %>%
				# 	purrr::list_rbind(),
				.name_repair = "minimal"
			) #%>%
			# ensures nrow is 1 not 0 if protocol is unknown
			# dplyr::select(-`Process Name`) #`Extract Name`)

			# tab %>% select(unique(colnames(.)))
			#list(inputs, outputs, protocols)

			# %>%
			# 	dplyr::relocate(
			# 		`Sample Name`, .before = tidyr::matches("Protocol")
			# 	)
			return(tab)
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
				checkmate::test_character(self$outputs)
				# checkmate::test_list(types = "character")
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
				checkmate::test_character(self$inputs)
				# checkmate::test_list(types = "character")
			) {
				lst[["inputs"]] <- self$inputs %>%
					purrr::map(~list("@id" = .x)) %>% purrr::set_names(NULL)
			} else {
				lst[["inputs"]] <- self$inputs %>%
					purrr::map(~list("@id" = .x$`@id`)) %>%
					purrr::set_names(NULL)
			}

			if (checkmate::test_list(
				self$parameter_values, len = 0, null.ok = TRUE
			)) {
				lst[["parameterValues"]] <- list()
			} else{
				lst[["parameterValues"]] <- self$parameter_values %>%
					purrr::map(~.x$to_list()) %>% purrr::set_names(NULL)
			}

			lst[["@id"]] <- self$`@id`

			lst[["date"]] <- self$date
			lst[["comments"]] <- self$comments
			lst[["performer"]] <- self$performer# purrr::map(self$performer, ~.x$to_list),
			# lst[["name"]] <- self$name
			# lst[["executesProtocol"]] <- purrr::map(
			# 	self$executes_protocol, ~.x$to_list()
			# )
			# if(is.null(self$executes_protocol)) {
			if(checkmate::test_list( # handle NULL and empty list cases
				self$executes_protocol, len = 0, null.ok = TRUE)
			) {
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
				if(checkmate::test_list(
					lst[["parameterValues"]], len = 0, null.ok = TRUE
				)) {
					self$parameter_values <- list()
				} else {
					self$parameter_values <- lst[["parameterValues"]] %>%
						purrr::map(~{
							pv <- ParameterValue$new(
								ontology_source_references =
									self$ontology_source_references,
								unit_references = self$unit_references,
								protocol_parameters =
									self$executes_protocol$parameters
							)
							pv$from_list(.x, recursive = recursive, json = json)
							pv
						})# %>%
						#purrr::set_names(purrr::map_chr(., ~.x$`@id`))
				}

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
					# handle when a name is returned from a lookup instead of an object
					# occurs with 'stand alone' assay objects with no input sample objects
					# alternative approach - generate dummy sample objects?
					if(checkmate::test_list(self$inputs, types = "character")) {
						self$inputs <- unlist(self$inputs)
					}
				}
			} else {
				# self$executes_protocol <- lst[["executes_protocol"]] # protocol object
				# self$parameter_values <- lst[["parameter_values"]] # ont anno?
				# self$outputs <- lst[["outputs"]] # sample obj ?
				# self$inputs <- lst[["inputs"]] # source obj
			}
			self$`@id` <- lst[["@id"]]
			self$set_name(lst[["name"]])
			self$set_date(lst[["date"]], null.ok = TRUE)
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
first_processes <- function(x) {
	x %>%
		purrr::map(~{ if(is.null(.x$previous_process)) { .x } }) %>%
		purrr::discard(is.null)
}
# first_processes <- get_fist_processes(processes)

next_processes <- function(previous_processes, processes) {
	processes[previous_processes %>% purrr::map_chr(~.x$next_process)]
}
# next_processes(first_processes, processes) %>% length()

# last has no next step
last_processes <- function(processes) {
	processes %>%
		purrr::map(~{if(is.null(.x$next_process)){.x}}) %>%
		purrr::discard(is.null)
}

#' process_order
#'
#' @param processes list of [Process] objects as in the process_sequence
#' sequence field of an [Assay] object
process_order <- function(processes) {
	process_order <- list()
	i <- 1
	first_processes_ids <- processes %>% first_processes() %>% names()
	last_processes_ids <- processes %>% last_processes() %>% names()
	process_order[[i]] <- first_processes_ids
	if(length(first_processes_ids) == length(last_processes_ids)) {
		if(all(first_processes_ids == last_processes_ids)) {
			return(process_order)
		}
	}
	while (all(!process_order[[i]] %in% last_processes_ids)) {
		i <- i + 1
		process_order[[i]] <- next_processes(
			processes[process_order[[i - 1]]], processes
		) %>% names()
	}
	return(process_order)
}

# process_graph <- function(processes) {
# 	process_order_mat <- process_order(processes) %>% do.call("cbind", .)
# 	n_proc_cols <- ncol(process_order_mat)
# 	edges <- character()
# 	for (i in 1:nrow(process_order_mat)) {
# 		j <- 1
# 		while (j < n_proc_cols) {
# 			j <- j + 1
# 			proc1 <- processes[[process_order_mat[i, j - 1]]]
# 			proc2 <- processes[[process_order_mat[i, j]]]
# 			edges <- c(edges, c(rbind(
# 				names(proc1$inputs),
# 				names(proc1$outputs)[
# 					names(proc1$outputs) %in% names(proc2$inputs)
# 				]
# 			)))
# 			if(j == n_proc_cols) {
# 				edges <- c(
# 					edges, c(rbind(names(proc2$inputs), names(proc2$outputs)))
# 				)
# 			}
# 			# print(paste(i,j))
# 		}
# 	}
# 	igraph::make_directed_graph(edges = edges)
# }

process_io_paths <- function(processes) {
	process_order_mat <- process_order(processes) %>% do.call("cbind", .)
	n_proc_cols <- ncol(process_order_mat)
	n_proc_rows <- nrow(process_order_mat)
	paths <- list()
	for (i in 1:nrow(process_order_mat)) {
		j <- 1
		paths[[i]] <- list()
		while (j <= n_proc_cols) {
			j <- j + 1
			proc1 <- processes[[process_order_mat[i, j - 1]]]
			if(n_proc_cols > 1) {
				proc2 <- processes[[process_order_mat[i, j]]]
				# include only outputs which are inputs to subsequent processes
				# paths[[i]][[j - 1]] <- names(proc1$outputs)[
				# 	names(proc1$outputs) %in% names(proc2$inputs)
				# ]
				paths[[i]][[j - 1]] <- names(proc1$outputs)
			} else {
				proc2 <- proc1
				paths[[i]][[j]] <- names(proc2$outputs)
			}
			# print(
			# 	names(proc1$outputs)[
			# 		names(proc1$outputs) %in% names(proc2$inputs)
			# 	]
			# )
			if((j - 1) == 1) {
				paths[[i]][[j - 1]] <- names(proc1$inputs)
				# print(names(proc1$inputs))
			}
			if(j == n_proc_cols) {
				paths[[i]][[j]] <- names(proc2$outputs)
				j <- j + 1
				# print(names(proc2$outputs))
			}
			# print(paste(i,j))
		}
		# print("---break----")
	}
	paths # %>%
		# purrr::map(
		# 	~.x %>%
		# 		purrr::set_names(paste0("X", seq_along(.))) %>%
		# 		expand.grid()
		# ) %>%
		# do.call("rbind", .)
}


process_paths <- function(processes) {
	process_order_mat <- process_order(processes) %>% do.call("cbind", .)
	n_proc_cols <- ncol(process_order_mat)
	n_proc_rows <- nrow(process_order_mat)
	n <- n_proc_cols * 2
	paths <- list()
	for (i in 1:n_proc_rows) {
		j <- 1
		q <- 1
		paths[[i]] <- list()
		while (q <= (n - 1)) {
			j <- j + 1
			proc1 <- processes[[process_order_mat[i, j - 1]]]
			if(n_proc_cols > 1) {
				proc2 <- processes[[process_order_mat[i, j]]]
				q <- q + 1
				paths[[i]][[q]] <- process_order_mat[i, j - 1]
				q <- q + 1
				# include only outputs which are inputs to subsequent processes
				# paths[[i]][[q]] <- names(proc1$outputs)[
				# 	names(proc1$outputs) %in% names(proc2$inputs)
				# ]
				paths[[i]][[q]] <- names(proc1$outputs)
			} else {
				proc2 <- proc1
				paths[[i]][[q + 1]] <- process_order_mat[i, j - 1]
				paths[[i]][[q + 2]] <- names(proc2$outputs)
				q <- q + 2
			}
			# print(
			# 	names(proc1$outputs)[
			# 		names(proc1$outputs) %in% names(proc2$inputs)
			# 	]
			# )
			if((j - 1) == 1) {
				paths[[i]][[q - 2]] <- names(proc1$inputs)
				# print(names(proc1$inputs))
			}
			if(j == n_proc_cols) {
				paths[[i]][[q + 1]] <- process_order_mat[i, j]
				paths[[i]][[q + 2]] <- names(proc2$outputs)
				q <- q + 1
				# print(names(proc2$outputs))
			}
			# print(paste(i,j))
		}
		# print("---break----")
	}
	paths # %>%
	# purrr::map(
	# 	~.x %>%
	# 		purrr::set_names(paste0("X", seq_along(.))) %>%
	# 		expand.grid()
	# ) %>%
	# do.call("rbind", .)
}

to_table_by_process_io_type <- function(x) {
	switch(
		get_r6_class(x[[1]]),
		"DataFile" = {
			# print("DataFile")
			# purrr::map_dfc(x, ~.x$to_table())

			tabs <- x %>% purrr::set_names(NULL) %>%
				purrr::map(~.x$to_table())

			# handling duplicate comments
			comment_tab <- tabs %>%
				purrr::map(~dplyr::select(
					.x, dplyr::starts_with("Comment["))
				) %>%
				purrr::list_cbind(name_repair = "minimal")

			unique_comments <- comment_tab %>%
				colnames() %>% unique()

			comment_tab <- comment_tab %>%
				dplyr::select(dplyr::all_of(unique_comments))

			not_comment_tab <- tabs %>%
				purrr::map(~dplyr::select(
					.x, -dplyr::starts_with("Comment[")
				)) %>%
				purrr::list_cbind()

			dplyr::bind_cols(
				# not_comment_tab, comment_tab,
				comment_tab, not_comment_tab,
				.name_repair = "minimal"
			)
		},
		"Sample" = {
			# print("Sample")
			# x$to_table()
			x %>%
				purrr::set_names(NULL) %>%
				purrr::map(~.x$to_table()) %>%
				purrr::list_rbind()
				#purrr::list_cbind(name_repair = "minimal")
		},
		"Process" = {
			# print("Process")
			# x$to_table()
			x %>%
				purrr::set_names(NULL) %>%
				purrr::map(~.x$to_table()) %>%
				purrr::list_cbind(name_repair = "minimal")
		},
		"Source" = {
			# source is always already printed in Study$to_table()
			# ~~ so if the inputs are sources return NULL to avoid
			# duplication ~~ handle elsewhere
			# Are there situations where outputs are a source?
			# if so would need handling
			# NULL

			x %>%
				purrr::set_names(NULL) %>%
				purrr::map(~.x$to_table()) %>%
				purrr::list_cbind()
		},
		"Material" = {
			# print("Material")
			x %>%
				purrr::set_names(NULL) %>%
				purrr::map(~.x$to_table()) %>%
				purrr::list_rbind()
		}
	)
}
