# Get ISA-tab files ----

#' check_isa_tab_dir
#'
#' Checks if a directory exists and contains the right files for an ISA-tab
#' description.
#'
#' Follows the pattern of the `check_*` functions from the {checkmate}
#' package.
#'
#' @param path the path to an ISA-tab directory
#'
#' @return a string or logical
#' @export
#'
#' @importFrom fs dir_exists dir_ls path_file path_ext
#' @importFrom dplyr `%>%`
# #' @examples
check_isa_tab_dir <- function(path) {
	if(!fs::dir_exists(path)) { return(paste0(
		"Directory:'", path, "' does not exist!"
	))}
	isatab_paths <- path %>% fs::dir_ls()
	if(length(isatab_paths) < 1) {return(paste0(
		"No files found in directory: '", path, "'!"
	))}
	isatab_files <- isatab_paths %>% fs::path_file()
	# get only the files with i/s/a_ prefixes
	isa_tab_prefix_files <- isatab_files[grepl("^([ias])_.*", isatab_files)]
	# get the file extentions of the ISA files
	isa_tab_prefix_files_extensions <- isa_tab_prefix_files %>% fs::path_ext()
	if(!all(isa_tab_prefix_files_extensions == "txt")) { return(paste0(
		"ISA-tab files must be prefixed i/s/a_",
		" and have the extension .txt!"
	))}
	# There should only be one Investigation file in a given ISA-tab directory
	inv_files_idx <- isa_tab_prefix_files %>% grepl("i_.*\\.txt", .) %>% which()
	n_inv <- inv_files_idx %>% length()
	if(n_inv > 1) { return(paste(
		sep = "\n",
		"More than one Investigation file found!",
		paste0(isa_tab_prefix_files[inv_files_idx], collapse = ", "),
		"Please only use only one investigation file per ISA-tab directory."
	))}

	n_sty <- isa_tab_prefix_files %>%
		grepl("s_.*\\.txt", .) %>%
		which() %>%
		length()

	n_asy <- isa_tab_prefix_files %>%
		grepl("a_.*\\.txt", .) %>%
		which() %>%
		length()

	if(!all(n_inv > 0, n_sty > 0, n_asy > 0)) {
		return(paste(
			sep = "\n",
			"There must be at least 1 investigation, 1 study, and 1 assay.",
			"There are:",
			paste0("\tInvestigations: ", n_inv),
			paste0("\tStudies: ", n_sty),
			paste0("\tAssays: ", n_asy)
		))
	}

	return(TRUE)
}

#' get_isatab_files
#'
#' Generates a table listing the files of an ISA-tab dataset from the directory
#' which contains them.
#'
#' @param path path to the directory containing isatab files
#' @param verbose produce a message listing the number of files of each type
#' found (Default: FALSE)
#'
#' @return A tibble with the columns paths, files, type.
#' Where path is the relative path, file is the filename, and type i,s, or a
#' @export
#'
#' @importFrom fs dir_ls path_file
#' @importFrom dplyr `%>%` mutate count filter pull
#' @importFrom tibble tibble
#' @importFrom glue glue
get_isatab_files <- function(path, verbose = FALSE) {
	# todo check .txt extension
	# files names have only: A-Za-z0-9._!#$%&+,;=@^(){}'[]
	# warn on (){}'[]$."
	isatab_valid <- path %>%
		check_isa_tab_dir() %>%
		error_with_check_message_on_failure()

	if(isTRUE(isatab_valid)) {
		isatab_files <- path %>%
			fs::dir_ls()

		isatab_files_tab <- isatab_files %>%
			tibble::tibble(paths = .) %>%
			dplyr::mutate(
				files = fs::path_file(paths),
				type = sub("^([ias])_.*", "\\1", files),
				type = factor(type, levels = c("i","s","a"), ordered = TRUE)
			) %>%
			dplyr::arrange(type)

		if (verbose) {
			isatab_file_type_counts <- isatab_files_tab %>%
				dplyr::count(type)
			i_n <- isatab_file_type_counts %>%
				dplyr::filter(type == "i") %>% dplyr::pull(n)
			s_n <- isatab_file_type_counts %>%
				dplyr::filter(type == "s") %>% dplyr::pull(n)
			a_n <- isatab_file_type_counts %>%
				dplyr::filter(type == "a") %>% dplyr::pull(n)

			glue::glue(
				.sep = "\n", "{i_n} Investigation", "{s_n} Studies",
				"{a_n} Assays"
			) %>%
				message()
		}

		return(isatab_files_tab)
	}
}

# Investigation Parsing Helper functions ----

#' clear_all_na_cols
#'
#' Removes columns the contents of which are all NA
#'
#' @param tbl a tibble
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' clear_all_na_cols(tibble::tibble(A = c(NA, NA), B = 1:2, C = c(NA, FALSE)))
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr `%>%` select all_of
clear_all_na_cols <- function(tbl) {
	all_na_cols <- tbl %>% as.list() %>% purrr::map_lgl(~all(is.na(.x)))

	dplyr::select(
		tbl, dplyr::all_of(names(which(!all_na_cols)))
	)
}

#' transpose_first_to_title
#'
#' Transposes a tibble and then used the first row of the transposed tibble
#' as the column names of the new tibble
#'
#' @param tbl A Tibble
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' transpose_first_to_title(tibble::tibble(X1 = c("A a", "B B"), X2 = c(1,2)))
#'
#' @importFrom dplyr `%>%` slice
#' @importFrom tibble remove_rownames as_tibble
transpose_first_to_title <- function(tbl) {
	transposed_section <- tbl %>%
		t() %>%
		as.data.frame() %>%
		tibble::remove_rownames()

	colnames(transposed_section) <- transposed_section %>% dplyr::slice(1)
	transposed_section <- transposed_section %>% dplyr::slice(-1)

	transposed_section %>% tibble::as_tibble()
}

#' clear_na_cols_and_transpose
#'
#' convenience wrapper for doing first removal of columns with all NA values
#' and transposition of a tibble where first row becomes the column names.
#' This function also adds the step of removing the first (redundant) word from
#' the sub-section names, this makes it easier to use the same parser for
#' fields which are in both Study and Investigation such as people and contacts
#'
#' @param tbl a tibble
#'
#' @return a tibble
#' @export
#'
#' @examples
#' clear_na_cols_and_transpose(tibble::tibble(A = c(NA, NA), B = 1:2, C = c(NA, FALSE)))
#'
#' @importFrom dplyr `%>%`
clear_na_cols_and_transpose <- function(tbl) {
	tbl %>%
		clear_all_na_cols() %>%
		dplyr::mutate(X1 = sub("\\w+ (.*)","\\1", X1)) %>%
		transpose_first_to_title()
}

# a dataset in the package see: data-raw/isa_tab_section_headings.R
# isa_tab_section_headings <- c(
# 	"ONTOLOGY SOURCE REFERENCE",
# 	"INVESTIGATION",
# 	"INVESTIGATION PUBLICATIONS",
# 	"INVESTIGATION CONTACTS",
# 	"STUDY",
# 	"STUDY DESIGN DESCRIPTORS",
# 	"STUDY PUBLICATIONS",
# 	"STUDY FACTORS",
# 	"STUDY ASSAYS",
# 	"STUDY PROTOCOLS",
# 	"STUDY CONTACTS"
# )
# names(isa_tab_section_headings) <- isa_tab_section_headings

#' read_investigation_and_assign_lines_to_sections
#'
#' @param path path to investigation file
#' @param section_headings named character vector of section headings
#'
#' @return a tibble
#' @export
#'
#' @importFrom dplyr `%>%` mutate case_when relocate everything
#' @importFrom readr read_tsv
#' @importFrom tidyr fill
# #' @examples
read_investigation_and_assign_lines_to_sections <- function(
	path, section_headings = isa_tab_section_headings
) {
	# Read the ISA-tab investigation as a tibble
	# Create a new column with the section type

	#i_tab_with_section_type <-
	path %>%
		readr::read_tsv(
			col_names = FALSE, show_col_types = FALSE, comment = "#"
		) %>%
		dplyr::mutate(
			section = dplyr::case_when(
				X1 %in% section_headings ~ section_headings[X1]
			)
		) %>%
		dplyr::relocate(section, dplyr::everything()) %>%
		tidyr::fill(section) %>%
		dplyr::mutate(section_index = NA_integer_)
}

#' index_repeated_sections
#'
#' If there are multiple copies of a section headings assign them an index
#' number to distinguish them, this should generally only apply to studies.
#'
#' @param i_tab_with_section_type the type of the section
#'
#' @return a tibble
#' @export
#'
#' @importFrom dplyr `%>%` filter select count pull
#' @importFrom tidyr fill
# #' @examples
index_repeated_sections <- function(i_tab_with_section_type) {
	# Index the sections of which there are more than one
	section_counts <- i_tab_with_section_type %>%
		dplyr::filter(section == X1) %>% # just the section type rows
		dplyr::select(section) %>%
		dplyr::count(section) # how many of each type of section header

	# Vector of section types to keep track of the number of sections of each type
	named_section_counts <- integer(length = nrow(section_counts))
	names(named_section_counts) <- section_counts %>%
		dplyr::pull(section)

	# for each row in the table
	for(i in seq_along(i_tab_with_section_type[[1]])) {
		# get the section names
		section_name <- i_tab_with_section_type[[2]][[i]] # i_tab_with_section_type[["X1]]
		# If it is one we need to keep track of the count for
		if(section_name %in% names(named_section_counts)) {
			# add 1 to the count
			named_section_counts[section_name] <-
				named_section_counts[section_name] + 1L
			# Record this count by the section heading in the table
			i_tab_with_section_type[["section_index"]][[i]] <-
				named_section_counts[section_name]
		}
	}
	# Count is only recorded on section heading lines
	# So fill it down to all the other lines without section headings
	i_tab_with_section_type %>% tidyr::fill(section_index)
}

# Investigation Parsing ----

#' parse_investigation
#'
#' @param path the path to an investigation file
#' @param section_headings named character vector of section headings
#'
#' @return a nested tibble
#' @export
#'
#' @importFrom dplyr `%>%` filter group_by
#' @importFrom tidyr nest
#' @importFrom purrr map
# #' @examples
parse_investigation <- function(
	path, section_headings = isa_tab_section_headings
) {
	# Read the ISA-tab investigation as a tibble
	# Create a new column with the section type
	i_tab_with_section_type <- path %>%
		read_investigation_and_assign_lines_to_sections(section_headings) %>%
		index_repeated_sections()

	i_tab_with_section_type_nested <- i_tab_with_section_type %>%
		# Remove the section heading lines, these will be redundant with the groups
		dplyr::filter(!X1 %in% section_headings) %>%
		# Group by section heading and index
		# so that repeated sections are not grouped together
		dplyr::group_by(section, section_index) %>%
		tidyr::nest()

	i_tab_with_section_type_nested_transposed <-
		i_tab_with_section_type_nested %>%
		dplyr::mutate(data = purrr::map(data, clear_na_cols_and_transpose))

	return(i_tab_with_section_type_nested_transposed)
}

# invtmp <- parse_investigation("../data/example-isatab-data/BII-I-1/i_Investigation.txt")

tbl_to_investigation <- function(data) {
	Investigation$new(

		# filename = ,
		# 'Identifier'

		title = data$Title,
		description = data$Description,
		# ! check if date before attempting string conversion !
		submission_date = date_input_handling(data$`Submission Date`),
		public_release_date = date_input_handling(data$`Public Release Date`),
		# ontology_source_references = ,
		# publications = ,
		# contacts = ,
		# studies = ,


		#comments =
	)
}

# tbl_to_investigation(invtmp$data[[2]])

# parse tree to object conversion ----

tbl_to_ontology_source <- function(data) {
	fx <- function(
		`Source Name`,
		`Source File`,
		`Source Version`,
		`Source Description`
	) {
		OntologySource$new(
			name = `Source Name`,
			file = `Source File`,
			version = `Source Version`,
			description = `Source Description`
		)
	}
	purrr::pmap(data %>% tidyr::replace_na(
		replace = list(
			`Source Name` = "",
			`Source File` = "",
			`Source Version` = "",
			`Source Description` = ""
		)), fx)
}

# tmp %>%
# 	dplyr::filter(grepl("PUBLICATIONS", section))

tbl_to_publication <- function(data) {
	fx <- function(
		`PubMed ID`,
		`Publication DOI`,
		`Publication Author list`,
		`Publication Title`,
		`Publication Status`,
		`Publication Status Term Accession Number`,
		`Publication Status Term Source REF`
	) {
		Publication$new(
			pubmed_id = `PubMed ID`,
			doi = `Publication DOI`,
			author_list = `Publication Author list`,
			title = `Publication Title`,
			status = `Publication Status`
		)
	}
	purrr::pmap(
		data %>%
			dplyr::mutate(`PubMed ID` = as.integer(`PubMed ID`)) %>%
			tidyr::replace_na(
				replace = list(
					`Publication DOI` = "",
					`Publication Author list` = "",
					`Publication Title` = "",
					`Publication Status` = "",
					`Publication Status Term Accession Number` = "",
					`Publication Status Term Source REF` = ""
				)
			),
		fx
	)
}
#
# tbl_to_publication(tmp$data[[3]])
#
# tmp$data[[7]]
tbl_to_person <- function(data) {
	fx <- function(
		`Person Last Name`,
		`Person First Name`,
		`Person Mid Initials`,
		`Person Email`,
		`Person Phone`,
		`Person Fax`,
		`Person Address`,
		`Person Affiliation`,
		`Person Roles`,
		`Person Roles Term Accession Number`,
		`Person Roles Term Source REF`
	) {
		Person$new(
			last_name = `Person Last Name`,
			first_name = `Person First Name`,
			mid_initials = `Person Mid Initials`,
			email = `Person Email`,
			phone = `Person Phone`,
			fax = `Person Fax`,
			address = `Person Address`,
			affiliation = `Person Affiliation`,
			# orcid = ,
			roles = `Person Roles`
		)
	}
	purrr::pmap(
		data %>%
			tidyr::replace_na(
			replace = list(
				`Person Last Name` = "",
				`Person First Name` = "",
				`Person Mid Initials` = "",
				`Person Email` = "",
				`Person Phone` = "",
				`Person Fax` = "",
				`Person Address` = "",
				`Person Affiliation` = "",
				`Person Roles` = "",
				`Person Roles Term Accession Number` = "",
				`Person Roles Term Source REF` = ""
			)
		),
		fx
	)
}

# tbl_to_person(tmp$data[[4]])

tbl_to_study <- function(data) {
	fx <- function(
		`Identifier`,
		`Title`,
		`Submission Date`,
		`Public Release Date`,
		`Description`,
		`File Name`
	) {
		Study$new(
			filename = `File Name`,
			title = `Title`,
			description = `Description`,
			submission_date = `Submission Date`,
			public_release_date = `Public Release Date`#,
			# contacts = ,
			# design_descriptors = ,
			# publications = ,
			# factors = ,
			# protocols = ,
			# assays = ,
			# sources = ,
			# samples = ,
			# process_sequence = ,
			# other_material = ,
			# characteristic_categories = ,
			# comments = ,
			# units =
		)
	}
	purrr::pmap(data, fx)
}


# Study & Assay shared parser helper functions ---

parse_characteristic <- function() {

}

parse_parameter_value <- function() {

}

parse_factor_value <- function() {

}

# Study ----

# ? parse_protocol <-

parse_source <- function() {

}


parse_study <- function(path) {
	study_tbl <- readr::read_tsv(path, show_col_types = FALSE, comment = "#")
	study_tbl
}

# Assay ----



parse_assay <- function(path) {
	assay_tbl <- readr::read_tsv(path, show_col_types = FALSE, comment = "#")
	assay_tbl
}


# readr::read_tsv("../data/example-isatab-data/BII-I-1/a_metabolome.txt") -> atmp
# readr::read_tsv("../data/example-isatab-data/BII-I-1/s_BII-S-1.txt") -> stmp
# stmp %>% colnames() -> stmpcn
# stmpcn

# attribute - Characteristics, Factor Type, Comment, Label, Material type, Factor value
# node - sample name, source name
# node assay - assay name, data transformation name
# processing - Protocols
# parameter - Parameter Value


# which(grepl("Characteristics\\[.+\\]", stmpcn))

# split by object

#' split_study_or_assay_by_object
#'
#' Objects in Study and Assay tables are 'generally'* defined by a
#' leading column of the form: "Entity Name"
#'
#' !! problem for reliablly identifying object boundaries in these tables
#' !! is there a more systematic way which does not make the name assumption?
#' !! seems like it would be complicated and by exclusion rather than a positive  ID?
#'
#'
#' @param data a
#'
#' @return a list of tibbles
#' @export
#'
#'
#' @importFrom dplyr `%>%` select all_of
#' @importFrom purrr map map2
split_study_or_assay_by_object <- function(data) {
	colnms <- data %>% colnames()

	num_cols <- data %>% ncol()

	object_col_indices <- colnms %>% grepl(".* Name$",.) %>% which()
	# The next column ending in 'Name' defines the end of the column range of
	# the object hence the range in this index minus one. Thus for this to
	# work for the last object in the table we use the number of columns plus one
	object_col_indices_offset <- c(object_col_indices[-1], num_cols + 1)

	object_column_indices <- purrr::map2(
		object_col_indices, object_col_indices_offset, ~seq(.x, .y - 1)
	)
	#object_column_indices
	objects <- purrr::map(object_column_indices, ~{
		data %>% dplyr::select(dplyr::all_of(.x))
	})

	tibble::tibble(
		name = colnms[object_col_indices] %>% sub(" Name$", "", .),
		data = objects
	)
}

# atmpsplit <- split_study_or_assay_by_object(atmp)
# stmpsplit <- split_study_or_assay_by_object(stmp)

# atmpsplit[[1]] <- dplyr::bind_cols(atmpsplit[[1]], tibble::tibble(`Comment[about something]` = "meh"))
extract_comments_from_object_columns <- function(lst) {
	purrr::map(lst, ~{
		tbl <- .x

		colnms <- tbl %>% colnames()

		comment_indices <- colnms %>%
			grepl("^Comment\\[.*\\]$" ,.) %>%
			which()

		comment_names <- colnms[comment_indices] %>%
			sub("^Comment\\[(.*)\\]$" , "\\1", .)

		purrr::map2(comment_indices, comment_names, ~{
			tbl %>% dplyr::pull(.x) %>% list() %>% purrr::set_names(.y)
		})

	})
}

# extract_comments_from_object_columns(atmpsplit)


remove_comments_from_object_columns <- function(lst) {
	purrr::map(lst, ~{
		comment_indices <- .x %>%
			colnames() %>%
			grepl("^Comment\\[.*\\]$" ,.) %>%
			which()
		if (length(comment_indices) > 0) {
			.x %>% dplyr::select(-dplyr::all_of(comment_indices))
		} else {
			.x
		}
	})
}


# "Comment",

attributes <- list(
	"Characteristics", "Factor Type", "Label", "Material Type", "Factor Value"
)
