
get_isatab_files <- function(path, verbose = FALSE) {
  isatab_files <- path %>%
    fs::dir_ls() %>% 
    tibble::tibble(paths = .) %>%
    dplyr::mutate(
      files = fs::path_file(paths),
      type = sub("^([ias])_.*", "\\1", files)
    )
  
  if (verbose) {
    isatab_file_type_counts <- isatab_files %>%
      dplyr::count(type)
    i_n <- isatab_file_type_counts %>% 
      dplyr::filter(type == "i") %>% dplyr::pull(n)
    s_n <- isatab_file_type_counts %>% 
      dplyr::filter(type == "s") %>% dplyr::pull(n)
    a_n <- isatab_file_type_counts %>% 
      dplyr::filter(type == "a") %>% dplyr::pull(n)
    
    glue::glue(
      .sep = "\n", "{i_n} Invesigation", "{s_n} Studies", "{a_n} Assays"
    ) %>%
      message()
  }
  
  return(isatab_files)
}

section_headings <- c(
  "ONTOLOGY SOURCE REFERENCE",
  "INVESTIGATION",
  "INVESTIGATION PUBLICATIONS",
  "INVESTIGATION CONTACTS",
  "STUDY",
  "STUDY DESIGN DESCRIPTORS",
  "STUDY PUBLICATIONS",
  "STUDY FACTORS",
  "STUDY ASSAYS",
  "STUDY PROTOCOLS",
  "STUDY CONTACTS"
)
names(section_headings) <- section_headings

tokenise_investigation <- function(path, section_headings) {
  # Read the ISA-tab investigation as a tibble
  # Create a new column with the section type 
  i_tab_with_section_type <- path %>% 
    readr::read_tsv(col_names = FALSE, show_col_types = FALSE) %>%
      dplyr::mutate(
        section = dplyr::case_when(
          X1 %in% section_headings ~ section_headings[X1]
        )
      ) %>%
      dplyr::relocate(section, dplyr::everything()) %>%
      tidyr::fill(section) %>%
      dplyr::mutate(section_index = NA_integer_)
  
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
    # If it is on we need to keep track of the count for
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
  i_tab_with_section_type <- i_tab_with_section_type %>%
    tidyr::fill(section_index)
  
  i_tab_with_section_type_nested <- i_tab_with_section_type %>%
    # Remove the section heading lines, these will be redundant with the groups
    dplyr::filter(!X1 %in% section_headings) %>%
    # Group by section heading and index 
    # so that repeated sections are not grouped together
    dplyr::group_by(section, section_index) %>%
    tidyr::nest()
  
  max_count <- section_counts %>% dplyr::pull(n) %>% max()
  
  # # complete table with study indices
  # dplyr::bind_rows(
  #   section_counts %>% dplyr::filter(n == 1),
  #   purrr::map_dfr(seq(2, max_count), ~{
  #     section_counts %>% dplyr::filter(n >= .x) %>% dplyr::mutate(n = .x - 1)
  #   }),
  #   section_counts %>% dplyr::filter(n > 1)
  # ) 
  
}

# invp <- "../data/example-isatab-data/BII-I-1/i_Investigation.txt"
# tokenise_investigation(invp, section_headings) -> tmpi
# tmpi

# tokenise_investigation <- function(path) { # , section_headings
#   tabsplit_lines <- path %>%
#     readLines() %>%
#     strsplit(split = "\t")
#   
#   # problem: names not unique
#   named_lines <- vector(mode = "list", length = length(tabsplit_lines))
#   for (i in seq_along(tabsplit_lines)) {
#     named_lines[[tabsplit_lines[[i]][1]]] <- tabsplit_lines[[i]][-1]
#   }
#   named_lines
# }
# tokenise_investigation(invp)

# invt <- invp %>% 
#   readr::read_tsv(col_names = FALSE) %>%
#   dplyr::mutate(section = dplyr::case_when(
#     X1 %in% section_headings ~ section_headings[X1]
#   )) %>%
#   dplyr::relocate(section, dplyr::everything()) %>%
#   tidyr::fill(section) %>%
#   dplyr::filter(!X1 %in% section_headings)


#tmp[[2]][[1]] %>% t() %>% as.data.frame() %>% tibble::remove_rownames() %>% #

# drop empty columns

# clear_all_na_cols <- function(tbl) {
#   all_na_cols <- tbl %>% as.list() %>% purrr::map_lgl(~all(is.na(.x)))
# 
#   dplyr::select(
#     tbl, dplyr::all_of(names(which(!all_na_cols)))
#   )
# }
# 
# clear_na_cols_and_transpose <- function(tbl) {
#   all_na_cols <- tbl %>% as.list() %>% purrr::map_lgl(~all(is.na(.x)))
#   
#   transposed_section <- dplyr::select(
#       tbl, dplyr::all_of(names(which(!all_na_cols)))
#     ) %>% 
#     t() %>% 
#     as.data.frame() %>%
#     tibble::remove_rownames()
#   
#   colnames(transposed_section) <- transposed_section %>% dplyr::slice(1)
#   transposed_section <- transposed_section %>% dplyr::slice(-1)
#   
#   transposed_section
# }

# clear_na_cols_and_transpose(tmp[[2]][[1]])
# 
# c(
#   "ONTOLOGY SOURCE REFERENCE",
#   "Term Source Name",
#   "Term Source File",
#   "Term Source Version",
#   "Term Source Description",
#   "INVESTIGATION",
#   "Investigation Identifier",
#   "Investigation Title",
#   "Investigation Description",
#   "Investigation Submission Date",
#   "Investigation Public Release Date",
#   "INVESTIGATION PUBLICATIONS",
#   "Investigation PubMed ID",
#   "Investigation Publication DOI",
#   "Investigation Publication Author list",
#   "Investigation Publication Title",
#   "Investigation Publication Status",
#   "Investigation Publication Status Term Accession Number",
#   "Investigation Publication Status Term Source REF",
#   "INVESTIGATION CONTACTS",
#   "Investigation Person Last Name",
#   "Investigation Person First Name",
#   "Investigation Person Mid Initials",
#   "Investigation Person Email",
#   "Investigation Person Phone",
#   "Investigation Person Fax",
#   "Investigation Person Address",
#   "Investigation Person Affiliation",
#   "Investigation Person Roles",
#   "Investigation Person Roles Term Accession Number",
#   "Investigation Person Roles Term Source REF",
#   "STUDY",
#   "Study Identifier",
#   "Study Title",
#   "Study Submission Date",
#   "Study Public Release Date",
#   "Study Description",
#   "Study File Name",
#   "STUDY DESIGN DESCRIPTORS",
#   "Study Design Type",
#   "Study Design Type Term Accession Number",
#   "Study Design Type Term Source REF",
#   "STUDY PUBLICATIONS",
#   "Study PubMed ID",
#   "Study Publication DOI",
#   "Study Publication Author list",
#   "Study Publication Title",
#   "Study Publication Status",
#   "Study Publication Status Term Accession Number",
#   "Study Publication Status Term Source REF",
#   "STUDY FACTORS",
#   "Study Factor Name",
#   "Study Factor Type",
#   "Study Factor Type Term Accession Number",
#   "Study Factor Type Term Source REF",
#   "STUDY ASSAYS",
#   "Study Assay Measurement Type",
#   "Study Assay Measurement Type Term Accession Number",
#   "Study Assay Measurement Type Term Source REF",
#   "Study Assay Technology Type",
#   "Study Assay Technology Type Term Accession Number",
#   "Study Assay Technology Type Term Source REF",
#   "Study Assay Technology Platform",
#   "Study Assay File Name",
#   "STUDY PROTOCOLS",
#   "Study Protocol Name",
#   "Study Protocol Type",
#   "Study Protocol Type Term Accession Number",
#   "Study Protocol Type Term Source REF",
#   "Study Protocol Description",
#   "Study Protocol URI",
#   "Study Protocol Version",
#   "Study Protocol Parameters Name",
#   "Study Protocol Parameters Name Term Accession Number",
#   "Study Protocol Parameters Name Term Source REF",
#   "Study Protocol Components Name",
#   "Study Protocol Components Type",
#   "Study Protocol Components Type Term Accession Number",
#   "Study Protocol Components Type Term Source REF",
#   "STUDY CONTACTS",
#   "Study Person Last Name",
#   "Study Person First Name",
#   "Study Person Mid Initials",
#   "Study Person Email",
#   "Study Person Phone",
#   "Study Person Fax",
#   "Study Person Address",
#   "Study Person Affiliation",
#   "Study Person Roles",
#   "Study Person Roles Term Accession Number",
#   "Study Person Roles Term Source REF"
# )