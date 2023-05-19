#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom isar Investigation
#' @importFrom gargoyle init
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
	#investigation <- isar::Investigation$new()

	CRediT <- OntologySource$new(
		name = "CRediT",
		url = "https://credit.niso.org/",
		description = "CRediT (Contributor Roles Taxonomy) is a high-level taxonomy, including 14 roles, that can be used to represent the roles typically played by contributors to research outputs. The roles describe each contributor’s specific contribution to the scholarly output.",
		terms_list = list(
			"Conceptualization" = "https://credit.niso.org/contributor-roles/conceptualization/",
			"Data curation" = "https://credit.niso.org/contributor-roles/data-curation/",
			"Formal analysis" = "https://credit.niso.org/contributor-roles/formal-analysis/",
			"Funding acquisition" = "https://credit.niso.org/contributor-roles/funding-acquisition/",
			"Investigation" = "https://credit.niso.org/contributor-roles/investigation/",
			"Methodology" = "https://credit.niso.org/contributor-roles/methodology/",
			"Project administration" = "https://credit.niso.org/contributor-roles/project-administration/",
			"Resources" = "https://credit.niso.org/contributor-roles/resources/",
			"Software" = "https://credit.niso.org/contributor-roles/software/",
			"Supervision" = "https://credit.niso.org/contributor-roles/supervision/",
			"Validation" = "https://credit.niso.org/contributor-roles/validation/",
			"Visualization" = "https://credit.niso.org/contributor-roles/visualization/",
			"Writing – original draft" = "https://credit.niso.org/contributor-roles/writing-original-draft/",
			"Writing – review & editing" = "https://credit.niso.org/contributor-roles/writing-review-editing/"
		)
	)

	Conceptualization <- OntologyAnnotation$new(term = "Conceptualization", term_source = CRediT)
	Conceptualization$set_id("8b73531f-db56-4914-9502-4cc4d4d8ed73")
	Validation <- OntologyAnnotation$new(term = "Validation", term_source = CRediT)
	Validation$set_id("4b1bf348-faf2-4fc4-bd66-4cd3a84b9d44")

	Jane_Doe <- Person$new(
		last_name = "Doe", first_name = "Jane", mid_initials = "C.",
		phone = "01234567890", fax = "12345",
		address = "The Library, Unseen University",
		roles = list(Conceptualization),
		email = "Jane.Doe@email.com",
		affiliation = "Unseen University",
		orcid = "0000-8888-8888-8888",
		comments = list(
			"namespace collision" = "good thing Jane has an ORCID"
		)
	)
	John_Doe <- Person$new(
		last_name = "Doe", first_name = "John", mid_initials = "D.",
		phone = "01234567890", fax = "67890",
		address = "The Library, Unseen University",
		roles = list(Validation),
		email = "John.Doe@email.com",
		affiliation = "Visible University",
		orcid = "0000-1111-1111-1111",
		comments = list(
			"namespace collision" = "good thing John has an ORCID"
		)
	)

	Paper8 <- Publication$new(
		title = "The impact of higher dimensional L spaces on thalmic flux",
		pubmed_id = 8L, doi = "10.8888/88", status = "pre-print",
		comments = list(
			"Location of the librarian?" =
				"we lost him in the stacks and we need to get his comments on this before be publish"
		),
		author_list = list(Jane_Doe, John_Doe)
	)

	QEI <- Assay$new(
		measurement_type = "spacetime curvature",
		technology_type = "quantum entanglement interferometry",
		technology_platform = "Widget 9000, CMOT Dibbler Ltd.",
		filename = "a_qei",
	)

	BookDensity <- Study$new(
		title = "The impact of book density on spacetime curvature",
		description = "Studying Lorem Ipsum",
		submission_date = "2023-05-10",
		public_release_date = "2023-05-19",
		contacts = list(Jane_Doe, John_Doe),
		publications = list(Paper8),
		filename = "s_example_study_1-book_density",
		design_descriptors = ,
		assays = ,
		factors = ,
		protocols = ,
		sources = ,
		samples = ,
		process_sequence = ,
		other_material = ,
		characteristic_categories = ,
		comments = list("Help!" = "Eldrich horrors are eating the students! Again."),
		units =
	)

	investigation <- isar::Investigation$new(
		title = "placeholder Investigation",
		description = "Investigating Lorem Ipsum",
		submission_date = "2023-03-31",
		public_release_date = "2023-03-31",
		studies = list(
			BookDensity,
			Study$new(
				title = "Test Study 2", description = "Studying Lorem Ipsum again"
			)
		),
		contacts = list(Jane_Doe, John_Doe),
		publications = list(Paper8),
		comments = list(
			"comment 1" = "good comment", "comment 2" = "bad comment"
		)
	)
	gargoyle::init("update_investigation")

	mod_Investigation_sidebar_server("Investigation_1", investigation = investigation)
	mod_Investigation_body_server("Investigation_1", investigation = investigation)

}
