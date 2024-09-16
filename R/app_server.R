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
		pubmed_id = 8L, doi = "10.8888/88",
		status = "pre-print", # Ontology Annotation!
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

	Sapience <- OntologySource$new(
		"Sapience",
		terms_list = list("Sapient" = "TRUE", "Not Sapient" = "FALSE")
	)

	BooksPCM <- OntologySource$new(
		"Books per cubic meter", terms_list = list("1" = 1, "2" = 2)
	)


	SapientPairwood <- Source$new(
		name = "Sapient Pairwood",
		comments = list(
			"Ethics?" =
				"If this wood is sapient should we be using it in this experiment, has anyone run this past the IRB?"
		),
		characteristics = list(
			Characteristic$new(
				category = OntologyAnnotation$new(term = "Sapient", term_source = Sapience),
				value = 1, unit = Unit$new("serving spoon") # allow composite units
			)
		)
	)

	BookDensity <- StudyFactor$new(
		"Book Density",
		factor_type = OntologyAnnotation$new("1", term_source = BooksPCM)
			#BooksPCM
	) # allow units

	Shelf1 <- Sample$new(
		name = "Shelf 1",
		factor_values = FactorValue$new(
			factor_name = "Low Book Density",
			value = "1",
			unit = "books / m^3"
		) ,
		characteristics = ,
		derives_from = SapientPairwood
	)

	#Protocol$new()


	BookDensitySTC <- Study$new(
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
		comments = list("Help!" = "Eldrich horrors are eating the students! Again!"),
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
