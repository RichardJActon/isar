# Units of Measurement

library(rdflib)
library(dplyr)

get_om_terms <- function(file, filetype, url, version) {

	# om
	om_rdf_file_url <- file
	# "https://raw.githubusercontent.com/HajoRijgersberg/OM/master/om-2.0.rdf"
	OM <- rdflib::rdf_parse(om_rdf_file_url)

	omq_prefix <- paste(
		sep = "\n",
		"PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>",
		"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
		"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
		"SELECT *",
		"{",
		"  ?unit rdf:type om:PrefixedUnit .",
		"  OPTIONAL { ?unit rdfs:comment ?comment }",
		"  OPTIONAL { ?unit om:symbol ?symbol }",
		"  ?unit rdfs:label ?label",
		"  FILTER(LANG(?label) = 'en' )",
		"}"
	)

	omq_unit <- paste(
		sep = "\n",
		"PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>",
		"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
		"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
		"SELECT *",
		"{",
		"  ?unit rdf:type om:Unit .",
		"  OPTIONAL { ?unit rdfs:comment ?comment }",
		"  OPTIONAL { ?unit om:symbol ?symbol }",
		"  ?unit rdfs:label ?label",
		"  FILTER(LANG(?label) = 'en' )",
		"}"
	)

	# cat(omq)
	omqr_unit <- rdflib::rdf_query(OM, omq_unit)
	omqr_prefix <- rdflib::rdf_query(OM, omq_prefix)
	omqr <- dplyr::bind_rows(omqr_unit,omqr_prefix)

	# omqr |> dplyr::filter(grepl("byte", label))

	omqr <- omqr |> dplyr::mutate(symbol = dplyr::if_else(
		is.na(symbol), label, symbol
	))

	omqrl <- omqr$symbol
	names(omqrl) <- omqr$label
	return(omqrl)
}
# terms tab?
# uri, name, short_name, description

# ontology_source object for OM

# get_om_terms

OM <- ontology_source$new(
	name = "Ontology of units of Measure (OM)",
	# ontology_id = "OM"
	file = "https://raw.githubusercontent.com/HajoRijgersberg/OM/master/om-2.0.rdf",
	# versioned_file = "https://raw.githubusercontent.com/HajoRijgersberg/OM/416b7b0bee253f3e4f0d05d4281fe76c8761ddae/om-2.0.rdf"
	file_type = "rdf",
	url = "http://www.ontology-of-units-of-measure.org/resource/om-2",
	# ontology_iri = "http://www.ontology-of-units-of-measure.org/resource/om-2"
	version = "2.0.38", # OLS version
	# commit_hash = "416b7b0bee253f3e4f0d05d4281fe76c8761ddae"
	description = "The OM ontology provides classes, instances, and properties that represent the different concepts used for defining and using measures and units. It includes, for instance, common units such as the SI units meter and kilogram, but also units from other systems of units such as the mile or nautical mile. For many application areas it includes more specific units and quantities, such as the unit of the Hubble constant: km/s/Mpc, or the quantity vaselife. OM defines the complete set of concepts in the domain as distinguished in the textual standards. As a result the ontology can answer a wider range of competency questions than the existing approaches do. The following application areas are supported by OM: Geometry; Mechanics; Thermodynamics; Electromagnetism; Fluid mechanics; Chemical physics; Photometry; Radiometry and Radiobiology; Nuclear physics; Astronomy and Astrophysics; Cosmology; Earth science; Meteorology; Material science; Microbiology; Economics; Information technology; Typography; Shipping; Food engineering; Post-harvest; technology; Dynamics of texture and taste; Packaging",
	get_terms_list = get_om_terms
	#terms_list = omqrl
)


OM$terms_list[["metre"]]

exOM <- ontology_annotation$new(
	term = "metre",
	term_source = OM
)

exOM

library(shiny)

ui <- fluidPage(
	exOM$get_ontology_annotation_ui("example")
)

server <- function(input, output, session) {
	exOM$get_ontology_annotation_server("example")
}

shinyApp(ui, server)
