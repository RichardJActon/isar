get_cito_terms <- function(file, filetype, url) {
	cito_json <- rjson::fromJSON(file = file)

	all_ids <- purrr::map_chr(cito_json, ~.x[["@id"]])

	items_with_cito_terms <- cito_json[grepl("http://purl.org/spar/cito/", all_ids)]
	# items_with_cito_terms

	terms <- sub(
		"http://purl.org/spar/cito/",
		"",
		all_ids[grepl("http://purl.org/spar/cito/", all_ids)]
	)

	comments <- purrr::map_chr(
		items_with_cito_terms,
		~.x$`http://www.w3.org/2000/01/rdf-schema#comment`[[1]]$`@value`
	)

	values <- purrr::map_chr(
		items_with_cito_terms,
		~.x$`http://www.w3.org/2000/01/rdf-schema#label`[[1]]$`@value`
	)

	terms_list <- as.list(terms)
	names(terms_list) <- values

	return(terms_list)
}

cito <- ontology_source$new(
	name = "CiTO",
	url = "https://sparontologies.github.io/cito/current/cito.html",
	file = "https://sparontologies.github.io/cito/current/cito.json",
	file_type = "json",
	#file = "https://raw.githubusercontent.com/SPAROntologies/cito/master/docs/current/cito.json",
	version = "2.8.1",
	description = "The Citation Typing Ontology (CiTO) is an ontology that enables characterization of the nature or type of citations, both factually and rhetorically.",
	get_terms_list = get_cito_terms
)
