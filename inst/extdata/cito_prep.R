get_cito_terms <- function(file, filetype, url, version) {
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

cito_terms <- get_cito_terms(
	file = "https://sparontologies.github.io/cito/current/cito.json",
	filetype =  "json",
	url = "https://sparontologies.github.io/cito/current/cito.html",
	version = "2.8.1"
)

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

cito_lst <- cito$to_list()
lst_cito <- ontology_source$new()
lst_cito$from_list(cito_lst)

cito$terms_list[["agrees with"]]

agrees_with <- ontology_annotation$new(
	term = "agrees with", term_source = cito
)

aw_lst <- agrees_with$to_list()
#lst_aw <- ontology_annotation$new()$from_list(aw_lst)

lst_aw <- ontology_annotation$new()
lst_aw$from_list(aw_lst)
# lst_aw$term_source$get_terms_list
# identical(agrees_with, lst_aw) # R6 object identity?


disagrees_with <- ontology_annotation$new(
	term = "disagrees with", term_source = cito
)

agree <- study_factor$new(
	name = "citor agrees with citee",
	factor_type = agrees_with
)

agree_lst <- agree$to_list()
lst_agree <- study_factor$new()
lst_agree$from_list(agree_lst)
lst_agree$factor_type
lst_agree$factor_type$term_source

disagree <- study_factor$new(
	name = "citor disagrees with citee",
	factor_type = agrees_with
)

fv_agree <- factor_value$new(factor_name = agree)
fv_disagree <- factor_value$new(factor_name = disagree)

fva_lst <- fv_agree$to_list()
lst_fva <- factor_value$new()
lst_fva$from_list(fva_lst)

# expect error
test_sample <- sample$new(
	name = "test",
	factor_values = fv_agree
)

test_sample <- sample$new(
	name = "test",
	factor_values = list(fv_agree, TRUE)
)

# example
test_sample <- sample$new(
	name = "test",
	factor_values = list(fv_agree)
)

test_sample_lst <- test_sample$to_list()
lst_test_sample <- sample$new()
lst_test_sample$from_list(test_sample_lst)


study_factor$new()


at <- assay$new()
at$set_measurement_type(agrees_with)



# validator functions

term_validator_example_list_cito <- function(
		value = NULL, accession = NULL, unit = NULL
) {
	if (!is.null(value)) {
		if (value %in% names(cito_terms)) {
			if(!is.null(accession)) {
				if (accession == cito_terms[[value]]) {
					message("value pressent, accession matched")
					return(TRUE)
				} else {
					stop("term does not match accession!")
				}
			} else {
				message("value pressent, accession null")
				return(TRUE)
			}
		} else {
			message("value not pressent")
			return(FALSE)
		}
	}
	if (!is.null(accession)) {
		if (accession %in% unlist(cito_terms)) {
			if(!is.null(value)) {
				if (
					value == names(
						unlist(cito_terms)[unlist(cito_terms) %in% accession]
					)
				) {
					message("accession present, value matched")
					return(TRUE)
				} else {
					stop("term does not match accession!")
				}
			} else {
				message("accession present, value null")
				return(TRUE)
			}
		} else {
			message("accession not present")
			return(FALSE)
		}
	}
}

term_validator_example_list_cito("agrees with", "agreesWith")
term_validator_example_list_cito("agrees with")
term_validator_example_list_cito("agrees with2")
term_validator_example_list_cito("agrees with", "disagreesWith")

term_validator_example_meters_float <- function(
		value = NULL, accession = NULL, unit = NULL
) {
	if(!is.numeric(value)){
		return(FALSE)
	}
	if(!is.finite(value)) {
		return(FALSE)
	}
	if(is.numeric(value) && is.null(unit)) {
		return(TRUE)
	}
	if (is.numeric(value) && !is.null(unit)) {
		if (unit == "m") {
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
}

term_validator_example_meters_float(value = 1, unit = "cm")

term_validator_example_meters_float(value = "1", unit = "m")
term_validator_example_meters_float(value = 1, unit = "m")
term_validator_example_meters_float(value = 0.345, unit = "m")
term_validator_example_meters_float(value = Inf, unit = "m")
term_validator_example_meters_float(value = NA_real_, unit = "m")
term_validator_example_meters_float(value = NaN, unit = "m")

term_validator_example_meters_float(value = 1e308, unit = "m") # not Inf
term_validator_example_meters_float(value = 1e309, unit = "m") # Inf

term_validator_example_meters_float(value = 0.345)
term_validator_example_meters_float(value = 0.345)

# ontology parsing for making ontoloogy source objects

library(rdflib)

# om
OM <- rdflib::rdf_parse("https://raw.githubusercontent.com/HajoRijgersberg/OM/master/om-2.0.rdf")

omq_prefix <- paste(
	sep = "\n",
	"PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>",
	"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
	"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
	"SELECT *",
	"{",
	# "  ?unit rdf:type om:Unit .",
	"  ?unit rdf:type om:PrefixedUnit .",
	#"  VALUES ?types { om:Unit om:PrefixedUnit }",
	#"  ?types om:Unit",

	#"  VALUES ?types om:PrefixedUnit",
	#"  ?unit rdf:type ?types",
	"  OPTIONAL { ?unit rdfs:comment ?comment }",
	"  OPTIONAL { ?unit om:symbol ?symbol }",

	# "  OPTIONAL {",
	# "    ?unit rdfs:comment ?comment .",
	# "    ?unit om:symbol ?symbol",
	# #"    ?unit om:prefix ?prefix",
	# "  }",
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

omqr |> dplyr::filter(grepl("byte", label))

# cito
cito_rdf <- rdflib::rdf_parse("https://sparontologies.github.io/cito/current/cito.xml")
cito_rdf

citoq <- paste(
	sep = "\n",
	"PREFIX : <http://www.sparontologies.net/example/>",
	"PREFIX cito: <http://purl.org/spar/cito/>",
	"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
	"SELECT *",
	"{",
	"  ?accession rdfs:label ?label .",
	"  ?accession rdfs:comment ?comment .",
	"}"
)

cat(citoq)
cito_labels <- rdflib::rdf_query(cito_rdf, citoq)
cito_labels

cito_labels %>%
	dplyr::filter(label == "citation")# %>%
# dplyr::slice(2) %>%
# dplyr::pull(comment) %>%
# purrr::walk(cat)

cl <- rdflib::rdf_parse("http://purl.obolibrary.org/obo/cl.owl")
# cl

clq <- paste(
	sep = "\n",
	"PREFIX cl: <http://purl.obolibrary.org/obo/cl#>",
	"PREFIX clp: <http://purl.obolibrary.org/obo/>", #CL_
	"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
	"PREFIX obocl: <http://purl.obolibrary.org/obo/cl.owl>",
	"PREFIX owl: <http://www.w3.org/2002/07/owl#>",
	"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
	"SELECT *",
	"WHERE",
	"{",
	#"  ?x cl: ?y .",
	#"  <http://purl.obolibrary.org/obo/> ?x ?y .",
	"  ?accession rdfs:about 'http://purl.obolibrary.org/obo/CL_0000749' .",
#	"  ?accession rdfs:label ?label .",
	#"  ?x obocl: "
	#"  ?accession rdfs:label ?label ;",
	#"  ?accessopm rdfs:string ?string .",
	# "  ?accession rdfs:comment ?comment .",
	# "  FILTER CONTAINS(?accession, 'CL_') .",#CONTAINS
	"}","LIMIT 1000"
)

cat(clq)
cl_labels <- rdflib::rdf_query(cl, clq)
cl_labels

cl_labels %>%
	dplyr::filter(grepl("http://purl.obolibrary.org/obo/CL_", accession))
