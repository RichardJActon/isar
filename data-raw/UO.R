uo <- rdflib::rdf_parse("http://purl.obolibrary.org/obo/uo.owl")

q <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
SELECT ?subject ?object
	WHERE { ?subject rdfs:subClassOf ?object }"

cat(q)
rdflib::rdf_query(uo, q)
#rdf_serialize(uo,format = "jsonld",doc = "data-raw/uo_ld.json")
#rdf_serialize(uo,format = "turtle",doc = "data-raw/uo.turtle")

# uojson <- jsonlite::read_json("data-raw/uo_ld.json")
# purrr::map(head(uojson$`@graph`),~.x$`@type`)# uojson$`@graph`[[1]]
