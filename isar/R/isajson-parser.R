# setup
# BII_I_1_jsonlite <- jsonlite::read_json("../data/ISAdatasets/json/BII-I-1/BII-I-1.json")
# BII_S_3_jsonlite <- jsonlite::read_json("../data/ISAdatasets/json/BII-S-3/BII-S-3.json")
# BII_I_1_tab <- readr::read_tsv("../data/ISAdatasets/tab/BII-I-1/s_BII-S-1.txt")
# BII_S_3_tab <- readr::read_tsv("../data/ISAdatasets/tab/BII-S-3/s_BII-S-3.txt")
# library(devtools)
# load_all()

# Inventory
# inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = FALSE, json = TRUE)
# inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)

# Person ~~complete~~
# p1 <- Person$new()
# p1$from_list(BII_I_1_jsonlite$people[[1]], json = TRUE)

# Publications ~~complete~~
# BII_I_1_jsonlite$publications[[1]]
# pub1 <- Publication$new()
# pub1$from_list(BII_I_1_jsonlite$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[1]]$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[2]]$publications[[1]], recursive = TRUE, json = TRUE)

# Studies
# BII_I_1_jsonlite$studies[[1]]
# s1 <- Study$new()
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = FALSE, json = TRUE)
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = TRUE, json = TRUE)
# s1

# Factors ~~complete~~
# BII_I_1_jsonlite$studies[[1]]$factors[[1]]
# sf1 <- StudyFactor$new()
# sf1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]],json = TRUE,recursive = FALSE)
# sf1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]],json = TRUE,recursive = TRUE)
# sf1


# ontologySourceReferences
# BII_I_1_jsonlite$ontologySourceReferences[[1]]
# os1 <- OntologySource$new()
# os1$from_list(BII_I_1_jsonlite$ontologySourceReferences[[1]], json = TRUE)

# BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType
# oa1 <- OntologyAnnotation$new()
# oa1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType, json = TRUE)


# Datafile
# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFilesi[[1]]
# df <- DataFile$new()
# df$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFiles[[1]])

# Measurement type
# mt <- OntologyAnnotation$new()
# mt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$measurementType, recursive = TRUE, json = TRUE)


# tt <- OntologyAnnotation$new()
# tt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$technologyType, recursive = TRUE, json = TRUE)


# Characteristics
# BII_I_1_jsonlite$studies[[1]]$characteristicCategories[[1]]#$characteristicType
# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$characteristicCategories[[1]]
# BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]$characteristics[[1]]#$value
# BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$characteristics[[1]]


# ct1 <- Characteristic$new()
# ct1$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$characteristicCategories[[1]], json = TRUE)
# ct1
# ct1$value
# ct2 <- Characteristic$new()
# ct2$from_list(BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]$characteristics[[1]], json = TRUE)
# ct2
# ct2$value

# ctoa1 <- OntologyAnnotation$new()
# ctoa1$from_list() BII_I_1_jsonlite$studies[[1]]$characteristicCategories[[1]]$characteristicType

# protocols
# pc <- Protocol$new()
# pc$from_list(BII_I_1_jsonlite$studies[[1]]$protocols[[2]], recursive = TRUE, json = TRUE)
# pc$from_list(BII_I_1_jsonlite$studies[[1]]$protocols[[6]], recursive = TRUE, json = TRUE)
# pc

# process
# pr <- Process$new()
# pr$from_list(BII_I_1_jsonlite$studies[[1]]$processSequence[[1]], recursive = FALSE, json = TRUE)
# pr

# uoa <- OntologyAnnotation$new()
# uoa$from_list(BII_I_1_jsonlite$studies[[1]]$unitCategories[[1]], recursive = TRUE, json = TRUE)
# uoa

# Sources
# src <- Source$new()
# src$from_list(BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]], recursive = TRUE, json = TRUE)
# src

# Samples
# samp <- Sample$new()
# samp$from_list(BII_I_1_jsonlite$studies[[1]]$materials$samples)
# samp

# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$technologyType
# BII_I_1_jsonlite$studies[[1]]$unitCategories[[1]]

# BII_S_3_jsonlite$studies[[1]]$processSequence[[1]]$executesProtocol$`@id`
# BII_S_3_jsonlite$studies[[1]]$processSequence[[1]]$`@id`


# Factor value seems parsable as a characteristic? - what's the value of a dedicated object?
# fvac <- Characteristic$new()
# fvac$from_list(
# 	BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[1]],
# 	recursive = TRUE, json = TRUE
# )
# fvac

# sf <- StudyFactor$new()
# sf$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]], recursive = TRUE, json = TRUE)
# sf

# fv <- FactorValue$new()
# fv$from_list(BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[1]], recursive = TRUE, json = TRUE)
# fv

#' get_process_sequence_order_from_json
#'
#' @param process_sequence
#'
#' @return an integer vector
#' @export
#'
get_process_sequence_order_from_json <- function(process_sequence) {
	purrr::map_int(process_sequence, ~{
		protocol <- sub(
			"#protocol/", "", .x$executesProtocol$`@id`, fixed = TRUE
		)
		process <- sub("#process/", "", .x$`@id`, fixed = TRUE)
		as.integer(sub(protocol, "", process, fixed = TRUE))
	})
}
# get_process_sequence_order_from_json(
# 	BII_S_3_jsonlite$studies[[1]]$processSequence
# )
# get_process_sequence_order_from_json(
# 	BII_I_1_jsonlite$studies[[1]]$processSequence
# )
