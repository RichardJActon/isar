# BII_I_1_jsonlite <- jsonlite::read_json("../data/ISAdatasets/json/BII-I-1/BII-I-1.json")
# library(devtools)
# load_all()

# inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = FALSE, json = TRUE)
# inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)

# p1 <- Person$new()
# p1$from_list(BII_I_1_jsonlite$people[[1]], json = TRUE)

# BII_I_1_jsonlite$publications[[1]]
# pub1 <- Publication$new()
# pub1$from_list(BII_I_1_jsonlite$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[1]]$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[2]]$publications[[1]], recursive = TRUE, json = TRUE)

# BII_I_1_jsonlite$studies[[1]]
# s1 <- Study$new()
# s1
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = FALSE, json = TRUE)
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = TRUE, json = TRUE)

# BII_I_1_jsonlite$studies[[1]]$factors[[1]]
# sf1 <- StudyFactor$new()

# BII_I_1_jsonlite$ontologySourceReferences[[1]]
# os1 <- OntologySource$new()
# os1$from_list(BII_I_1_jsonlite$ontologySourceReferences[[1]], json = TRUE)

# BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType
# oa1 <- OntologyAnnotation$new()
# oa1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType, json = TRUE)


# datafile
# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFiles[[1]]
# df <- DataFile$new()
# df$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFiles[[1]])

# measurement type
# mt <- OntologyAnnotation$new()
# mt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$measurementType, recursive = TRUE, json = TRUE)

# tt <- OntologyAnnotation$new()
# tt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$technologyType, recursive = TRUE, json = TRUE)
