# setup
# BII_I_1_jsonlite <- jsonlite::read_json("../data/ISAdatasets/json/BII-I-1/BII-I-1.json")
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


#ct <- Characteristic$new()
#ct$from_list(BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]$characteristics[[1]])
#ct
