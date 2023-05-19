# Roles from the CRediT taxonomy
# https://credit.niso.org/
# See also https://jats4r.org/credit-taxonomy

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
		"Writing - original draft" = "https://credit.niso.org/contributor-roles/writing-original-draft/",
		"Writing - review & editing" = "https://credit.niso.org/contributor-roles/writing-review-editing/"
	)
)
usethis::use_data(CRediT, overwrite = TRUE)

CRediT_role_uuids <- c(
	"Conceptualization" = "8b73531f-db56-4914-9502-4cc4d4d8ed73",
	"Data curation" = "f93e0f44-f2a4-4ea1-824a-4e0853b05c9d",
	"Formal analysis" = "95394cbd-4dc8-4735-b589-7e5f9e622b3f",
	"Funding acquisition" = "34ff6d68-132f-4438-a1f4-fba61ccf364a",
	"Investigation" = "2451924d-425e-4778-9f4c-36c848ca70c2",
	"Methodology" = "f21e2be9-4e38-4ab7-8691-d6f72d5d5843",
	"Project administration" = "a693fe76-ea33-49ad-9dcc-5e4f3ac5f938",
	"Resources" = "ebd781f0-bf79-492c-ac21-b31b9c3c990c",
	"Software" = "f89c5233-01b0-4778-93e9-cc7d107aa2c8",
	"Supervision" = "0c8ca7d4-06ad-4527-9cea-a8801fcb8746",
	"Validation" = "4b1bf348-faf2-4fc4-bd66-4cd3a84b9d44",
	"Visualization" = "76b9d56a-e430-4e0a-84c9-59c11be343ae",
	"Writing - original draft" = "43ebbd94-98b4-42f1-866b-c930cef228ca",
	"Writing - review & editing" = "d3aead86-f2a2-47f7-bb99-79de6421164d"
)

CRediT_role_descriptions <- c(
	"Conceptualization" = "Ideas; formulation or evolution of overarching research goals and aims.",
	"Data curation" = "Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use.",
	"Formal analysis" = "Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data.",
	"Funding acquisition" = "Acquisition of the financial support for the project leading to this publication.",
	"Investigation" = "Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection.",
	"Methodology" = "Development or design of methodology; creation of models.",
	"Project administration" = "Management and coordination responsibility for the research activity planning and execution.",
	"Resources" = "Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools.",
	"Software" = "Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components.",
	"Supervision" = "Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team.",
	"Validation" = "Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs.",
	"Visualization" = "Preparation, creation and/or presentation of the published work, specifically visualization/data presentation.",
	"Writing - original draft" = "Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation).",
	"Writing - review & editing" = "Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision – including pre- or post-publication stages."
)

for (role in names(CRediT_role_uuids)) {
	assign(role, {
		x <- OntologyAnnotation$new(
			term = role, term_source = CRediT,
			comments = list("description" = CRediT_role_descriptions[role])
		)
		x$set_id(CRediT_role_uuids[role])
		x
	})
}

# 'enbacktack'
#cat(paste0("`",names(CRediT_role_uuids),"`"), sep = "\n")

usethis::use_data(`Conceptualization`, overwrite = TRUE)
usethis::use_data(`Data curation`, overwrite = TRUE)
usethis::use_data(`Formal analysis`, overwrite = TRUE)
usethis::use_data(`Funding acquisition`, overwrite = TRUE)
usethis::use_data(`Investigation`, overwrite = TRUE)
usethis::use_data(`Methodology`, overwrite = TRUE)
usethis::use_data(`Project administration`, overwrite = TRUE)
usethis::use_data(`Resources`, overwrite = TRUE)
usethis::use_data(`Software`, overwrite = TRUE)
usethis::use_data(`Supervision`, overwrite = TRUE)
usethis::use_data(`Validation`, overwrite = TRUE)
usethis::use_data(`Visualization`, overwrite = TRUE)
usethis::use_data(`Writing - original draft`, overwrite = TRUE)
usethis::use_data(`Writing - review & editing`, overwrite = TRUE)

# generate documentation for dara.R
# glue::glue(
# 	"#' CRediT taxonomy: {role}",
# 	"#' @format [OntologyAnnotation] object",
# 	"#' @source <https://credit.niso.org/contributor-roles/{role_url}/>",
# 	'"{role}"',
# 	.sep = "\n",
# 	role = names(CRediT_role_uuids),
# 	role_url = gsub("[ &-]+","-",tolower(names(CRediT_role_uuids)))
# )
