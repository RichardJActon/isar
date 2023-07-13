isa_tab_section_headings <- c(
	"ONTOLOGY SOURCE REFERENCE",
	"INVESTIGATION",
	"INVESTIGATION PUBLICATIONS",
	"INVESTIGATION CONTACTS",
	"STUDY",
	"STUDY DESIGN DESCRIPTORS",
	"STUDY PUBLICATIONS",
	"STUDY FACTORS",
	"STUDY ASSAYS",
	"STUDY PROTOCOLS",
	"STUDY CONTACTS"
)
names(isa_tab_section_headings) <- isa_tab_section_headings

usethis::use_data(isa_tab_section_headings, overwrite = TRUE)
